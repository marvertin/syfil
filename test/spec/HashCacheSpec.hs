{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
module HashCacheSpec (spec) where

import Control.Exception
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson (encode, object, (.=))
import Numeric (showHex)
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Yaml as Y
import System.Directory
import System.FilePath
import System.IO.Temp
import Test.Hspec
import Syfil.IO.HashCache
import Syfil.IO.SourceScaner
import Syfil.Data.Lodree (flattenFileLodree)
import Util.Lib (ErrList(..))

withCacheTest :: (FilePath -> FilePath -> FilePath -> IORef [String]
                  -> ((HashCache -> IO ()) -> IO ()) -> IO ()) -> IO ()
withCacheTest action = withSystemTempDirectory "syfil-cache" $ \dir -> do
  let path = dir </> cacheFileName "tree"
      legacy = dir </> "1_tree.yaml"
  warnings <- newIORef []
  let warn msg = modifyIORef' warnings (msg:)
      session = withHashCache warn path legacy dir
  action dir path legacy warnings session

spec :: Spec
spec = describe "Resumable source hash cache" $ do
  it "reuses journalled records after interruption and keeps the latest version" $ withCacheTest $ \dir _ _ _ session -> do
    let source = dir </> "file"
    writeFile source "first"
    first <- loadStableFileRee source
    (session $ \cache -> do
      rememberHash cache ["file"] first
      throwIO (userError "interrupted"))
      `catch` (\(_ :: IOException) -> pure ())
    writeFile source "changed contents"
    second <- loadStableFileRee source
    session $ \cache -> do
      M.lookup ["file"] (cachedHashes cache) `shouldBe` Just first
      rememberHash cache ["file"] second
    session $ \cache -> M.lookup ["file"] (cachedHashes cache) `shouldBe` Just second

  it "migrates legacy YAML without rehashing or modifying the old cache" $ withCacheTest $ \dir _ legacy _ session -> do
    writeFile (dir </> "file") "legacy"
    ree <- loadStableFileRee (dir </> "file")
    let values = M.singleton ["file"] ree
    Y.encodeFile legacy values
    original <- B.readFile legacy
    session $ \cache -> cachedHashes cache `shouldBe` values
    B.readFile legacy `shouldReturn` original

  it "removes an incomplete tail before appending the next record" $ withCacheTest $ \dir path _ warnings session -> do
    writeFile (dir </> "file") "data"
    ree <- loadStableFileRee (dir </> "file")
    session $ \cache -> rememberHash cache ["a"] ree
    B.appendFile path "{\"payload\":\"unfinished"
    session $ \cache -> do
      M.member ["a"] (cachedHashes cache) `shouldBe` True
      rememberHash cache ["b"] ree
    session $ \cache -> M.keys (cachedHashes cache) `shouldBe` [["a"],["b"]]
    readIORef warnings `shouldSatisfyIO` (not . null)

  it "ignores a checksum-corrupted record while preserving earlier useful hashes" $ withCacheTest $ \dir path _ _ session -> do
    writeFile (dir </> "file") "data"
    ree <- loadStableFileRee (dir </> "file")
    session $ \cache -> rememberHash cache ["good"] ree
    B.appendFile path "{\"payload\":\"{}\",\"sha256\":\"bad\"}\n"
    session $ \cache -> M.keys (cachedHashes cache) `shouldBe` [["good"]]

  it "recovers a completed replacement after the old file was deleted" $ withCacheTest $ \dir path _ _ session -> do
    writeFile (dir </> "file") "data"
    ree <- loadStableFileRee (dir </> "file")
    session $ \cache -> compactCache cache (M.singleton ["file"] ree)
    renameFile path (path ++ ".new")
    session $ \cache -> M.lookup ["file"] (cachedHashes cache) `shouldBe` Just ree
    doesFileExist (path ++ ".new") `shouldReturn` False

  it "keeps a newer main journal instead of a stale completed replacement" $ withCacheTest $ \dir path _ _ session -> do
    writeFile (dir </> "file") "data"
    ree <- loadStableFileRee (dir </> "file")
    session $ \_ -> pure ()
    session $ \cache -> do
      copyFile path (path ++ ".new")
      rememberHash cache ["newer"] ree
    session $ \cache -> M.member ["newer"] (cachedHashes cache) `shouldBe` True

  it "keeps main cache when the replacement is incomplete" $ withCacheTest $ \dir path _ _ session -> do
    writeFile (dir </> "file") "data"
    ree <- loadStableFileRee (dir </> "file")
    session $ \cache -> rememberHash cache ["good"] ree
    B.writeFile (path ++ ".new") "{unfinished"
    session $ \cache -> M.member ["good"] (cachedHashes cache) `shouldBe` True

  it "rejects simultaneous writers and releases the lock after exceptions" $ withCacheTest $ \_ _ _ _ session -> do
    session (\_ -> session (\_ -> pure ())) `shouldThrow` anyIOException
    session (\_ -> pure ())

  it "survives a cache write failure without failing the caller" $ withCacheTest $ \dir path _ warnings session -> do
    writeFile (dir </> "file") "data"
    ree <- loadStableFileRee (dir </> "file")
    session $ \cache -> do
      removeFile path
      createDirectory path
      rememberHash cache ["file"] ree
      compactCache cache M.empty
      removeDirectory path
    readIORef warnings `shouldSatisfyIO` (not . null)

  it "compacts to only current entries while preserving legacy data" $ withCacheTest $ \dir _ _ _ session -> do
    writeFile (dir </> "file") "data"
    ree <- loadStableFileRee (dir </> "file")
    session $ \cache -> do
      rememberHash cache ["gone"] ree
      rememberHash cache ["current"] ree
      compactCache cache (M.singleton ["current"] ree)
    session $ \cache -> M.keys (cachedHashes cache) `shouldBe` [["current"]]

  it "does not load another source identity's cache" $ withCacheTest $ \dir path legacy warnings session -> do
    writeFile (dir </> "file") "data"
    ree <- loadStableFileRee (dir </> "file")
    session $ \cache -> rememberHash cache ["file"] ree
    Y.encodeFile legacy (M.singleton (["legacy-from-wrong-source"] :: [String]) ree)
    withHashCache (\msg -> modifyIORef' warnings (msg:)) path legacy "other-source" $ \cache ->
      cachedHashes cache `shouldBe` M.empty

  it "rehashes changed files and does not resurrect deleted or ignored paths" $ withCacheTest $ \dir _ _ _ session -> do
    let source = dir </> "source"
        quiet _ _ = pure ()
    createDirectory source
    writeFile (source </> "unchanged") "same"
    writeFile (source </> "changed") "old"
    writeFile (source </> "deleted") "gone"
    session $ \cache -> do
      (Just tree, errors) <- readSourceTreeCached quiet (cachedHashes cache) (rememberHash cache) [] source
      getErrList errors `shouldBe` []
      compactCache cache (M.fromList (flattenFileLodree tree))
    writeFile (source </> "changed") "new larger content"
    removeFile (source </> "deleted")
    writeFile (source </> "ignored") "excluded"
    hashed <- newIORef []
    session $ \cache -> do
      (Just tree, errors) <- readSourceTreeCached quiet (cachedHashes cache)
        (\p r -> modifyIORef' hashed (p:) >> rememberHash cache p r) ["e=ignored"] source
      getErrList errors `shouldBe` []
      M.keys (M.fromList (flattenFileLodree tree)) `shouldBe` [["changed"],["unchanged"]]
      compactCache cache (M.fromList (flattenFileLodree tree))
    readIORef hashed `shouldReturn` [["changed"]]
    session $ \cache -> M.keys (cachedHashes cache) `shouldBe` [["changed"],["unchanged"]]

  it "rejects a source changing during its hash read" $ withSystemTempDirectory "syfil-changing" $ \dir -> do
    let path = dir </> "file"
    writeFile path "before"
    let changingReader file = do
          old <- B.readFile file
          writeFile file "different length after read"
          pure old
    loadStableFileReeWith changingReader path `shouldThrow` anyIOException

  it "round trips Unicode, newline and quotes in file path components" $ withCacheTest $ \dir _ _ _ session -> do
    writeFile (dir </> "file") "content"
    ree <- loadStableFileRee (dir </> "file")
    let path = ["žluťoučký\n\"soubor", "adresář"]
    session $ \cache -> rememberHash cache path ree
    session $ \cache -> M.keys (cachedHashes cache) `shouldBe` [path]

  it "rejects an unknown schema version without trusting its file entries" $ withCacheTest $ \dir path _ warnings session -> do
    writeFile (dir </> "file") "data"
    ree <- loadStableFileRee (dir </> "file")
    session $ \cache -> rememberHash cache ["must-not-trust"] ree
    bytes <- B.readFile path
    let payload = L.toStrict (encode (object ["type" .= ("header" :: String), "version" .= (99 :: Int),
                  "source" .= dir, "algorithm" .= ("sha1" :: String)]))
        hexDigit n = let value = showHex n "" in if length value == 1 then '0':value else value
        checksum = concatMap hexDigit (B.unpack (SHA256.hash payload))
        headLine = L.toStrict (encode (object ["payload" .= C.unpack payload, "sha256" .= checksum]))
    B.writeFile path (headLine <> "\n" <> B.drop 1 (B.dropWhile (/= 10) bytes))
    session $ \cache -> cachedHashes cache `shouldBe` M.empty
    readIORef warnings `shouldSatisfyIO` (not . null)

  it "disables caching if the cache directory cannot be created" $ withSystemTempDirectory "syfil-cache-failure" $ \dir -> do
    let blocker = dir </> "file"
    writeFile blocker "regular file, not directory"
    warnings <- newIORef []
    withHashCache (\msg -> modifyIORef' warnings (msg:)) (blocker </> "cache") (dir </> "legacy") dir $ \cache ->
      cachedHashes cache `shouldBe` M.empty
    readIORef warnings `shouldSatisfyIO` (not . null)

  it "ignores malformed legacy hash encoding without aborting the scan" $ withCacheTest $ \dir _ legacy warnings session -> do
    writeFile (dir </> "file") "data"
    ree <- loadStableFileRee (dir </> "file")
    Y.encodeFile legacy (M.singleton (["file"] :: [String]) ree)
    bytes <- B.readFile legacy
    -- Preserve YAML structure, replace the historical Show/Read hash value.
    let corrupt row = case C.breakSubstring "rhash:" row of
          (indent, rest) | not (B.null rest) -> indent <> "rhash: not-a-haskell-bytestring"
          _ -> row
    let damaged = C.unlines (map corrupt (C.lines bytes))
    damaged `shouldNotBe` bytes
    B.writeFile legacy damaged
    session $ \cache -> cachedHashes cache `shouldBe` M.empty
    readIORef warnings `shouldSatisfyIO` (not . null)

shouldSatisfyIO :: (Show a) => IO a -> (a -> Bool) -> Expectation
shouldSatisfyIO action predicate = action >>= (`shouldSatisfy` predicate)
