{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Disposable, resumable source hash cache. This is deliberately independent
-- of the slice index format. The source directory remains authoritative.
module Syfil.IO.HashCache
  ( HashCache, withHashCache, cachedHashes, rememberHash, compactCache
  , cacheFileName
  ) where

import Control.Exception
import Control.Monad
import qualified Crypto.Hash.SHA256 as SHA256
import Data.Aeson
import Data.Aeson.Types (parseEither, Parser)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as L
import Data.IORef
import Data.List (foldl')
import qualified Data.Map.Strict as M
import Data.Time.Clock
import Data.Time.Clock.POSIX (getPOSIXTime)
import Numeric (showHex, readHex)
import System.Directory
import System.FileLock
import System.FilePath
import System.IO
#ifndef mingw32_HOST_OS
import System.Posix.IO (openFd, closeFd, OpenMode(ReadOnly), defaultFileFlags)
import System.Posix.Unistd (fileSynchronise)
#endif
import qualified Data.Yaml as Y
import Syfil.Data.Ree
import Util.Lib (RevPath)

cacheFileName :: String -> FilePath
cacheFileName name = "2_" ++ name ++ ".hash-cache.jsonl"

data HashCache = HashCache
  { cachedHashes :: CacheHash
  , cachePath :: FilePath
  , cacheIdentity :: String
  , cacheWarn :: String -> IO ()
  , cacheEnabled :: IORef Bool
  , cachePending :: IORef (Int, UTCTime)
  }

hex :: B.ByteString -> String
hex = concatMap (\n -> let s = showHex n "" in if length s == 1 then '0':s else s) . B.unpack

unhex :: String -> Parser B.ByteString
unhex s
  | length s /= 40 = fail "invalid SHA1 length"
  | otherwise = B.pack <$> go s
  where
    go [] = pure []
    go (a:b:rest) = case readHex [a,b] of
      [(n, "")] -> (n :) <$> go rest
      _ -> fail "invalid SHA1 encoding"
    go _ = fail "invalid SHA1"

header :: String -> Value
header identity = object ["type" .= ("header" :: String), "version" .= (2 :: Int),
                           "source" .= identity, "algorithm" .= ("sha1" :: String)]

entry :: RevPath -> Ree -> Value
entry path r = object ["type" .= ("file" :: String), "path" .= path,
                      "size" .= rsize r, "mtime" .= rtime r, "hash" .= hex (rhash r)]

-- A checksum covers the exact serialized payload, not a compiler's Show/Read.
line :: Value -> B.ByteString
line value = let payload = L.toStrict (encode value)
             in L.toStrict (encode (object ["payload" .= C.unpack payload,
                    "sha256" .= hex (SHA256.hash payload)])) <> "\n"

parseLine :: B.ByteString -> Either String Value
parseLine bytes = eitherDecodeStrict' bytes >>= parseEither (withObject "record" $ \o -> do
  payload <- o .: "payload"
  checksum <- o .: "sha256"
  let raw = C.pack payload
  unless (checksum == hex (SHA256.hash raw)) (fail "record checksum mismatch")
  either fail pure (eitherDecodeStrict' raw))

kind :: Value -> Either String String
kind = parseEither (withObject "record" (.: "type"))

parseEntry :: Value -> Either String (RevPath, Ree)
parseEntry = parseEither $ withObject "file" $ \o -> do
  path <- o .: "path"
  unless (not (null path) && all (\p -> not (null p) && p /= "." && p /= ".." && not (any isPathSeparator p)) path)
    (fail "invalid relative path")
  size <- o .: "size"
  when (size < 0) (fail "negative file size")
  time <- o .: "mtime"
  digest <- o .: "hash" >>= unhex
  pure (path, Ree 1 size time digest)

validateHeader :: String -> Value -> Either String ()
validateHeader identity = parseEither $ withObject "header" $ \o -> do
  version <- o .: "version"
  source <- o .: "source"
  algorithm <- o .: "algorithm"
  typ <- o .: "type"
  unless (version == (2 :: Int) && source == identity && algorithm == ("sha1" :: String) && typ == ("header" :: String))
    (fail "unsupported cache format or different source")

-- Completed temporary files must contain exactly a header, valid file records,
-- and a footer covering every preceding byte. Journals may append after footer.
readCache :: String -> Bool -> FilePath -> IO (CacheHash, [String])
readCache identity requireComplete path = do
  bytes <- B.readFile path
  let chunks = C.split '\n' bytes
      terminated = not (B.null bytes) && B.last bytes == 10
      records = init chunks
      incomplete = ["discarded incomplete cache tail" | not terminated]
  case records of
    [] -> ioError (userError "empty cache")
    h:rest -> do
      either (ioError . userError) pure (parseLine h >>= validateHeader identity)
      let step (!values, !errors, !prefix, _complete) raw =
            case parseLine raw >>= (\v -> (,) <$> kind v <*> pure v) of
              Right ("file", value) -> case parseEntry value of
                Right (p, r) -> (M.insert p r values, errors, (raw <> "\n"):prefix, False)
                Left e -> (values, e:errors, (raw <> "\n"):prefix, False)
              Right ("complete", value) ->
                let valid = parseEither (withObject "complete" $ \o -> do
                      count <- o .: "count"
                      digest <- o .: "sha256"
                      unless (count == M.size values && digest == hex (SHA256.hash (B.concat (reverse prefix)))) (fail "invalid completion footer")) value
                in case valid of
                  Right () -> (values, errors, (raw <> "\n"):prefix, True)
                  Left e -> (values, e:errors, (raw <> "\n"):prefix, False)
              Right _ -> (values, "unknown cache record":errors, (raw <> "\n"):prefix, False)
              Left e -> (values, e:errors, (raw <> "\n"):prefix, False)
          (values, errors, _, complete) = foldl' step (M.empty, incomplete, [h <> "\n"], False) rest
      when (requireComplete && (not complete || not (null errors))) (ioError (userError "temporary cache is incomplete or corrupt"))
      pure (values, reverse errors)

syncFile :: FilePath -> IO ()
#ifndef mingw32_HOST_OS
syncFile path = bracket (openFd path ReadOnly defaultFileFlags) closeFd fileSynchronise
#else
-- Windows builds retain process-crash recovery; power-loss durability depends
-- on the filesystem flushing policy. Cache loss only costs a rescan.
syncFile _ = pure ()
#endif

writeSnapshot :: FilePath -> String -> CacheHash -> IO ()
writeSnapshot path identity values = do
  let prefix = B.concat (line (header identity) : [line (entry p r) | (p,r) <- M.toAscList values])
      footer = line (object ["type" .= ("complete" :: String), "count" .= M.size values,
                             "sha256" .= hex (SHA256.hash prefix)])
  withBinaryFile path WriteMode $ \h -> B.hPut h (prefix <> footer) >> hFlush h
  syncFile path
  (loaded, _) <- readCache identity True path
  unless (loaded == values) (ioError (userError "cache read-back differs"))

-- Caller holds the lock. Delete then rename is intentional for filesystems
-- that cannot replace an existing destination. A completed .new is recoverable.
installSnapshot :: FilePath -> IO ()
installSnapshot path = do
  -- Persist the replacement name before risking removal of the old name.
  syncFile (takeDirectory path)
  exists <- doesFileExist path
  when exists (removeFile path)
  syncFile (takeDirectory path)
  renameFile (path ++ ".new") path
  syncFile (takeDirectory path)

recoverCache :: (String -> IO ()) -> FilePath -> FilePath -> String -> IO CacheHash
recoverCache warn path legacy identity = do
  let pending = path ++ ".new"
  pendingExists <- doesFileExist pending
  when pendingExists $ do
    recovered <- try (readCache identity True pending)
    case recovered of
      Right _ -> do
        mainExists <- doesFileExist path
        -- An intact main journal may contain newer appended records than a
        -- leftover snapshot. It is always sufficient to resume from it.
        main <- if mainExists then try (readCache identity False path)
                              else pure (Left (userError "missing main cache"))
        case main of
          Right (_, []) -> removeFile pending
          _ -> do
            when mainExists $ do
              stamp <- show <$> getPOSIXTime
              renameFile path (path ++ ".damaged-" ++ stamp)
            installSnapshot path
      Left (e :: IOException) -> do
        warn ("Ignoring unfinished cache replacement: " ++ show e)
        stamp <- show <$> getPOSIXTime
        renameFile pending (pending ++ ".rejected-" ++ stamp)
  exists <- doesFileExist path
  if exists then do
    result <- try (readCache identity False path)
    case result of
      Right (values, []) -> pure values
      Right (values, errors) -> do
        mapM_ warn errors
        -- Rebuild before appending, so an incomplete tail cannot swallow the
        -- next record. Preserve the original for diagnosis/recovery.
        stamp <- show <$> getPOSIXTime
        writeSnapshot pending identity values
        renameFile path (path ++ ".damaged-" ++ stamp)
        installSnapshot path
        pure values
      Left (e :: IOException) -> do
        warn ("Cannot read source cache: " ++ show e)
        stamp <- show <$> getPOSIXTime
        renameFile path (path ++ ".rejected-" ++ stamp)
        -- A version/identity mismatch must not import a legacy cache that
        -- may belong to the previous source. Start cold in this case.
        writeSnapshot (path ++ ".new") identity M.empty
        installSnapshot path
        pure M.empty
  else migrate
  where
    migrate = do
      old <- doesFileExist legacy
      values <- if old then do
        -- Legacy Ree uses a historical partial Read instance for hash bytes.
        -- Force validation here so malformed legacy input cannot abort backup.
        result <- try $ do
          decoded <- Y.decodeFileEither legacy
          values <- either (ioError . userError . show) pure decoded
          valid <- evaluate (all (\r -> B.length (rhash r) == 20 && rsize r >= 0) (M.elems values))
          unless valid (ioError (userError "invalid legacy hash records"))
          pure values
        case result of
          Right values -> pure values
          Left (e :: SomeException) -> case fromException e :: Maybe AsyncException of
            Just asynchronous -> throwIO asynchronous
            Nothing -> warn ("Ignoring unreadable legacy cache: " ++ show e) >> pure M.empty
        else pure M.empty
      writeSnapshot (path ++ ".new") identity values
      installSnapshot path
      pure values

withHashCache :: (String -> IO ()) -> FilePath -> FilePath -> String -> (HashCache -> IO a) -> IO a
withHashCache warn path legacy identity action = do
#ifdef mingw32_HOST_OS
  warn "Hash cache: Windows build provides process-crash recovery; power-loss flushing is not guaranteed"
#endif
  acquired <- try $ do
    createDirectoryIfMissing True (takeDirectory path)
    tryLockFile (path ++ ".lock") Exclusive
  case acquired of
    Left (e :: IOException) -> do
      warn ("Hash cache unavailable; backup continues without persistent progress: " ++ show e)
      run False
    Right Nothing -> ioError (userError ("Source cache already in use: " ++ path))
    Right (Just held) -> run True `finally` unlockFile held
  where
    run available = do
      enabled <- newIORef available
      now <- getCurrentTime
      pending <- newIORef (0, now)
      values <- if not available then pure M.empty else do
        loaded <- try (recoverCache warn path legacy identity)
        case loaded of
          Right values -> pure values
          Left (e :: IOException) -> writeIORef enabled False >> warn ("Hash cache disabled: " ++ show e) >> pure M.empty
      let cache = HashCache values path identity warn enabled pending
      action cache `finally` cacheIO cache (syncFile path)

cacheIO :: HashCache -> IO () -> IO ()
cacheIO cache work = do
  enabled <- readIORef (cacheEnabled cache)
  when enabled $ work `catch` \(e :: IOException) -> do
    writeIORef (cacheEnabled cache) False
    cacheWarn cache ("Hash cache disabled; backup continues without persistent progress: " ++ show e)

rememberHash :: HashCache -> RevPath -> Ree -> IO ()
rememberHash cache path value = cacheIO cache $ do
  withBinaryFile (cachePath cache) AppendMode $ \h -> B.hPut h (line (entry path value)) >> hFlush h
  now <- getCurrentTime
  (count, lastSync) <- readIORef (cachePending cache)
  if count >= 999 || diffUTCTime now lastSync >= 5 then do
    syncFile (cachePath cache)
    writeIORef (cachePending cache) (0, now)
  else writeIORef (cachePending cache) (count + 1, lastSync)

compactCache :: HashCache -> CacheHash -> IO ()
compactCache cache values = cacheIO cache $ do
  writeSnapshot (cachePath cache ++ ".new") (cacheIdentity cache) values
  installSnapshot (cachePath cache)
