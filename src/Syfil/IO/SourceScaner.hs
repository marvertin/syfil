{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Syfil.IO.SourceScaner (
 readSourceTree,
 readSourceTreeCached,
 loadStableFileRee,
 loadStableFileReeWith,
) where


import           Control.Exception
import qualified Data.ByteString      as Strict
import qualified Data.ByteString.UTF8 as BSU
import           Data.Function
import           Data.IORef
import           Data.List
import qualified Data.Map             as M
import           Data.Maybe
import           Data.Time.Clock
import           System.Directory
import           System.FilePath
import           System.IO


import           Syfil.App.Ignorances
import           Syfil.App.Log
import           Syfil.Data.Lodree
import           Syfil.Data.Ree
import           Util.DirScan
import           Util.Lib


import qualified Data.ByteString.Lazy as Lazy


getEventHandler :: UTCTime -> Log -> (EventEnvelop a ErrList -> IO ErrList, ErrList)
getEventHandler time lo  = (logInScan time lo, ErrList [])

-- EventHandler Lodree b
readSourceTree :: Log -> CacheHash -> IgnoranceDef -> FilePath -> IO (Maybe Lodree, ErrList)
readSourceTree lo cacheHash = readSourceTreeCached lo cacheHash (\_ _ -> pure ())

-- Only newly hashed stable files are journalled. Cache errors are handled by
-- the cache writer, while source read errors remain scan failures.
readSourceTreeCached :: Log -> CacheHash -> (RevPath -> Ree -> IO ()) -> IgnoranceDef -> FilePath -> IO (Maybe Lodree, ErrList)
readSourceTreeCached lo cacheHash remember ignorance rootDir = do
     startTime <- getCurrentTime
     hits <- newIORef (0 :: Int)
     misses <- newIORef (0 :: Int)
     let hashAndRemember rp path = do
           modifyIORef' misses (+1)
           value <- loadStableFileRee path
           remember rp value
           pure value
         readLFile rp = do
           let path = rootDir </> pth rp
           size <- getFileSize path
           time <- getModificationTime path
           value <- case M.lookup rp cacheHash of
             Just ree | size == rsize ree && time == rtime ree ->
               modifyIORef' hits (+1) >> pure ree
             _ -> hashAndRemember rp path
           pure (LFile value (pth rp))
     result <- scanDirectory (const makeLDir)
                     (makeFilterFce ignorance)
                     readLFile
                     (getEventHandler startTime lo)
                     rootDir
     reused <- readIORef hits
     hashed <- readIORef misses
     lo Inf ("    source hash cache: reused " ++ show reused ++ ", hashed " ++ show hashed)
     pure result

-- Force the digest before the second stat. A source changing during a read
-- must not leave a reusable cache record or silently enter the snapshot.
loadStableFileRee :: FilePath -> IO Ree
loadStableFileRee = loadStableFileReeWith computeFileHash

-- Injectable reader permits deterministic testing of changes during hashing.
loadStableFileReeWith :: (FilePath -> IO Strict.ByteString) -> FilePath -> IO Ree
loadStableFileReeWith hashFile path = do
  size <- getFileSize path
  time <- getModificationTime path
  digest <- hashFile path
  _ <- evaluate (Strict.length digest)
  afterSize <- getFileSize path
  afterTime <- getModificationTime path
  if size == afterSize && time == afterTime
    then pure (Ree 1 size time digest)
    else ioError (userError ("Source changed while hashing: " ++ path))
