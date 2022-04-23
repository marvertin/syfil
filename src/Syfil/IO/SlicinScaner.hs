{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Syfil.IO.SlicinScaner
    (
    readSlice,
    ) where

import           Control.Exception
import           Control.Monad
import qualified Crypto.Hash.SHA1      as Cr
import qualified Data.ByteString       as Strict
import qualified Data.ByteString.Lazy  as Lazy
import qualified Data.ByteString.UTF8  as BSU
import           Data.List
import qualified Data.Map              as M
import           Data.Maybe
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Data.Yaml
import           GHC.Generics
import           GHC.IO.Encoding
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           System.IO             (hFlush, stdout)
import           Text.Printf           (printf)
import           Text.Regex.Posix

import            Syfil.App.Log 
import           Syfil.Data.Ree
import           Syfil.Data.Slicin
import           Syfil.IO.FileNamesC
import           Util.DirScan
import           Util.Lib




readSlice :: EventHandler Slicin ErrList -> FilePath -> IO (Slicin, ErrList)
readSlice eventHandler rootDir = do 
    modificationTimes <- loadModificationTimesFile rootDir
    scanDirectory mkDir filterFilesInRoot (readSFile modificationTimes) eventHandler
          (replaceVerticalToSlashes rootDir) -- >>= ((takeDirectory rootDir ):/)
  where
    -- rootDir1 = (takeFileName . takeDirectory) rootDir ++ "|" ++ takeFileName rootDir -- it os not filename but root directory
    rootDir1 = takeFileName rootDir -- it is not filename but root directory
    readSFile :: M.Map FilePath UTCTime -> RevPath -> IO Slicin
    readSFile modificationTimes rp = File (head rp) <$> loadSliceFile modificationTimes rootDir rp

    mkDir rp list = Dir (safeHead rootDir1 rp) (filter (not . isEmptyDir) . fmap snd $ list)

    filterFilesInRoot [fordName] = takeExtension fordName /= ".yaml"
    filterFilesInRoot _          = True



loadSliceFile :: M.Map FilePath UTCTime -> FilePath -> RevPath -> IO SliceFile
loadSliceFile modificationTimes rootPath rp = do
  let path = replaceBacklashesToSlashes (pth rp)
  let realPath = replaceVerticalToSlashes (rootPath </> path)
  size <- getFileSize realPath
  time <- getModificationTime realPath
  hash <- computeFileHash  realPath
  
  let effectiveTime = M.findWithDefault time path modificationTimes
  let originalPath = "/" ++ takeFileName rootPath ++ "/" ++ path
  if not $ isMetaFileName path then return (RegularFile  (Ree 1 size effectiveTime hash) originalPath )
          else (MetaFile . parseMetaFile . T.unpack) <$> TIO.readFile realPath

isMetaFileName :: FilePath -> Bool
isMetaFileName path =
  (metaSuffix `isSuffixOf` path || ".yaba" `isSuffixOf` path)
  && (takeFileName path =~ ("^~.+~" :: String))





isEmptyDir :: Slicin -> Bool
isEmptyDir (Dir _ []) = True
isEmptyDir _          = False


loadModificationTimesFile :: FilePath ->  IO (M.Map FilePath UTCTime)
loadModificationTimesFile rootSliceDir = do
  let modificationTimesFilePath =   replaceBacklashesToSlashes . replaceVerticalToSlashes $ rootSliceDir ++ "/" ++ modificationTimesFileName
  doesFileExist modificationTimesFilePath >>=
      \exist -> if exist  then (decodeFileThrow modificationTimesFilePath :: IO (M.Map FilePath UTCTime))
                          else return M.empty