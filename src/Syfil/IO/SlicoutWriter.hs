{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Syfil.IO.SlicoutWriter (
  writeBackup,
)  where


import           Control.Arrow
import           Control.Monad
import           Data.IORef
import           Data.List
import qualified Data.Map              as M
import           Data.Time.Clock
import           System.Directory
import           System.Directory.Tree
import           System.Exit
import           System.FilePath
import           System.IO
import           Text.Printf


import           Syfil.App.Config
import           Syfil.App.Log
import qualified Syfil.Data.Slicin      as YDSI
import           Syfil.Data.Slicout     as YDSO
import           Syfil.IO.FileNamesC
import qualified Data.ByteString.Lazy  as BS
import           Util.Lib

-- | Write backu. returns (count of copies files, sizue of set files, count of metafiles)
writeBackup :: Log -> AnchoredSlicout -> ForestDef ->  IO (AnchoredDirTree Stat3)
writeBackup lo abt@(base :/ bt@(Dir newSliceName _)) forest = do
        let (Dir _ subdirs) = (first replaceWithSorucePath) <$> zipPaths ("" :/ bt)
        counters <- newIORef mempty
        writeDirectoryWith (writeFileToBackup counters) (base :/ (Dir (replaceVerticalToSlashes newSliceName) subdirs))
    where
      replaceWithSorucePath :: FilePath -> FilePath
      replaceWithSorucePath fp = let
           (treeName, path) = break (=='/') . drop (length newSliceName + 1) . replaceBacklashesToSlashes $ fp
           in case tdPath <$> find ((==treeName) . tdName) forest of
             Nothing ->  error $ "Imposible has happened! \"" ++ treeName ++ "\" not found in " ++ show forest ++ " the rest is \"" ++ path ++ "\""
             Just root -> root ++ path

      writeFileToBackup :: IORef Stat3 -> FilePath -> (FilePath, Cmd) -> IO Stat3
      writeFileToBackup  counters destPath (sourcePath, (Insert _ _))  = do
           lo Debug $ printf "copy file: \"%s\" --> \"%s\"" sourcePath destPath
           lo Progress $ printf "copy file: \"%s\" --> \"%s\"" sourcePath destPath
           BS.readFile sourcePath >>= BS.writeFile destPath
           -- createDirectory $ ":|:" ++ destPath
           fsize <- getFileSize destPath
           modifyIORef' counters (mappend (MonoidPlus3 (1, fsize, 0)))
           show <$> readIORef counters >>= lo Debug

           return $ MonoidPlus3 (1, fsize, 0)

      writeFileToBackup counters path (_, cmd) = do
           let (dir, file) = splitFileName path
           let cesta = dir </> (kindOfChange cmd ++ file) ++ metaSuffix
           lo Debug $ printf  "create meta: \"%s\": %s" cesta (showCmd cmd )
           writeFile cesta (formatCmd cmd)
           modifyIORef' counters (mappend (MonoidPlus3 (0, 0, 1)))
           show <$> readIORef counters >>= lo Debug
           return $ MonoidPlus3 (0,0,1)

showCmd :: Cmd ->  String
showCmd (YDSO.Insert {}) = "<insert>"
showCmd (YDSO.Delete {}) = "<delete>"
showCmd (YDSO.Link fp _) = "<link \"" ++ fp ++  "\" >"



title x = ["", "----- " ++ x ++ " ---------------------------"]

formatCmd :: Cmd -> String
formatCmd cmd = unlines $ concat $ [
     (YDSI.formatMetaFileHeader . convertToSliceCmd) cmd,
     title "Operation",
     [kindOfChange cmd],
     case cmd of
           Link _ info      -> formatInfo info
           YDSO.Delete info -> formatInfo info
  ]

formatInfo :: Info -> [String]
formatInfo (Info hash Paths{..} lodree)  = concat [
    title "Hash" ,
    [toHexStr hash],
    [show hash],
    title "Source paths",
    pathsNew,
    title "Last slice paths",
    pathsLast,
    title "History paths",
    pathsHistory,
    title "Tree",
    toDump lodree
   ]


convertToSliceCmd :: Cmd -> YDSI.SliceCmd
convertToSliceCmd (YDSO.Link  x _) = YDSI.Link x
convertToSliceCmd (YDSO.Delete _ ) = YDSI.Delete
