{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}

module Syfil.Command.MarkDuplicities (
  cmdMarkDuplicities
) where

import           Control.Monad
import qualified Data.Map            as M
import           Data.Time.Clock
import           System.Directory
import           System.Exit
import           System.FilePath


import           Syfil.App.Context
import           Syfil.App.Log
import           Syfil.Command.Backup
import           Syfil.Data.Lodree
import           Syfil.Data.Ree
import           Util.DirScan
import           Util.Lib




getEventHandler :: UTCTime -> Log -> (EventEnvelop a ErrList -> IO ErrList, ErrList)
getEventHandler time lo  = (logInScan time lo, ErrList [])

cmdMarkDuplicities :: FilePath -> Ctx -> IO ExitCode
cmdMarkDuplicities markedDir ctx@Ctx{..} = do
    (slicinLodree, failusSlices) <- scanSlices ctx
    putStrLn $ "Scaning dir for mark: \"" ++ markedDir ++ "\""
    startTime <- getCurrentTime
    (markedLodree, _) <- scanDirectory (const makeLDir)
                    (const True)
                    loadFile
                    (getEventHandler startTime lo)
                    markedDir
    let hashesSlicin = createMapOfHashes slicinLodree
    let hashesMarked = createMapOfHashes markedLodree
    let beMarked = M.intersection hashesMarked hashesSlicin
    let list = concat (fst <$> filter (isFile . snd) (M.elems beMarked) )
    forM_ list (\p -> do
      let path1 = markedDir ++ p
      let path2 = takeDirectory path1 ++ "/~DUPLICITY~" ++ takeFileName path1
      lo Inf path2
      renameFile path1 path2
      )
    return ExitSuccess
  where
    loadFile :: RevPath -> IO Lodree
    loadFile rp = do
      ree <- loadFileRee (markedDir </> pth rp)
      return $ LFile ree ""
