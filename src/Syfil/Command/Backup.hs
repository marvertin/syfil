{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections   #-}


module Syfil.Command.Backup (
  cmdBackup,
  scanSlices
) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Counter
import           Data.List
import qualified Data.Map                    as M
import           Data.Time.Clock
import           Data.Tuple
import           Data.Yaml
import           System.Directory
import           System.Directory.Tree
import           System.Exit
import           System.FilePath
import           System.IO
import           Text.Printf
import           Text.RawString.QQ

import           Syfil.App.Config
import           Syfil.App.Context
import           Syfil.App.Ignorances
import           Syfil.App.Log
import           Syfil.Data.Differences
import           Syfil.Data.Lodree
import           Syfil.Data.Ree
import           Syfil.Data.Slicin
import           Syfil.Data.Slicout
import           Syfil.IO.FileNamesC
import           Syfil.IO.RestoreScriptWriter
import           Syfil.IO.SliceNameStrategy
import           Syfil.IO.SlicinScaner
import           Syfil.IO.SlicoutWriter
import           Syfil.IO.SourceScaner
import           Syfil.Process.SlicinMerger
import           Syfil.Process.SlicoutBuilder
import           Syfil.Process.TreeComparator
import           Util.DirScan
import           Util.Lib


getEventHandler :: UTCTime -> Log -> (EventEnvelop a ErrList -> IO ErrList, ErrList)
getEventHandler time lo  = (logInScan time lo, ErrList [])


cmdBackup :: Ctx -> IO ExitCode
cmdBackup ctx@Ctx{..} =  do -- gcc crashes whne versio is obtain from here
  startTime <- getCurrentTime
  exitCode <- do
    tmStart <- getCurrentTime

    (rootLodree, failusSlices) <- scanSlices ctx
    (lodreeSourceAllNodes, failusSurces) <- scanSources ctx
    resultOfCompare <- compareSlicesToSources ctx rootLodree lodreeSourceAllNodes
    failusCopy <- copyFiles ctx resultOfCompare

    forM_ (failusSlices ++ failusSurces ++ failusCopy) (\msg -> do
        lo Summary $  msg
      )
    if null failusCopy && null failusSurces
         then do
           when (not . null $ empties) $ do
              forM_ [lo Inf, lo Summary] ($  "!!!! WARNING: some trees are empty: " ++ (show empties))
           lo Inf $ showSuccess
           return ExitSuccess
         else do
           when (not . null $ failusSlices) $
             lo Error $ printf  "!!!!! %d ERRORS while scaning slices !!!!!" (length failusSlices)
           when (not . null $ failusSurces) $
             lo Error $ printf  "!!!!! %d ERRORS while scaning sources !!!!!" (length failusSurces)
           when (not . null $ failusCopy) $
             lo Error $ printf  "!!!!! %d ERRORS while copiing files !!!!!" (length failusCopy)
           lo Error $ "!!!!! " ++ show (length failusCopy + length failusSurces + length failusSlices) ++ " ERRORS totally !!!!!"
           return $ ExitFailure 1

  endTime <- getCurrentTime
  forM_ [lo Inf, lo Summary] ($ "Total time: " ++ showDiffTm endTime startTime ++ "\n")
  return exitCode


scanSlices :: Ctx -> IO (Lodree, [String])
scanSlices ctx@Ctx{..} = do
  lo Inf "Phase 1/4 - reading slices backed up before"
  tmStart <- getCurrentTime
  sliceNames <-  listSlices  sliceNameStrategy dataRoot
  lo Inf $ if null sliceNames
              then "    no slices was backed up yet"
              else let decorate fce = (++"\"") . ("\""++) .  replaceVerticalToSlashes . fce
                   in printf "    %d slices: %s ... %s" (length sliceNames) (decorate head sliceNames) (decorate last sliceNames)
  forM_  (zip [1 :: Int ..] sliceNames) (\(n, slicen) -> lo Debug $ printf "%6d. %s" n slicen )
  (slices, failusSlices) <- fmap (>>= getErrList) . unzip <$>
      forM sliceNames (\name -> do
        let sliceIndexPath = (replaceVerticalToSlashes (indexRoot </> name </> sliceIndexName))
        let sliceIndexTempPath = (replaceVerticalToSlashes (indexRoot </> name </> "~~" ++ sliceIndexName))
        createDirectoryIfMissing True (takeDirectory sliceIndexPath)
        (slice, errs) <- doesFileExist sliceIndexPath >>=
          (\exists -> if exists then do
                                   slice <- (decodeFileThrow sliceIndexPath :: IO Slicin)
                                   return (slice, ErrList [])
                                else do
                                   (slice, errs) <- readSlice (getEventHandler tmStart lo) (dataRoot </> name)
                                   when (null . getErrList $ errs)  $ do -- write index only whne there are no errors
                                     encodeFile sliceIndexTempPath slice
                                     slice2 <- (decodeFileThrow sliceIndexTempPath :: IO Slicin)
                                     if (slice == slice2)
                                        then do
                                               renameFile sliceIndexTempPath sliceIndexPath
                                               lo Inf $ "    created index: \"" ++ sliceIndexPath ++ "\""
                                        else
                                               lo Error $ "IMPOSSIBLE: written a read slices are not same!"
                                   return (slice, errs)
             )
        lo Inf $ printf "        %s: %s" (replaceVerticalToSlashes name) (show (computeSizes slice))
        return (slice, errs)
    )
  let statTotal =  foldMap computeSizes slices
  lo Inf $ printf "    total slices:    %s" (show statTotal)
  let rootLodree = mergesToLodree emptyLodree slices
  lo Inf $ printf "    all slices merged:   %s" (showRee . ree . currentLodree $ rootLodree)
  let lodreeBackupCurrent = currentLodree rootLodree
  -- M.Map FilePath UTCTime

  anyScriptCreated <- createRestoreScripts ctx rootLodree
  when (anyScriptCreated) $ lo Inf "    Any restore script has been created."
  -- createDirectoryIfMissing True (takeSlicedIndexPath newSliceName)
  encodeFile (takeSlicedLogPath newSliceName </> sliceLogicalTree_suffix) lodreeBackupCurrent

  tmEnd <- getCurrentTime
  lo Inf $ showPhaseTime tmEnd tmStart
  lo Summary $ printf "scaned %d slices - %s (%s)" (length sliceNames) (show statTotal) (showDiffTm tmEnd tmStart)
  return (rootLodree, failusSlices)
  -- return ()


scanSources :: Ctx -> IO (Lodree, [String])
scanSources ctx@Ctx{..} = do
  lo Inf "Phase 2/4 - reading source forest for backup"
  tmStart <- getCurrentTime
  lo Inf $ printf "    %d trees in forest " (length forest)
  (lodreeSourceAllNodes, failusSurces) <- bimap makeLDir (>>= getErrList) . unzip <$>
       forM forest ( \(TreeDef treeName treePath ignorances) -> do
          lo Inf $ printf "       scaning %-15s- \"%s\"" treeName treePath
          lo Debug $ "ignorance patterns: " ++ (show ignorances)
          tmSourceStart <- getCurrentTime
          let cacheHashPath = (indexRoot </> (indexVersion ++ "_" ++ treeName ++ ".yaml"))
          cacheHash <- doesFileExist cacheHashPath >>=
                             (\exists -> if exists then (decodeFileThrow cacheHashPath :: IO CacheHash)
                                                   else return M.empty)
          (lodreeSourceOneNode, errList) <- readSourceTree lo cacheHash ignorances treePath
          -- encodeFile (indexRoot </> (treeName ++ sliceSourceTree_suffix)) lodreeSourceOneNode
          -- encodeFile cacheHashPath (flattenFileLodree lodreeSourceOneNode)
          encodeFile cacheHashPath (M.fromList (flattenFileLodree lodreeSourceOneNode))
          tmSourceEnd <- getCurrentTime
          lo Summary $ printf "source %-15s-%s \"%s\" (%s)" treeName (showRee (ree lodreeSourceOneNode)) treePath (showDiffTm tmSourceEnd tmSourceStart)
          return ((treeName, lodreeSourceOneNode), errList)
     )
  lo Inf $ "    " ++ showRee (ree lodreeSourceAllNodes)
  tmEnd <- getCurrentTime
  lo Inf $ showPhaseTime tmEnd tmStart
  lo Summary $ printf "forest of %d trees %s (%s)" (length forest) (showRee (ree lodreeSourceAllNodes)) (showDiffTm tmEnd tmStart)
  return (lodreeSourceAllNodes, failusSurces)

compareSlicesToSources :: Ctx -> Lodree -> Lodree -> IO (Maybe Slicout)
compareSlicesToSources ctx@Ctx{..} rootLodree lodreeSourceAllNodes = do
  lo Inf "Phase 3/4 - comparing slices and source forest"
  tmStart <- getCurrentTime

  let maybeResultDiff = compareTrees (currentLodree rootLodree) lodreeSourceAllNodes

  maybeSliceout <- do
    case maybeResultDiff of
      Nothing -> do
         lo Inf $ "    no differences"
         return Nothing
      Just resultDiff -> do
         let lf x = takeSlicedLogPath newSliceName </> x
         dumpToFile (lf "differences.log") resultDiff
         let mapOfHashes = createMapOfHashes3 (rootLodree, lodreeSourceAllNodes)
         let (allSlicinHashes, sourceHashes, lastSlicinHashes) = mapOfHashes in do
            dumpToFile (lf "hashes-allSlicin.log") allSlicinHashes
            dumpToFile (lf "hashes-sources.log") sourceHashes
            dumpToFile (lf "hashes-lastSlicin.log") lastSlicinHashes
         let resultSliceout = buildSlicout mapOfHashes resultDiff newSliceName
         dumpToFile (lf "slicout.log") resultSliceout
         forM_ [lo Inf, lo Summary] ($ formatDiffResult resultDiff)
         forM_ [lo Inf, lo Summary] ($ printf "    %s+ %s" (replaceVerticalToSlashes newSliceName) (show $ computeSizes resultSliceout))
         forM_ (M.toList $ countCounters resultSliceout) (\(key, value) ->
             forM_ [lo Inf, lo Summary] ($ printf "%8d * %s" value key)
             )
         return $ Just resultSliceout
  tmEnd <- getCurrentTime
  lo Inf $ showPhaseTime tmEnd tmStart
  return maybeSliceout

copyFiles :: Ctx -> Maybe Slicout -> IO [String]
copyFiles ctx@Ctx{..} resulta = do
  lo Inf $ "Phase 4/4 - copying files to new slice"
  tmStart <- getCurrentTime
  case resulta of
    Nothing -> do
       lo Inf $ "    skipped, no differencies, NO BACKUP NEEDED: "
       return []
    Just backupDirTree -> do
       lo Inf $ "    Writing new slice to: " ++ takeSlicedDataPath newSliceName
       createDirectoryIfMissing True (takeSlicedDataPath newSliceName)
       encodeFile (takeSlicedDataPath newSliceName ++ "/" ++ modificationTimesFileName)  (modificationTimes backupDirTree)
       (_  :/ resultOfCopy) <- writeBackup lo (dataRoot :/ backupDirTree) forest
       let failus :: [DirTree Stat3]
           failus = failures resultOfCopy
       let failusStr = (show . err) <$> failus
       forM_ failusStr (\msg -> do
           lo Error $ "    !!!!! ERROR !!!!! " ++ msg
         )
       let msg = printf "%s+ %s"  (replaceVerticalToSlashes newSliceName) (show $ foldMap id resultOfCopy)  :: String
       lo Inf $ printf "    %s" msg

       tmEnd <- getCurrentTime
       lo Summary $ printf "%s (%s)" msg (showDiffTm tmEnd tmStart)
       lo Inf $ showPhaseTime tmEnd tmStart
       return failusStr

showPhaseTime tmEnd tmStart  = "    (" ++ showDiffTm tmEnd tmStart ++ ")"

showSuccess = [r|---------------------------------------------------------
*** OK *** OK *** OK *** SUCCESS *** OK *** OK *** OK ***
|]

formatDiffResult :: Differences -> String
formatDiffResult  compareResult =
  let ((cl, sl), (cr, sr)) = diffCountAndSizes compareResult
  in printf "    deleted (#%d, %s), inserted (#%d, %s), diff (#%d, %s)"
                  cl (showSz sl) cr (showSz sr) (cr - cl) (showSz (sr - sl))
