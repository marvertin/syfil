{-
   Scan direcotory and do some stuff for files and folders
-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.DirScan (
  scanDirectory,
  RevPath,
  emptyEventHandler,
  --stdOutLoggingEventHanler,
  --hLoggingEventHandler,
  EventHandler(..),
  Event(..),
  Cumulative(..),
  EventEnvelop(..),
  EventFile(..)
) where

import           Control.Exception
import           Control.Monad
import           Data.List
import           Data.Time.Clock
import           System.Directory
import           System.FilePath
import           System.IO
import           Text.Printf

import           Util.Lib          (FileSize, FilesCount, RevPath, pth,
                                    safeHead)

type FlowAvar = [(UTCTime, FilesCount, FileSize, UTCTime)] -- timce, count, size, header has latest
data Acum a b = Acum FlowAvar [(FilePath, a)] b deriving (Show)

data EventEnvelop a b = EventEnvelop RevPath Cumulative (Event a) b
    deriving (Show)

type EventHandler a b =   (EventEnvelop a b -> IO b, b)

data Event a
      = BeforeFile {
            efile :: EventFile
          }
      | AfterFile  {
            efile   :: EventFile,
            eresult :: a
          }
      | BeforeDir { efords :: [String] }
      | AfterDir  { efords     :: [String],
                    etuplelist :: [(FilePath, a)],
                    eresult    :: a
                  }
      | Start { erootPath :: FilePath }
      | End {
               erootPath :: FilePath,
               eresult   :: a
            }
      | Ignore
      | Failure IOException
    deriving (Show)


newtype EventFile = EventFile {
     efileSize :: FileSize
   } deriving (Show)
data Cumulative = Cumulative {
      etotalCount       :: FilesCount,
      etotalSize        :: FileSize,
      avarageCountSpeed :: Double,
      avarageSizeSpeed  :: FileSize  -- bytes per second
    } deriving (Show)

flowAvarEmpty :: UTCTime -> FlowAvar
flowAvarEmpty startTime = [(startTime, 0, 0, startTime), (startTime, 0, 0, startTime)]

emptyEventHandler :: EventHandler a ()
emptyEventHandler = (\x -> return (), ())

--stdOutLoggingEventHanler = hLoggingEventHandler stdout
--hLoggingEventHandler handle startTime = (printLog startTime handle, ())

scanDirectory :: Show a =>
        (RevPath -> [(FilePath, a)] -> a) -> -- directory node creator
        (RevPath -> Bool) ->                 -- dir or file filter
        (RevPath -> IO a) ->                -- file processor
        EventHandler a b ->
        FilePath ->                         -- scaned root
        IO (a, b)                           -- result,
scanDirectory createDirNode predicate createFileNode (eventFce, eventStart) rootPath = do
    startTime <- getCurrentTime
    let nula = 0 :: Int
    let startFlowAvar = flowAvarEmpty startTime
    evacum2 <- emitEvent' [] startFlowAvar (Start rootPath) eventStart
    Acum flowAvar ((_, result) : _) evacum3
      <- scanDirectory' 0 startTime (Acum startFlowAvar [] evacum2) []
    endTime <- getCurrentTime
    evacum4 <- emitEvent' [] [myhead flowAvar, last startFlowAvar]  (End rootPath result) evacum3
    --putStrLn $ "End scanning at " ++ show endTime ++ ", duration=" ++ show duration ++ "; total: "
    --printf "%6d# %10.3f MB | %9.2f #/s  %10.3f MB/s  \n" count (sizeInMb size) countSpeed sizeSpeed
    return (result, evacum4)
 where
  -- scanDirectory' :: Show a => Int -> Acum a -> RevPath -> IO (Acum a)
  scanDirectory' level startTime acum@(Acum flowAvar reslist evacum) revpath =
    if (not . null) revpath -- root level is not checked, predicate has never empty
        && (not . predicate) revpath then do -- if not skip whole subtree
       evacum2 <- emitEvent flowAvar Ignore evacum
       return $  Acum flowAvar reslist evacum2
    else do
      let fullPath = rootPath </> pth revpath
      -- putStrLn $ "Pokus: " ++ path
      isDir <- doesDirectoryExist fullPath
      catch (
        if isDir then do

           fords <- sort <$> listDirectory fullPath -- simple names
            -- fords <- fmap (fmap (path </>)) (listDirectory fullPath)
           evacum2 <- emitEvent flowAvar (BeforeDir fords) evacum
           Acum newFlowAvar lili evacum3 <- foldM (scanDirectory' (level + 1) startTime)
                                           (Acum flowAvar [] evacum2)
                                           (fmap (:revpath) (reverse fords)) -- foldM reverts it again
           let dirnode = createDirNode revpath lili
           evacum4 <- emitEvent flowAvar (AfterDir fords lili dirnode) evacum3
           return $  Acum newFlowAvar ((safeHead "" revpath, dirnode): reslist) evacum4

         else do
           sz <- getFileSize fullPath
           evacum2 <- emitEvent flowAvar (BeforeFile (EventFile sz)) evacum
           result <- createFileNode revpath -- can takes long
           nowTime <- getCurrentTime
           let newFlowAvar = updateFlowAvar flowAvar (1, fromIntegral sz) nowTime
           evacum3 <- emitEvent newFlowAvar (AfterFile (EventFile sz) result) evacum2
           let newAcum =  Acum newFlowAvar ((myhead revpath, result): reslist) evacum3
           return newAcum
        )  (\e  -> do
               let err = show (e :: IOException)
               evacum2 <- emitEvent flowAvar (Failure e) evacum
               return $  Acum flowAvar reslist evacum2
        )
    where emitEvent = emitEvent' revpath
  emitEvent'  revpath flowAvar event evacum =
     eventFce $ EventEnvelop revpath (getCumulative flowAvar) event evacum

  fullPth :: RevPath -> FilePath
  fullPth p = rootPath </> pth  p



updateFlowAvar :: FlowAvar -> (FilesCount, FileSize) -> UTCTime ->FlowAvar
updateFlowAvar flowavar (count', size') nowTime =
  let (lastTime, count1 , size1, latTraceTime )  = myhead flowavar
      count2 = count1 + count'
      size2 = size1 + size'
      jecas = diffUTCTime nowTime latTraceTime > 1.0
  in if jecas then (nowTime, count2, size2, nowTime) : take 10 flowavar
              else (nowTime, count2, size2, lastTime) : tail flowavar

getCumulative :: FlowAvar -> Cumulative
getCumulative flowAvar = let
     (countSpeed, sizeSpeed) = averageSpeed' (last flowAvar) (myhead flowAvar)
     (_, totalCount, totalSize, _) = myhead flowAvar
    in Cumulative totalCount totalSize countSpeed sizeSpeed



averageSpeed :: FlowAvar -> (Double, FileSize)
averageSpeed flowAvar = averageSpeed' (last flowAvar) (myhead flowAvar)

averageSpeed' :: (UTCTime, FilesCount, FileSize, UTCTime) -> (UTCTime, FilesCount, FileSize, UTCTime) -> (Double, FileSize)
averageSpeed' (time1, count1, size1, _) (time2, count2, size2, _) =
    let
        timeDiff :: Double = realToFrac  $ diffUTCTime time2 time1
    in (fromIntegral (count2 - count1) / timeDiff,
         round  (fromIntegral (size2 - size1) / timeDiff))


myhead :: [a] -> a
myhead []    = error "the list is empty DirScan"
myhead (x:_) = x
