{-# LANGUAGE NamedFieldPuns #-}
module Syfil.App.Log (
  withLogger,
  Level(..),
  Log,
  logInScan
  -- lo
) where

import           Control.Monad
import qualified Data.ByteString              as BS
import qualified Data.ByteString.UTF8         as BSU
import           Data.IORef
import           Data.Time.Clock
import qualified System.Console.Terminal.Size as Terminal
import           System.IO
import           Text.Printf

import           Util.DirScan
import           Util.Lib

type Log = Level -> String -> IO ()

data Level
  = Debug -- only to log file
  | Inf -- to stdout and to log file
  | Progress -- only to stdout only if terminal is attached, not scroll "\rtext\t"
  | Summary -- only to sumary file
  | Error -- to stderr and to log file
  deriving (Show)

withLogger :: FilePath ->  FilePath -> (Log -> IO a) -> IO a
withLogger mainLogPath sliceLogPath fce = do
  charOnLineCounter <- newIORef 0
  isTerm <- hIsTerminalDevice stdout
  withFile mainLogPath AppendMode (\yhandle ->
    withFile sliceLogPath WriteMode (\shandle ->
      fce (doLog isTerm charOnLineCounter yhandle shandle)
     )
   )

-- fmap ((show l ++ ": "):) .
doLog :: Bool -> IORef Int -> Handle -> Handle -> Level -> String -> IO()
doLog isTerminal charOnLineCounter yh sh l s = do
    let ss = show l ++ ": " ++ s
        printToDebugLog = do
          time' <- getCurrentTime
          hPutStrLn sh $ (take 19 $ show time') ++ ": " ++ ss
          hFlush sh -- is not it too ofen
          return ()
    case l of
      Debug -> do
        time' <- getCurrentTime
        printToDebugLog
      Inf -> do
        cleanLine
        putStrLn s
        hFlush stdout
        printToDebugLog
      Progress -> do
        when isTerminal $ do
          with <-  termWidth
          case with of
            Nothing -> return ()
            Just width -> do
              cleanLine
              let utf8Diff = BS.length (BSU.fromString s) - length s
              let txt = take (width - 1 - utf8Diff) s
              writeIORef charOnLineCounter (length txt + utf8Diff)
              putStr $ '\r' : txt ++ "\r"
              hFlush stdout
      Summary -> do
        time <- getCurrentTime
        hPutStrLn yh $ (take 19 . show $ time) ++ ": " ++ s
        hFlush yh
        printToDebugLog
      Error -> do
        hPutStrLn stderr $ s
        printToDebugLog
  where
    cleanLine = do -- cleaneng line of progress
      when isTerminal $ do
       charsToClean <- readIORef charOnLineCounter
       when (charsToClean > 0) $ do
           width <-  termWidth
           case width of
              Nothing -> return ()
              Just width -> do
                 putStr $ replicate (min charsToClean (width - 1)) ' ' ++ "\r"
                 writeIORef charOnLineCounter 0

termWidth :: IO (Maybe Int)
termWidth = fmap (fmap Terminal.width) Terminal.size
--termWidth = return (Just 100)

loa :: String -> IO ()
loa xx = do
   bufan <- hGetBuffering stdout
   let kon =  LineBuffering == bufan
   putStr $ show kon ++ ": "
   putStrLn xx
   hPutStrLn stderr xx

logInScan :: UTCTime -> Log -> EventEnvelop a ErrList -> IO ErrList
logInScan startTime lo (EventEnvelop revpath (Cumulative count' size' countSpeed sizeSpeed) event errList) = do
 case event of
   Ignore -> do
     lo Debug $ "IGNORE: " ++ pth revpath
     return errList
   Start rootPath -> do
     lo Debug $  "========================================================================="
     lo Debug $ "Start scanning: " ++ rootPath ++ " at " ++ show startTime
     return errList
   End _ _ -> do
     endTime <- getCurrentTime
     lo Debug $ "End scanning at " ++ show endTime ++ ", duration=" ++ showDuration endTime ++ "; total: "
     -- not LN
     lo Debug $ printf "%6d# %s | %9.2f #/s  %s/s  " count' (showSz size') countSpeed (showSz sizeSpeed)
     return errList
   AfterFile (EventFile size)  _ -> do
     time' <- getCurrentTime
     -- lo Debug $  duration time'
     forM_ [lo Debug, lo Progress] ($  printf "%s | %s - %s" (showProgress time') (showSz size) (pth revpath))
     return errList
   BeforeFile (EventFile size) -> do
     when (size > 1024 * 1024 * 100) (do
       time' <- getCurrentTime
       forM_ [lo Debug, lo Progress] ($  printf "%s | big file: %s - %s" (showProgress time') (showSz size) (pth revpath))
       )
     return errList
   Failure exc -> do
     let errstr = "!!!!! ERROR !!!!! " ++ show exc
     lo Error $ "    " ++ errstr
     return  $ ErrList (errstr: getErrList errList)
   _ -> return errList

 where showDuration time' = take 6 (show (diffUTCTime time' startTime)) ++ "s "
       showProgress :: UTCTime -> String
       showProgress time' = printf "%s %6d # %s %9.2f #/s  %s/s"
           (showDuration time' ) count' (showSz size') countSpeed (showSz sizeSpeed)

-- getEventHandler lo  = (logInScan lo, getCurrentTime)
