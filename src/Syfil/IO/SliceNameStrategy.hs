{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric  #-}

module Syfil.IO.SliceNameStrategy (
  SliceNameStrategy(),
  defaultSliceNameStrategy,

  listSlices,
  nextSliceName,
  checkSliceNamePattern

) where

import           Control.Monad
import           Data.Char
import           Data.List
import           Data.Maybe
import           Data.Time.Calendar
import           Data.Time.Clock
import           Data.Time.Format
import           Data.Yaml
import           GHC.Generics
import           System.Directory
import           System.FilePath
import           System.FilePath.Find
import           System.IO
import           Text.Printf
import           Text.Regex.Posix

import           Util.Lib

data SliceNameStrategy = Pattern String
   deriving (Show, Generic, FromJSON, ToJSON)

defaultSliceNameStrategy = Pattern "*-*/001"

currentUtcTimeFormatted :: IO String
currentUtcTimeFormatted = do
  now <- getCurrentTime
  return $ formatTime defaultTimeLocale (iso8601DateFormat (Just "%H-%M-%SZ")) now


listSlices :: SliceNameStrategy -> FilePath -> IO [FilePath]
listSlices sns path = fmap replaceSlashesToVertical <$> listSlices' path
 where
  listSlices' = listDirSortedN $ numberOfParts sns

listDirSorted :: FilePath -> IO [FilePath]
listDirSorted dir = sort <$> listDirectory dir

listDirSortedN :: Int -> FilePath -> IO [FilePath]
listDirSortedN n rootDir = fmap replaceBacklashesToSlashes <$> listd n ""
  where
     listd :: Int -> FilePath -> IO [FilePath]
     listd 0 x   = return [x]
     listd n dir = do
        names <- listDirSorted (rootDir </> dir)
        concat <$> mapM (listd (n - 1) . (dir </>)) names

-- | lastSliceLatPartName "../../.." ["aa/xx/11", "bb/yy/222"] = Just "222"
lastSliceLatsPartName :: SliceNameStrategy -> FilePath -> IO (Maybe String)
lastSliceLatsPartName strategy root =
    lastSliceLatsPartName' <$> listDirSortedN (numberOfParts strategy) root

-- pure functions --

checkSliceNamePattern :: SliceNameStrategy -> Bool
checkSliceNamePattern (Pattern pattern)
  = length (filter (=='*') pattern) <= 6
    && pattern =~ ("^[^?/|\\0-9]+(/[^?/|\\0-9]+)*(/[0-9]+)?$" :: String)

lastSliceLatsPartName' :: [FilePath] -> Maybe String
lastSliceLatsPartName' list =
    fmap (lastPart . replaceSlashesToVertical ) . listToMaybe . reverse $ list
  where lastPart = reverse . takeWhile (/='|') . reverse


nextSliceName :: FilePath -> SliceNameStrategy -> IO String
nextSliceName sliceRoot strategy@(Pattern pat) = do
  time <- currentUtcTimeFormatted
  lastSliceLastPart <- lastSliceLatsPartName strategy sliceRoot
  when (not ((fromMaybe "0" lastSliceLastPart) =~ "[a-z]?[0-9]+" :: Bool) ) (do
      hPrintf stderr "Then last path element %s in subtree of %s is not number, have not you manipulate with slice name strategy patterns?\n"
                          (show lastSliceLastPart) sliceRoot
    )
  return $ replaceSlashesToVertical $ buildSliceName pat time lastSliceLastPart

-- | next number with numerical and lexicoraphical order
nextn :: String -> String
nextn whole@(letter : digits)
  | isDigit letter = dropWhile (== pred 'a') . nextn $ pred 'a' : whole
  | otherwise =
      let newNum = 1 + read digits :: Int
          digits2 = show newNum
      in if length digits2 > length digits
         then (succ letter) : reverse digits2
         else letter : printf ("%0" ++ show (length digits) ++ "d") newNum

-- | "y*M*/*-*xx/0001" "2042-12-17T23-15-47" = "y2042M12/17-23xx/0001"
buildSliceName :: String -> String -> Maybe String -> String
buildSliceName pattern stime lastSliceName = bsn pattern stime
  where
    bsn [] _ = []
    bsn ('*': xs) stime =
        let (nums, rest) = spanNums stime
        in nums ++ bsn xs rest
    bsn whole@(char: xs) stime
        | isDigit char = fromMaybe whole (nextn <$> lastSliceName)
        | otherwise = char : bsn xs stime

spanNums :: String -> (String, String)
spanNums s = let (x, y) = span isDigit s
             in (x, dropWhile (not . isDigit) y)

numberOfParts :: SliceNameStrategy -> Int
numberOfParts (Pattern pat) = length (filter (=='/') pat) + 1
