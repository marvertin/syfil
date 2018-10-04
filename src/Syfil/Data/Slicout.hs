{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE InstanceSigs         #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Syfil.Data.Slicout (
  Slicout,
  AnchoredSlicout,
  Cmd(..),
  Paths(..),
  Info(..),
  sizeToBackup,
  countsToBackup,
  modificationTimes,
  countCounters,
  kindOfChange,
) where

import           Data.Counter
import           Data.List
import qualified Data.Map              as M
import qualified Data.Set              as S
import           System.FilePath.Posix

import           Syfil.Data.Lodree
import           System.Directory.Tree
import           Util.Lib




data Info = Info Hash Paths Lodree  deriving (Show) -- gives information only to peaple, not processed by machine
data Cmd = Insert FileSize UTCTime | Delete Info | Link FilePath Info  deriving (Show)
data Paths = Paths { pathsNew :: [FilePath], pathsLast:: [FilePath], pathsHistory :: [FilePath] }  deriving (Show)

type Slicout = DirTree Cmd
type AnchoredSlicout = AnchoredDirTree Cmd

info (Delete i) = i
info (Link _ i) = i

sizeToBackup :: Slicout -> FileSize
sizeToBackup bt = sum $ fmap mapa bt
   where mapa (Insert sz _) = sz
         mapa _             = 0

countsToBackup :: Slicout -> FilesCount
countsToBackup bt = sum $ fmap mapa bt
  where mapa (Insert _ _) = 1
        mapa _            = 0

modificationTimes :: Slicout ->  M.Map FilePath UTCTime
modificationTimes bt = M.fromList . sort $ foldMap extractTime (zipPaths' bt)
  where
     extractTime (fp, (Insert _ time)) = [(fp, time)]
     extractTime _                     = []


countCounters :: Slicout -> Counter String Int
countCounters = count . (foldMap (return . kindOfChange))


data Kardinality = Zero | One | Many

kindOfChange :: Cmd -> String
kindOfChange (Insert{})                                     = "~INSERT~"
kindOfChange cmd =
  let Info _ paths _ = info cmd
      (pathsLast, pathsNew, pathsCommon) = pickPaths paths

      k :: Cmd -> Kardinality -> Kardinality -> Bool -> String
      k cmd Many Zero hasHistory = k cmd One Zero hasHistory
      k cmd Zero Many hasHistory = k cmd Zero One hasHistory
      k _ One One _              = if isSame takeDirectory then "RENAME" else
                                       if isSame takeFileName then "MOVE"
                                                     else "MOVE-RENAME"
      k _ One Many _             = "FORK"
      k _ Many One _             = "JOIN"
      k _ Many Many _            = "CROSS"
      k Delete{} One Zero False  = "DELETE"
      k Delete{} One Zero True   = "UNLINK"
      k Link{} Zero One False    = if null (pathsHistory paths) then "NEW" else "RESTORE"
      k Link{} Zero One True     = "CLONE"
      k _ _ _ _ = "IMPOSSIBLE"

      isSame fce = fce (head pathsLast) == fce (head pathsNew)
  in "~" ++ case cmd of
          Delete{} -> "DEL_"
          Link{}   -> "INS_"
      ++ (k cmd (kardinality pathsLast) (kardinality pathsNew) (not . null $ pathsCommon))
      ++ "~"

  where
    pickPaths :: Paths -> ([FilePath],  [FilePath], [FilePath])
    pickPaths Paths{..} =
      let palast = S.fromList pathsLast
          panew = S.fromList pathsNew
          common = palast `S.intersection` panew
      in  (S.toList $ palast S.\\ common ,  S.toList $ panew S.\\ common, S.toList common)


    kardinality :: [a] -> Kardinality
    kardinality []  = Zero
    kardinality [_] = One
    kardinality _   = Many

instance Stat3Compute Cmd where
  computeSizes (Insert size _) = MonoidPlus3 (1, size, 0)
  computeSizes Delete{}        = MonoidPlus3 (0, 0, 1)
  computeSizes Link{}          = MonoidPlus3 (0, 0, 1)


instance Stat3Compute Slicout where
  computeSizes :: Slicout -> Stat3
  computeSizes = foldMap computeSizes


instance Dumpable Cmd where
    toDump x = [ "**" ++ show x ]
    toDumpS = tostra
      where
          tostra (Insert size time) = "Insert " ++ (showSz size) ++ " " ++ (show time)
          tostra x             = show x


instance Dumpable Slicout where
  toDump = dirTreeToStringList printCmd

printCmd :: Cmd ->  Maybe String
printCmd (Insert {}) = Just "<insert>"
printCmd (Delete {}) = Just "<delete>"
printCmd (Link fp _) = Just $ "<link \"" ++ fp ++  "\" >"
