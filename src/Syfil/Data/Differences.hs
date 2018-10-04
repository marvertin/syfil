{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE RecordWildCards #-}

module Syfil.Data.Differences (
  Differences(..),
  diffCountAndSizes
) where

import           Syfil.Data.Lodree
import           Util.Lib

data Differences = QDir [(FileName, Differences)]
 | QLeft Lodree
 | QRight Lodree
 | QBoth Lodree Lodree

 -- | dirrenence coune and sizeSpeed
 -- | return left count, left size, right count, right size
diffCountAndSizes :: Differences -> ((FilesCount, FileSize), (FilesCount, FileSize))
diffCountAndSizes dirCompare = let MonoidPlus2x2 result = dcas dirCompare in result
 where
   dcas :: Differences -> MonoidPlus2x2 FilesCount FileSize FilesCount FileSize
   dcas (QLeft lodree) = MonoidPlus2x2 (countsize lodree, (0,0))
   dcas (QRight lodree) = MonoidPlus2x2 ((0,0), countsize lodree)
   dcas (QBoth lodreeLeft lodreeRight) = MonoidPlus2x2 (countsize lodreeLeft, countsize lodreeRight)
   dcas (QDir list) = foldMap (dcas . snd) list

   countsize lodree = let Ree{..} = ree lodree in (rcount, rsize)


 -------------------------------------------------------------------
 -- The rest of this modul is for DEBUGING purpose only - it is dump
 --
instance Dumpable Differences where
 toDump :: Differences -> [String]

 toDump (QLeft (LFile _ _)) = ["- . "]
 toDump (QLeft (LDir _ _)) = ["- / "]
 toDump (QRight (LFile _ _)) = ["+ . "]
 toDump (QRight (LDir _ _)) = ["+ / "]
 toDump (QBoth LFile {} LFile {}) = ["~ .."]
 toDump (QBoth LFile {} LDir {}) = ["~ ./"]
 toDump (QBoth LDir {} LFile {}) = ["~ /."]
 toDump (QBoth LDir {} LDir {}) = ["~ //"]

 toDump (QDir items) = ("       " ++) <$> (items >>= todump)
    where
       todump :: (FileName, Differences) -> [String]
       todump (filename, dc@(QDir dir)) = ("/    " ++ filename) : toDump dc
       todump (filename, dc) = appendToFirst (" " ++ filename) (toDump dc)
