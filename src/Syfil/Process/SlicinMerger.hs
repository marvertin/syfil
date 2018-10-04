{-# LANGUAGE InstanceSigs    #-}
{-# LANGUAGE NamedFieldPuns  #-}
{-# LANGUAGE RecordWildCards #-}

module Syfil.Process.SlicinMerger (
  mergeToLodree,
  mergesToLodree,
) where

import qualified Crypto.Hash.SHA1      as Cr

import           Syfil.Data.Lodree
import           Syfil.Data.Slicin
import           Syfil.IO.FileNamesC
import           Control.Applicative
import qualified Data.ByteString       as Strict
import qualified Data.ByteString.UTF8  as BSU
import           Data.Function
import           Data.List
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Yaml
import           GHC.Generics
import           System.Directory.Tree (DirTree (Dir, File), FileName)
import           System.FilePath

import           Util.Lib



mergesToLodree :: Lodree -> [Slicin] -> Lodree
mergesToLodree = foldl mergeToLodree

mergeToLodree :: Lodree -> Slicin -> Lodree
mergeToLodree lodree sliceTree =
   let mergeSliceTo :: Lodree -> Lodree
       mergeSliceTo = flip merge1 sliceTree -- functio
   -- must be more times applied due to resolve duplicities added in previews
   -- slice where are physical links to the same slice, thand droped the last but one
   in (iterate2 20 eqHashes (dropPenuliamate . mergeSliceTo) . mergeSliceTo) lodree
 where dropPenuliamate :: Lodree -> Lodree
       dropPenuliamate (LDir _ (last' : _ : rest)) = makeLDir (last': rest)

merge1 :: Lodree -> Slicin -> Lodree
merge1 rootLodree rootDirTree = let
--     x = 0
       rootList (LDir _ list) =  list
       newLodree = fromMaybe emptyLodree (merge' (Just $ currentLodree rootLodree) (Just rootDirTree))
    in  makeLDir ((fileNamex rootDirTree, newLodree) : rootList rootLodree)
    --in  LDir (DRee 0 0 Strict.empty) [ (rootList rootLodree)
  where
    merge' :: Maybe Lodree -> Maybe Slicin -> Maybe Lodree
    merge' Nothing Nothing = Nothing
    merge' Nothing dirtree = merge' (Just emptyLodree) dirtree -- nemámeli složku, stvoříme ji
    merge' lodree Nothing = lodree
    merge' _ (Just (File _ (RegularFile ree originalPath)))= Just $ LFile ree originalPath
    merge' lodree (Just (File _ (MetaFile content)))
      | isDelete content = Nothing
      | isLink  content = findTarget content rootLodree <|> lodree
    merge' (Just (LDir ree subdirs)) (Just (Dir name dirtrees)) =
       let pa = pairDirs subdirs (filterOutMeta dirtrees)
           qa = map (\(name, lodree, dirtree) ->  (name, merge' lodree dirtree)) pa
           ra :: [(FileName, Lodree)]
           ra = mapMaybe tupleMaybeUpSnd qa
       in if null ra then Nothing -- we dotnt want empty dirs
                     else Just (makeLDir ra)

------------------------------------ private -------------------------
pairDirs :: [(FileName, Lodree)] -> [Slicin]
             -> [(FileName, Maybe Lodree, Maybe (Slicin))]
pairDirs lodree2 dirtree =
    let preZiped = zipMaybe fst pickPureFordName lodree2 dirtree
    in map (\(name, lodree, dirtree) -> (name, snd <$> lodree, dirtree)) preZiped

filterOutMeta :: [Slicin] -> [Slicin]
filterOutMeta fulllist = let
    list = filter (isJust . extractPureFordName . fileNamex ) fulllist -- jen správně udělaná meta fajly
    (metaall, regular) = partition isMetaFile list
    regularFordNames = fileNamex <$> regular
    metaNoHiden = filter (not . (`elem` regularFordNames) . pickPureFordName) metaall -- regular file must hide metas
    metas = nubBy ((==) `on` pickPureFordName) metaNoHiden
  in metas ++ regular


extractPureFordName :: FileName -> Maybe FileName
extractPureFordName [] = Nothing
extractPureFordName fullName
  | not $ isExtensionOf metaSuffix fullName = Just fullName
extractPureFordName ('~' : zbytek) =
   let pureName = dropWhile ('~' ==) . dropWhile ('~' /=) $ takeBaseName zbytek
   in if null pureName then Nothing
                       else Just pureName
extractPureFordName _ = Nothing

pickPureFordName :: DirTree a -> FileName
pickPureFordName = fromJust . extractPureFordName . fileNamex  -- function returning name in meta and no meta files

isMetaFile :: Slicin -> Bool
isMetaFile (File _ (MetaFile _)) = True
isMetaFile _                     = False

isDelete :: SliceCmd -> Bool
isDelete Delete = True
isDelete _      = False


isLink :: SliceCmd -> Bool
isLink (Link _) = True
isLnk _         = False

findTarget :: SliceCmd -> Lodree -> Maybe Lodree
findTarget cmd = findLodreeNode $ getLinkTarget cmd

getLinkTarget :: SliceCmd -> FilePath
getLinkTarget (Link target)  = target

eqHashes :: Lodree -> Lodree -> Bool
eqHashes = (==) `on` hashLodree
