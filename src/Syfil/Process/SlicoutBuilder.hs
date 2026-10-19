{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Syfil.Process.SlicoutBuilder (
  buildSlicout,
  createMapOfHashes3,
) where


import           Data.List                   (isPrefixOf, mapAccumL)
import qualified Data.Map                    as M
import           Data.Maybe
import           System.Directory.Tree

import           Syfil.Data.Differences
import           Syfil.Data.Lodree
import           Syfil.Data.Slicout
import           Syfil.Process.TreeComparator
import           Util.Lib

mapCall :: (s -> RevPath -> a -> (s, Slicout)) -> s -> RevPath -> [(FileName, a)] -> (s, [Slicout])
mapCall fce available revpath =
  mapAccumL (\known (name, node) -> fce known (name:revpath) node) available

buildSlicout :: (MapOfHashes, MapOfHashes, MapOfHashes) ->  Differences ->  SliceName -> Slicout
buildSlicout (allSlicinHashes, sourceHashes, lastSlicinHashes) diff newSliceName =
  let
      makePaths :: Hash -> Paths
      makePaths hash =
        let find  set = maybe [] fst (M.lookup hash set)
        in Paths (find sourceHashes) (find lastSlicinHashes) (find allSlicinHashes)

      -- A target enters this map only when it exists in history or its data
      -- has been planned. Source hash candidates alone are not safe targets:
      -- a candidate's parent may itself be replaced by a directory link.
      historicalTargets = M.mapMaybe (listToMaybe . fst) allSlicinHashes

      linksToNewSlice (Link path _) = ("/" ++ newSliceName ++ "/") `isPrefixOf` path
      linksToNewSlice _ = False

      bFromLodree :: M.Map Hash FilePath -> RevPath -> Lodree -> (M.Map Hash FilePath, Slicout)
      bFromLodree available revpath@(name:_) lodree =
        let hash = hashLodree lodree
            paths = makePaths hash
            target = namesToPath revpath
        in
         case M.lookup hash available of
          Just path -> (available, File name $ Link path $ Info hash paths lodree)
          Nothing -> case lodree of
            LFile Ree{rsize, rtime} _ ->
              (M.insert hash target available, File name (Insert rsize rtime))
            LDir _ list ->
              let (known, children) = mapCall bFromLodree available revpath list
                  result = Dir name children
                  -- Even acyclic chains of directory links can exceed the
                  -- reader's fixed-point pass limit. Only publish directories
                  -- independent of links into this slice. Their aliases need
                  -- no further same-slice expansion; files still deduplicate
                  -- directly to physical Insert targets at any nesting depth.
                  published = if any linksToNewSlice result
                                then known
                                else M.insert hash target known
              in (published, result)

      bFromDifferences :: M.Map Hash FilePath -> RevPath -> Differences -> (M.Map Hash FilePath, Slicout)
      bFromDifferences available (name:_) (QLeft lodree) =
        let hash = hashLodree lodree
        in (available, File name $ Delete $ Info hash (makePaths hash) lodree)
      bFromDifferences available revpath (QRight lodree)  = bFromLodree available revpath lodree
      bFromDifferences available revpath (QBoth _ lodree) = bFromLodree available revpath lodree
      bFromDifferences available revpath@(name:_) (QDir list) =
        let (known, children) = mapCall bFromDifferences available revpath list
        in (known, Dir name children)

  in snd $ bFromDifferences historicalTargets [newSliceName] diff


createMapOfHashes3 :: (Lodree, Lodree) -> (MapOfHashes, MapOfHashes, MapOfHashes)
createMapOfHashes3  (rootSlicinLodree, sourceLodree) =
  (filterRoot $ createMapOfHashes rootSlicinLodree,
   createMapOfHashes sourceLodree,
   filterRoot $ createMapOfHashes (currentLodree rootSlicinLodree))
   where  -- we dont to have the root becouse root is not real an if backup is empty it has the same hash as zero files
      filterRoot :: MapOfHashes -> MapOfHashes
      filterRoot = M.filter ((/= [""]) . fst)
