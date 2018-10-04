{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NamedFieldPuns       #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Syfil.Process.SlicoutBuilder (
  buildSlicout,
  createMapOfHashes3,
) where


import           Control.Arrow
import           Data.List                   (mapAccumL)
import qualified Data.Map                    as M
import           Data.Maybe
import           Debug.Trace
import           Debug.Trace
import           System.Directory.Tree

import           Syfil.Data.Differences
import           Syfil.Data.Lodree
import           Syfil.Data.Slicout
import           Syfil.Process.TreeComparator
import           Util.Lib

mapCall :: (RevPath -> a -> Slicout ) -> RevPath -> [(FileName, a)] -> [Slicout]
mapCall fce revpath = map (uncurry fce . (first (:revpath)))

buildSlicout :: (MapOfHashes, MapOfHashes, MapOfHashes) ->  Differences ->  SliceName -> Slicout
buildSlicout (allSlicinHashes, sourceHashes, lastSlicinHashes) diff newSliceName =
  let
      makePaths :: Hash -> Paths
      makePaths hash =
        let find  set = maybe [] fst (M.lookup hash set)
        in Paths (find sourceHashes) (find lastSlicinHashes) (find allSlicinHashes)

      bFromLodree :: RevPath -> Lodree -> Slicout
      bFromLodree revpath@(name:_) lodree =
        let hash = hashLodree lodree
        in
         case M.lookup hash allSlicinHashes of
          Nothing  -> let paths = makePaths hash
                      in  case mustInsert revpath paths of
                            Nothing ->
                              case lodree of
                                LFile Ree{rsize, rtime} _   ->  File name (Insert rsize rtime)
                                LDir _ list -> Dir name (mapCall bFromLodree revpath list)  -- Dir name (map (uncurry bFromLodree) list)
                            Just pathToLink -> File name $ Link ("/" ++ newSliceName ++ pathToLink) $ Info hash paths lodree
          Just (path: _, lodree2) -> File name $ Link path $ Info hash (makePaths hash) lodree2

      bFromDifferences :: RevPath -> Differences -> Slicout
      bFromDifferences (name:_) (QLeft lodree)   = let hash = hashLodree lodree in File name $ Delete $ Info hash (makePaths hash) lodree
      bFromDifferences revpath (QRight lodree)  = bFromLodree revpath lodree
      bFromDifferences revpath (QBoth _ lodree) = bFromLodree revpath lodree
      bFromDifferences revpath@(name:_) (QDir list) = Dir name (mapCall bFromDifferences  revpath list)

  in  bFromDifferences [newSliceName] diff


createMapOfHashes3 :: (Lodree, Lodree) -> (MapOfHashes, MapOfHashes, MapOfHashes)
createMapOfHashes3  (rootSlicinLodree, sourceLodree) =
  (filterRoot $ createMapOfHashes rootSlicinLodree,
   createMapOfHashes sourceLodree,
   filterRoot $ createMapOfHashes (currentLodree rootSlicinLodree))
   where  -- we dont to have the root becouse root is not real an if backup is empty it has the same hash as zero files
      filterRoot :: MapOfHashes -> MapOfHashes
      filterRoot = M.filter ((/= [""]) . fst)


mustInsert :: RevPath -> Paths -> Maybe FilePath
mustInsert _ (Paths [] _ _) = Nothing -- impossible
mustInsert revpath (Paths (itMustInsert:_) _ _) =
    let path = namesToPath . init $ revpath
    in if path == itMustInsert then Nothing
                               else Just itMustInsert
