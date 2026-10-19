module Support where

import           Control.Monad (forM_)
import           Test.Hspec (shouldBe, expectationFailure)
import qualified Crypto.Hash.SHA1 as SHA1
import qualified Data.ByteString.Char8 as BS
import           Data.List (isPrefixOf)
import qualified Data.Map as M
import           Data.Time.Calendar (fromGregorian)
import           Data.Time.Clock (UTCTime(..))
import           System.Directory.Tree (DirTree(Dir, File, Failed))

import           Syfil.Data.Lodree
import qualified Syfil.Data.Slicin as Input
import qualified Syfil.Data.Slicout as Output
import           Syfil.Process.SlicinMerger (mergeToLodree)
import           Syfil.Process.SlicoutBuilder
import           Syfil.Process.TreeComparator (compareTrees)
import           Util.Lib (namesToPath)

file :: String -> Lodree
file content = LFile (Ree 1 (fromIntegral $ BS.length bytes) time (SHA1.hash bytes)) ""
  where
    bytes = BS.pack content
    time = UTCTime (fromGregorian 2026 10 19) 0

-- Turn the actual copy plan into stored files and metadata without disk I/O.
-- Insert must have a corresponding source file; links do not create data.
materialize :: Lodree -> Output.Slicout -> Input.Slicin
materialize source = walk []
  where
    walk parents (Dir name children) = Dir name (map (walk (name:parents)) children)
    walk parents (File name command) = File name $ case command of
      Output.Insert _ _ ->
        let path = name:parents
        in case findLodreeNode (namesToPath $ init path) source of
          Just (LFile attributes _) -> Input.RegularFile attributes (namesToPath path)
          _ -> error "Insert has no source file"
      Output.Link target _ -> Input.MetaFile (Input.Link target)
      Output.Delete _ -> Input.MetaFile Input.Delete
    walk _ (Failed _ _) = error "Unexpected failure in copy plan"

contents :: Lodree -> M.Map FilePath (Integer, BS.ByteString)
contents tree = M.fromList
  [(namesToPath path, (rsize attributes, rhash attributes))
  | (path, attributes) <- flattenFileLodree tree]

checkSnapshot :: (Lodree, M.Map FilePath Ree) -> (Int, Lodree) -> IO (Lodree, M.Map FilePath Ree)
checkSnapshot (history, stored) (number, source) = do
  let slice = "2026-10|00" ++ show number
  case compareTrees (currentLodree history) source of
    Nothing -> do
      contents (currentLodree history) `shouldBe` contents source
      return (history, stored)
    Just difference -> do
      let plan = buildSlicout (createMapOfHashes3 (history, source)) difference slice
          written = materialize source plan
          regularFiles (File _ (Input.RegularFile attributes path)) = [(path, attributes)]
          regularFiles (Dir _ children) = concatMap regularFiles children
          regularFiles _ = []
          available = M.union (M.fromList $ regularFiles written) stored
          merged = mergeToLodree history written
          restored = currentLodree merged
          nodes parents node@(Dir name children) =
            (namesToPath (name:parents), node) : concatMap (nodes (name:parents)) children
          nodes parents node@(File name _) = [(namesToPath (name:parents), node)]
          nodes _ (Failed _ _) = []
          nodeIndex = M.fromList (nodes [] written)
          linksHere (Input.MetaFile (Input.Link target)) = ("/" ++ slice ++ "/") `isPrefixOf` target
          linksHere _ = False
      -- New links must not depend on another new link: this invariant bounds
      -- expansion depth regardless of the number or nesting of source folders.
      forM_ (foldMap (:[]) written) $ \entry -> case entry of
        Input.MetaFile (Input.Link target) | linksHere entry ->
          case M.lookup target nodeIndex of
            Just node -> any linksHere node `shouldBe` False
            Nothing -> expectationFailure ("Missing planned link target " ++ target)
        _ -> return ()
      contents restored `shouldBe` contents source
      forM_ (takeRestoreTuples restored) $ \(path, target, _) ->
        case M.lookup target available of
          Just attributes -> M.lookup (namesToPath path) (contents source)
            `shouldBe` Just (rsize attributes, rhash attributes)
          Nothing -> expectationFailure ("Restore points to missing physical data: " ++ target)
      return (merged, available)


forest :: [(String, Lodree)] -> Lodree
forest entries = makeLDir [("demo", makeLDir entries)]

planBetween :: Lodree -> Lodree -> Maybe Output.Slicout
planBetween history source = fmap
  (\difference -> buildSlicout (createMapOfHashes3 (history, source)) difference "2026-10|999")
  (compareTrees (currentLodree history) source)
