module BackupSpec (spec) where

import Control.Monad (foldM, forM_)
import Data.List (permutations)
import qualified Data.Map as M
import Data.Maybe (fromJust, isNothing)
import Test.Hspec hiding (before, after)
import Test.QuickCheck
import Syfil.Data.Lodree
import qualified Syfil.Data.Slicout as Output
import Support

spec :: Spec
spec = do
  describe "Deduplication regressions" $ do
    forM_ [21, 25, 40 :: Int] $ \depth ->
      it ("restores overlapping directory trees at depth " ++ show depth) $ do
        let levels = iterate (\tree -> makeLDir
              [("child", tree), ("own.txt", file (show $ rcount $ ree tree))])
              (makeLDir [("leaf.txt", file "base")])
            source = forest [("D" ++ show (100 + i), tree) | (i, tree) <- zip [0..depth] levels]
        _ <- checkSnapshot (emptyLodree, M.empty) (1, source)
        return ()
    forM_ (permutations ["A", "A 2", "A 3"]) $ \order ->
      it ("restores initial and incremental duplicates in order " ++ show order) $ do
        let original = file "historical content"
            baseline = makeLDir [("demo", makeLDir [("baseline.txt", original)])]
            folder = makeLDir
              [("a.txt", file "new content one"),
               ("nested", makeLDir [("b.txt", file "new content two")]),
               ("historical.txt", original)]
            snapshot names extra = makeLDir [("demo", makeLDir
              ([("baseline.txt", original)] ++ [(name, folder) | name <- names] ++ extra))]
            added = snapshot order []
            renamed = snapshot ["renamed", "renamed 2"] [("changed.txt", file "changed")]
            -- Historical data remains usable after removing its original names.
            reused = snapshot ["returned"] [("changed.txt", file "changed again")]
        _ <- foldM checkSnapshot (emptyLodree, M.empty)
          (zip [1..] [baseline, added, added, renamed, reused])
        -- Also cover duplicates in the very first backup, without any history.
        _ <- checkSnapshot (emptyLodree, M.empty) (1, added)
        return ()
  describe "Backup planning" $ do
    it "stores identical new file content only once, including empty files" $ do
      let source = forest [("a", file "same"), ("b", file "same"),
                           ("zero1", file ""), ("zero2", file "")]
          plan = fromJust (planBetween emptyLodree source)
      Output.countsToBackup plan `shouldBe` 2
      Output.sizeToBackup plan `shouldBe` 4
      _ <- checkSnapshot (emptyLodree, M.empty) (1, source)
      return ()
    it "does not create a plan for an unchanged snapshot" $ do
      let source = forest [("a", file "same")]
      (history, _) <- checkSnapshot (emptyLodree, M.empty) (1, source)
      isNothing (planBetween history source) `shouldBe` True
    it "reuses historical data after renaming a file" $ do
      let before = forest [("old", file "same")]
          after = forest [("new", file "same")]
      state@(history, _) <- checkSnapshot (emptyLodree, M.empty) (1, before)
      Output.countsToBackup (fromJust $ planBetween history after) `shouldBe` 0
      _ <- checkSnapshot state (2, after)
      return ()
    it "stores changed content and retains unchanged files" $ do
      let before = forest [("a", file "old"), ("b", file "same")]
          after = forest [("a", file "new"), ("b", file "same")]
      state@(history, _) <- checkSnapshot (emptyLodree, M.empty) (1, before)
      Output.countsToBackup (fromJust $ planBetween history after) `shouldBe` 1
      _ <- checkSnapshot state (2, after)
      return ()
    it "removes deleted files without copying other content" $ do
      let before = forest [("a", file "removed"), ("b", file "kept")]
          after = forest [("b", file "kept")]
      state@(history, _) <- checkSnapshot (emptyLodree, M.empty) (1, before)
      Output.countsToBackup (fromJust $ planBetween history after) `shouldBe` 0
      _ <- checkSnapshot state (2, after)
      return ()
    it "restores generated snapshot histories with duplicates, additions, changes and deletions" $
      property $ forAllShrink snapshotHistory shrinkHistory $ \snapshots -> ioProperty $ do
        _ <- foldM checkSnapshot (emptyLodree, M.empty)
          (zip [1..] (map snapshotTree snapshots))
        return True

-- Slot names are stable across snapshots; contents use a small pool to exercise
-- deduplication frequently. A permanent file keeps these histories nonempty.
-- Shrinking removes snapshots or entries to give a small reproducible failure.
snapshotHistory :: Gen [[(Int, Int)]]
snapshotHistory = do
  count <- chooseInt (1, 5)
  vectorOf count (resize 16 $ listOf ((,) <$> chooseInt (0, 15) <*> chooseInt (0, 3)))

shrinkHistory :: [[(Int, Int)]] -> [[[(Int, Int)]]]
shrinkHistory = shrinkList (shrinkList (const []))

snapshotTree :: [(Int, Int)] -> Lodree
snapshotTree entries = forest $ ("baseline", file "historical") :
  [("dir" ++ show group, makeLDir
      [("file" ++ show slot, file (show content))
      | (slot, content) <- M.toList unique, slot `div` 4 == group])
  | group <- [0..3 :: Int]]
  where unique = M.fromList entries
