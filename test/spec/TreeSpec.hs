module TreeSpec (spec) where

import Data.Maybe (isNothing)
import qualified Data.Map as M
import Test.Hspec
import Test.QuickCheck
import Syfil.Data.Differences
import Syfil.Data.Lodree
import Syfil.Process.TreeComparator
import Support

spec :: Spec
spec = do
  describe "Logical trees" $ do
    it "ignores empty directories" $
      hashLodree (makeLDir [("empty", emptyLodree)]) `shouldBe` hashLodree emptyLodree
    it "counts nested files and bytes" $ do
      let tree = makeLDir [("a", file "abc"), ("d", makeLDir [("b", file "12345")])]
      (rcount (ree tree), rsize (ree tree)) `shouldBe` (2, 8)
    it "finds Unicode paths and reports missing entries" $ do
      let tree = forest [("žluťoučký", file "abc")]
      fmap hashLodree (findLodreeNode "/demo/žluťoučký" tree) `shouldBe` Just (hashLodree $ file "abc")
      fmap hashLodree (findLodreeNode "/demo/missing" tree) `shouldBe` Nothing
    it "indexes every occurrence of duplicate content" $ do
      let tree = forest [("a", file "same"), ("b", file "same")]
      fmap fst (M.lookup (hashLodree $ file "same") (createMapOfHashes tree))
        `shouldBe` Just ["/demo/b", "/demo/a"]

  describe "Tree comparison" $ do
    it "reports no difference for identical generated snapshots" $
      property $ \values ->
        let tree = forest [(show i, file (show n)) | (i, n) <- zip [0 :: Int ..] (values :: [Int])]
        in isNothing (compareTrees tree tree)
    it "reports inserted file counts and bytes" $
      counts emptyLodree (forest [("a", file "abc")]) `shouldBe` Just ((0, 0), (1, 3))
    it "reports deleted file counts and bytes" $
      counts (forest [("a", file "abc")]) emptyLodree `shouldBe` Just ((1, 3), (0, 0))
    it "counts only changed content in a directory" $
      counts (forest [("a", file "old"), ("b", file "same")])
             (forest [("a", file "newer"), ("b", file "same")])
        `shouldBe` Just ((1, 3), (1, 5))
    it "detects file-to-directory and directory-to-file changes" $ do
      let f = file "abc"
          d = makeLDir [("child", file "12345")]
      counts f d `shouldBe` Just ((1, 3), (1, 5))
      counts d f `shouldBe` Just ((1, 5), (1, 3))
  where
    counts left right = diffCountAndSizes <$> compareTrees left right
