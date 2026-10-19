module RestoreSpec (spec) where

import Data.List (isPrefixOf, sort)
import qualified Data.Map as M
import Test.Hspec
import Syfil.Data.Lodree
import Syfil.Process.RestoreScriptMaker
import Support
import Util.Lib (replaceVerticalToSlashes)

spec :: Spec
spec = describe "Restore manifest" $ do
  it "creates no operations for an empty snapshot" $
    makeRestoreScript csvDefinition emptyLodree `shouldBe` []
  it "preserves logical names, physical slice paths and modification times" $ do
    let attributes = ree (file "payload")
        tree = forest [("žluťoučký soubor.txt",
          LFile attributes "/2026-10|001/demo/original.txt")]
    makeRestoreScript csvDefinition tree `shouldContain`
      ["FILE|demo/žluťoučký soubor.txt|2026-10/001/demo/original.txt|\"2026-10-19T00:00:00Z\""]
  it "creates each parent directory once" $ do
    let tree = forest [("nested", makeLDir
          [("a", LFile (ree $ file "a") "/slice/a"),
           ("b", LFile (ree $ file "b") "/slice/b")])]
    filter (isPrefixOf "DIR|") (makeRestoreScript csvDefinition tree)
      `shouldBe` ["DIR|||", "DIR|demo||", "DIR|demo/nested||"]
  it "includes every restored file and references existing physical data after deduplication" $ do
    let source = forest [("A", makeLDir [("x", file "same")]),
                         ("B", makeLDir [("x", file "same")])]
    (history, stored) <- checkSnapshot (emptyLodree, M.empty) (1, source)
    let rows = [split row | row <- makeRestoreScript csvDefinition (currentLodree history),
                           "FILE|" `isPrefixOf` row]
        physicalPaths = M.fromList [(replaceVerticalToSlashes path, ()) | path <- M.keys stored]
    sort [logical | (_:logical:_) <- rows] `shouldBe` ["demo/A/x", "demo/B/x"]
    mapM_ (\row -> case row of
      (_:_:physical:_) -> M.member ('/' : physical) physicalPaths `shouldBe` True
      _ -> expectationFailure ("Malformed manifest row: " ++ show row)) rows

split :: String -> [String]
split value = case break (== '|') value of
  (part, []) -> [part]
  (part, _:rest) -> part : split rest
