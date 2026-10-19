module DiskSpec (spec) where

import Control.Monad (forM_)
import qualified Data.ByteString as BS
import qualified Data.Map as M
import Data.Yaml (encodeFile, decodeFileThrow)
import System.Directory
import System.Directory.Tree (AnchoredDirTree((:/)), failures)
import System.FilePath ((</>), takeDirectory)
import System.IO.Temp (withSystemTempDirectory)
import Test.Hspec
import Syfil.App.Config (TreeDef(..))
import Syfil.Data.Lodree
import Syfil.Data.Slicin (Slicin)
import qualified Syfil.Data.Slicout as Output
import Syfil.IO.FileNamesC (modificationTimesFileName)
import Syfil.IO.SlicinScaner (readSlice)
import Syfil.IO.SlicoutWriter (writeBackup)
import Syfil.IO.SourceScaner (readSourceTree)
import Syfil.Process.SlicinMerger (mergeToLodree)
import Syfil.Process.SlicoutBuilder
import Syfil.Process.TreeComparator (compareTrees)
import Support (contents, file)
import Util.DirScan (Event(..), EventEnvelop(..))
import Util.Lib (ErrList(..), namesToPath, replaceVerticalToSlashes)

spec :: Spec
spec = describe "Backup on disk" $
  it "scans, writes, indexes and restores duplicate binary and empty files" $
    withSystemTempDirectory "syfil-test-" $ \root -> do
      let sourceDir = root </> "source"
          dataDir = root </> "data"
          sliceName = "2026-10|001"
          sliceDir = dataDir </> "2026-10" </> "001"
          quiet _ _ = return ()
          fixtures = [("A/binary.dat", BS.pack [0, 255, 13, 10, 128]),
                      ("B/binary.dat", BS.pack [0, 255, 13, 10, 128]),
                      ("empty.dat", BS.empty)]
          expectedTime = rtime (ree $ file "")
      forM_ fixtures $ \(relative, bytes) -> do
        let path = sourceDir </> relative
        createDirectoryIfMissing True (takeDirectory path)
        BS.writeFile path bytes
        setModificationTime path expectedTime
      (scanned, sourceErrors) <- readSourceTree quiet M.empty [] sourceDir
      getErrList sourceErrors `shouldBe` []
      source <- case scanned of
        Just tree -> return (makeLDir [("demo", tree)])
        Nothing -> expectationFailure "Source scan returned no tree" >> fail "No source"
      difference <- case compareTrees emptyLodree source of
        Just diff -> return diff
        Nothing -> expectationFailure "Source scan appears empty" >> fail "No difference"
      let plan = buildSlicout (createMapOfHashes3 (emptyLodree, source)) difference sliceName
      Output.countsToBackup plan `shouldBe` 2
      createDirectoryIfMissing True sliceDir
      encodeFile (sliceDir </> modificationTimesFileName) (Output.modificationTimes plan)
      (_ :/ written) <- writeBackup quiet (dataDir :/ plan) [TreeDef "demo" sourceDir []]
      length (failures written) `shouldBe` 0
      let onEvent (EventEnvelop _ _ event (ErrList errors)) = return $ ErrList $ case event of
            Failure err -> show err : errors
            _ -> errors
      (readBack, sliceErrors) <- readSlice (onEvent, ErrList []) (dataDir </> sliceName)
      getErrList sliceErrors `shouldBe` []
      slice <- case readBack of
        Just tree -> return tree
        Nothing -> expectationFailure "Slice scan returned no tree" >> fail "No slice"
      let index = root </> "index.yaml"
      encodeFile index slice
      indexed <- decodeFileThrow index :: IO Slicin
      indexed `shouldBe` slice
      let restored = currentLodree (mergeToLodree emptyLodree indexed)
      contents restored `shouldBe` contents source
      forM_ (takeRestoreTuples restored) $ \(logical, physical, time) -> do
        let relative = drop (length "/demo/") (namesToPath logical)
            destination = root </> "restored" </> relative
            stored = dataDir </> drop 1 (replaceVerticalToSlashes physical)
        time `shouldBe` expectedTime
        createDirectoryIfMissing True (takeDirectory destination)
        copyFile stored destination
        actual <- BS.readFile destination
        original <- BS.readFile (sourceDir </> relative)
        actual `shouldBe` original
