import Test.Hspec
import qualified BackupSpec
import qualified MetadataSpec
import qualified RestoreSpec
import qualified TreeSpec
import qualified HashCacheSpec
import qualified DiskSpec

main :: IO ()
main = hspec $ do
  HashCacheSpec.spec
  MetadataSpec.spec
  TreeSpec.spec
  BackupSpec.spec
  RestoreSpec.spec
  DiskSpec.spec
