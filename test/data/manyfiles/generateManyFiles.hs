
import           System.Directory
import           System.FilePath

toStrList = map (tail . show . (+10000))

writeFileBundle dir numberList = do
  createDirectoryIfMissing True dir
  mapM (\x -> writeFile  (dir </> x) x) $ toStrList numberList

main = do
  writeFileBundle  "backup/0000-many-files.slice/root" [0..4999]
  writeFileBundle  "source-of-root" [5000..9999]
