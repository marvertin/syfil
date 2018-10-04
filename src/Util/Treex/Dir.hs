module Util.Treex.Dir (
  qq
) where

import           Data.Foldable
import qualified Data.Map              as M
import           Data.Monoid
import qualified Data.Set              as S
import           System.Directory
import           System.FilePath.Posix

import           Util.Treex


loadDir :: FilePath -> IO (Tree String (Maybe (Sum Integer)))
loadDir filePath = buildDownM (fun filePath)


fun :: FilePath -> [String] -> IO (Either (S.Set String) (Sum Integer))
fun rootPath revPath = do
  let pth = rootPath </> foldl (flip (</>)) [] revPath
  isdir <- doesDirectoryExist pth
  if isdir then Left . S.fromList <$> listDirectory pth
           else Right . Sum  <$> getFileSize pth

sumka :: (M.Map String (Tree String (Sum Integer)) -> (Sum Integer))
sumka mapa = let xx = M.elems mapa
                 yy = foldMap fold xx
             in foldMap fold xx

qq = do
  tree <- loadDir "c:/vyvoj/syfil/src"
  let tree2 = fillNodes sumka tree
  let tree3 = fmap getSum tree2
  print tree3
  dump "" tree3


dump :: String ->  Tree String Integer -> IO ()
dump prefix (Tree cislo ch) = do
  putStrLn $ "   (" ++ show cislo ++ ")"
  forM_ (M.toList ch) (\ (k, tree) -> do
      putStr $ prefix ++ k
      dump (prefix ++ "    ") tree
    )
