{-# LANGUAGE TupleSections #-}

module Util.Treex.Dir (
  qq, ww
) where

import           Data.Foldable
import qualified Data.Map              as M
import           Data.Monoid
import qualified Data.Set              as S
import           System.Directory
import           System.FilePath.Posix

import           Util.Treex

type Ford a = Tree String a
type FwdPath = [String]
type RevPath = [String]

scanDir :: (RevPath -> FilePath -> IO a) -> FilePath  -> IO (Ford (Maybe a))
scanDir fce rootPath  = buildDownEitherM (\revPath -> do
      let pth = rootPath </> foldl (flip (</>)) [] revPath
      isdir <- doesDirectoryExist pth
      if isdir then Left . S.fromList <$> listDirectory pth
               else Right <$> fce revPath pth
    )

loadDir :: FilePath -> IO (Ford (Maybe (Sum Integer)))
loadDir = scanDir (\_ filePath -> Sum  <$> getFileSize filePath)



sumka :: M.Map String (Ford (Sum Integer)) -> (Sum Integer)
sumka mapa = let xx = M.elems mapa
             in foldMap fold xx


loadDir2 :: FilePath -> IO (Ford (Sum Integer))
loadDir2 filePath = buildDownPairM (fun2 filePath)

fun2 :: FilePath -> [String] -> IO (S.Set String, Sum Integer)
fun2 rootPath revPath = do
  let pth = rootPath </> foldl (flip (</>)) [] revPath
  isdir <- doesDirectoryExist pth
  if isdir then (, Sum 0) . S.fromList <$> listDirectory pth
           else (S.empty, ) . Sum  <$> getFileSize pth


sumka2 :: M.Map String (Ford (Sum Integer)) -> (Sum Integer)
sumka2 mapa = let xx = M.elems mapa
              in foldMap fold xx


qq = do
  -- tree <- loadDir "c:/vyvoj/syfil/src"
  tree <- loadDir "i:/zoje/syfil/src"
  let tree2 = fillNodes sumka tree
  let tree3 = fmap getSum tree2
  print tree3
  dump "" tree3

ww = do
  -- tree <- loadDir "c:/vyvoj/syfil/src"
  tree <- loadDir2 "i:/zoje/syfil/src"

  dump "" (getSum <$> tree)

dump :: String ->  Ford Integer -> IO ()
dump prefix (Tree cislo ch) = do
  putStrLn $ "   (" ++ show cislo ++ ")"
  forM_ (M.toList ch) (\ (k, tree) -> do
      putStr $ prefix ++ k
      dump (prefix ++ "    ") tree
    )
