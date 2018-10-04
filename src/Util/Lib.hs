{-# LANGUAGE DeriveAnyClass       #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE RecordWildCards      #-}
{-# LANGUAGE StandaloneDeriving   #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Util.Lib
    (
     Dumpable(..),
     ErrList(..),
     ErrMsg,
     FileName,
     FilePath,
     FilesCount,
     FileSize,
     HasFileName(..),
     Hash,
     Hexable(..),
     MonoidPlus2x2(..),
     MonoidPlus3(..),
     RevPath,
     SliceName,
     Stat3,
     Stat3Compute(..),
     UTCTime,
     appendToFirst,
     computeFileHash,
     createDirectories,
     deAnchore,
     dirTreeToStringList,
     dropPrefixSlashes,
     iterate2,
     mapTree,
     namesToPath,
     prependToFirst,
     pth,
     replaceBacklashesToSlashes,
     replaceItem,
     replaceSlashesToBacklashes,
     replaceSlashesToVertical,
     replaceVerticalToSlashes,
     safeHead,
     showDiffTm,
     showSz,
     splitByChar,
     trim,
     tupleMaybeUpFst,
     tupleMaybeUpSnd,
     zipMaybe,
     zipPaths',
    ) where

import           Control.Arrow
import           Control.Exception
import           Control.Monad
import qualified Crypto.Hash.SHA1      as Cr
import qualified Data.ByteString       as SBS
import qualified Data.ByteString.Lazy  as Lazy
import qualified Data.ByteString.UTF8  as BSU
import           Data.Char
import           Data.Either
import           Data.Function
import           Data.List             (intercalate, nub, sortBy)
import           Data.List.Unique
import           Data.Maybe
import qualified Data.Text             as T
import           Data.Time.Clock
import           Data.Yaml
import           GHC.Generics
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           Text.Printf           (printf)



-- | size of file
type FileSize = Integer

-- | count of files
type FilesCount = Int

-- | it is reverse list of path items: ["myfile.txt", "myaccount", "home", "opt"]
type RevPath = [String]

type Hash = SBS.ByteString

type ErrMsg = String

-- | slcienamei of the form "2008-02|12|156" It contains vertical lines, not slashes.
type SliceName = String

-- | List of error messages
newtype ErrList = ErrList { getErrList :: [String] } deriving (Show)


instance ToJSON Hash where
  toJSON hash = toJSON (show hash)

instance FromJSON Hash where
  parseJSON = withText "chuj2" (return . parseee)
    where
      parseee :: T.Text -> Hash
      parseee x = (read (T.unpack x) :: Hash)



class  HasFileName a  where
  fileNamex :: a -> FileName


instance HasFileName (DirTree a) where
  fileNamex (Dir name _)    = name
  fileNamex (File name _)   = name
  fileNamex (Failed name _) = name



-- (Num a, Num b, Num c) =>
data  MonoidPlus3 a b c = MonoidPlus3 (a, b, c) deriving (Show)

instance (Num a, Num b, Num c) => Monoid (MonoidPlus3 a b c) where
  mempty = MonoidPlus3 (0, 0, 0)
  mappend (MonoidPlus3 (x1, y1, z1)) (MonoidPlus3 (x2, y2, z2)) = MonoidPlus3 (x1 + x2, y1 + y2, z1 + z2)

data  MonoidPlus2x2 a b c d = MonoidPlus2x2 ((a, b), (c, d))

instance (Num a, Num b, Num c, Num d) => Monoid (MonoidPlus2x2 a b c d) where
  mempty = MonoidPlus2x2 ((0, 0), (0, 0))
  mappend (MonoidPlus2x2 ((w1, x1), (y1, z1))) (MonoidPlus2x2 ((w2, x2), (y2, z2))) = MonoidPlus2x2 ((w1 + w2, x1 + x2), (y1 + y2, z1 + z2))


{- | Dump to lines for debug purpures.
-}
class Dumpable a where
  toDump :: a -> [String]

  toDumpS :: a -> String
  toDumpS = unlines . toDump

  dump :: a -> IO ()
  dump = putStrLn . toDumpS

  dumpToFile :: FilePath -> a -> IO ()
  dumpToFile fp = writeFile fp . toDumpS

class  Hexable a  where
  toHexStr :: a -> String

instance Hexable SBS.ByteString where
  toHexStr bytes = SBS.unpack bytes >>= printf "%02x"

dropPrefixSlashes :: FilePath -> FilePath
dropPrefixSlashes []         = []
dropPrefixSlashes ('/' : x)  = dropPrefixSlashes x
dropPrefixSlashes ('\\' : x) = dropPrefixSlashes x
dropPrefixSlashes x          = x

tupleMaybeUpFst :: (Maybe a, b) -> Maybe (a, b)
tupleMaybeUpFst (Nothing, _) = Nothing
tupleMaybeUpFst (Just a, b)  = Just (a, b)

tupleMaybeUpSnd :: (a, Maybe b) -> Maybe (a, b)
tupleMaybeUpSnd (_, Nothing) = Nothing
tupleMaybeUpSnd (a, Just b)  = Just (a, b)


replaceBacklashesToSlashes :: String -> String
replaceBacklashesToSlashes = let
      repl '\\' = '/'
      repl x    = x
  in map repl

replaceSlashesToBacklashes :: String -> String
replaceSlashesToBacklashes = let
      repl '/' = '\\'
      repl x   = x
  in map repl


replaceVerticalToSlashes :: String -> String
replaceVerticalToSlashes = let
      repl '|' = '/'
      repl x   = x
  in map repl

replaceSlashesToVertical :: String -> String
replaceSlashesToVertical = let
        repl '/'  = '|'
        repl '\\' = '|'
        repl x    = x
    in map repl

-- | Prepends list to the first list of list of list
-- | IF list of list is empty return singleton list of first list
prependToFirst :: [a] -> [[a]] -> [[a]]
prependToFirst [] []     = []
prependToFirst x []      = [x]
prependToFirst x (y: ys) = (x ++ y) : ys

-- | Appends list to the first list of list of list
-- | IF list of list is empty return singleton list of first list
appendToFirst :: [a] -> [[a]] -> [[a]]
appendToFirst [] []     = []
appendToFirst x []      = [x]
appendToFirst x (y: ys) = (y ++ x) : ys

-- | O(n (ln n)) Takes two functios which creates Ordable keys from two Util.Types
-- | Then takes two lists whic zips by result of the two functions
-- | The result is sorted by the key
-- | zipMaybe (*3) length [1, 2] ["qwer", "abcABC"] = [(3,Just 1,Nothing),(4,Nothing,Just "qwer"),(6,Just 2,Just "abcABC")]
zipMaybe :: Ord k => (a -> k) -> (b -> k) -> [a] -> [b] -> [(k, Maybe a, Maybe b)]
zipMaybe af bf al bl = merge (sortBy (compare `on` af) al)
                             (sortBy (compare `on` bf) bl)
    where
     -- merge :: [a] -> [b] -> [(k, Maybe a, Maybe b)]
     merge [] [] = []
     merge (a: as) [] = (af a, Just a, Nothing) :  merge as []
     merge [] (b: bs) = (bf b, Nothing, Just b) :  merge [] bs
     merge aq@(a: as) bq@(b: bs) = let
           ak = af a
           bk = bf b
        in case ak `compare` bk of
          LT -> (ak, Just a, Nothing) : merge as bq
          GT -> (bk, Nothing, Just b) : merge aq bs
          EQ -> (ak, Just a, Just b) : merge as bs

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

safeHead :: a -> [a] -> a
safeHead def [] = def
safeHead _ l    = head l

-- | Create directory witch parent directories but only if not exists
createDirectories :: FilePath -> IO ()
createDirectories dir = do
   createDirectoryIfMissing True (takeDirectory dir)
   createDirectory dir

replaceItem :: Int -> a -> [a] ->[a]
replaceItem index item list =
    let (pref, suf) = splitAt index list
    in if null suf then list
                   else pref ++ [item] ++ tail suf

-- | Iterate maximum iteration while the subsequence iteration are not equals
-- | by function fceEq
iterate2 :: Int -> (a -> a -> Bool) -> (a -> a) -> a -> a
iterate2 maxIterCount fceEq fceIter value
  | maxIterCount <= 0 = value
  | otherwise =
     let value' = fceIter value
     in if value `fceEq` value' then value'
                                else iterate2 (maxIterCount - 1) fceEq fceIter value'


mapTree :: ([FileName] -> a -> b) -> DirTree a -> DirTree b
mapTree  =
  let
    mapa :: [FileName] -> ([FileName] -> a -> b) -> DirTree a -> DirTree b
    mapa path fun (File name x)   = File name (fun (name:path) x)
    mapa path fun (Dir name list) = Dir name $  map (mapa (name:path) fun) list
  in mapa []

namesToPath :: [FileName] -> FilePath
namesToPath list = "/" ++ intercalate "/" (reverse list)

deAnchore :: AnchoredDirTree a -> DirTree a
deAnchore (_ :/ dirtree) = dirtree

removeFirstChar :: Char -> String -> String
removeFirstChar _ [] = []
removeFirstChar c q@(x: xs)
  | x == c = xs
  | otherwise = q

splitByChar :: Char -> String -> (String, String)
splitByChar z xs = let (s1, s2) = span (/=z) xs
                   in (trim s1, trim . removeFirstChar z $ s2)


-- | Compute hask of the file on filesystem
computeFileHash :: FilePath -> IO SBS.ByteString
computeFileHash = (fmap Cr.hashlazy . Lazy.readFile)  >=> evaluate

sizeInMb :: FileSize -> Double
sizeInMb x =  fromIntegral x / 1024 / 1024

showSz ::  Integral a  => a -> String
showSz sz = let (x, m) = safeHead (0, "XiB") . dropWhile (\(q, _) ->  abs q > 1024.0)
                         $ zip (qs (fromIntegral sz)) ["B  ", "KiB", "MiB", "GiB", "TiB", "PiB"]
            in printf "%7.3f %s" (x :: Double) m
  where qs size = size : qs (size / 1024.0)


showDiffTm :: UTCTime -> UTCTime -> String
showDiffTm endTime startTime = show (diffUTCTime endTime startTime)

zipPaths' :: DirTree a -> DirTree (FilePath, a)
zipPaths' (Dir _ list) = let dir = Dir "" list
                         in (first replaceBacklashesToSlashes) <$> zipPaths ("" :/ dir)

-- | convert reverse path to forward path not starting with slash
-- | pth ["jaba", "home", "opt"] == opt/home/jaba
pth :: RevPath -> FilePath
pth = foldl (flip (</>)) []



dirTreeToStringList :: Show a =>  (a -> Maybe String) -> DirTree a -> [String]
dirTreeToStringList f (File name a) = [name ++ maybe "" (": " ++) (f a) ]
dirTreeToStringList f (Dir name contents) = ("/ " ++ name) : map ("   "++) (concat (dirTreeToStringList f <$> contents))
dirTreeToStringList f (Failed name exc) = [name ++ "EXC " ++ show exc]


type Stat3 = MonoidPlus3 FilesCount FileSize FilesCount

instance  {-# OVERLAPS #-}  Show Stat3 where
  show (MonoidPlus3 (files, size, metas)) = printf "%6d files, %-8s, %5d metas" files (showSz size) metas

class Stat3Compute a where
  computeSizes :: a -> Stat3
