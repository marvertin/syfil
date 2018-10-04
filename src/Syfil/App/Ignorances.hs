module Syfil.App.Ignorances (

  makeFilterFce,
  IgnoranceDef,
  Expr2
) where

import           Control.Exception
import           Data.Foldable
import           Data.Maybe
import           Data.Monoid
import           Debug.Trace
import           System.FilePath
import           Text.Regex.Posix

import           Util.Lib


type IgnoranceDef = [String]

type Expr = String
type Expr1 = String
type Expr2 = String

data MyException = BadFirstCharEception Char | BadSecondCharException Char

instance Exception MyException
instance Show MyException
  where
     show (BadFirstCharEception x) = "first character is '"  ++ [x] ++ "', should be 'e' or 'i'"
     show (BadSecondCharException x) = "second character is '"  ++ [x] ++ "', should be '=' or '~'"

makeFilterFce :: [Expr2] -> (RevPath -> Bool)
makeFilterFce exprlist revpath =
    let path = (replaceBacklashesToSlashes . pth) revpath
        fces = (neg <$> exprlist) ++ [const $ Just True]
    in fromJust $ getFirst (foldMap First (fmap ($  path) fces))


neg :: Expr2 -> (FilePath -> Maybe Bool)
neg ('i': expr1) = justTrue $ check expr1
neg ('e': expr1) = justFalse $ check expr1
neg (x: _)       = throw $ BadFirstCharEception x
neg _=           throw $ BadFirstCharEception ' '

check :: Expr1 -> (FilePath -> Bool)
check ('=': expr) path = checkEquality expr path
check ('~': expr) path = checkRegexp expr path
check (x: _) _         = throw $  BadSecondCharException x
check _ _              = throw $  BadSecondCharException ' '
--check expr _           = error ("Bat pattern in filter: " ++ expr)
-- check _ _ = True

checkEquality :: Expr -> FilePath -> Bool
checkEquality = (==)


checkRegexp :: Expr -> FilePath -> Bool
checkRegexp regexp path = path =~ wrapregexp regexp

wrapregexp :: String -> String
wrapregexp r = "^" ++ r ++ "$"


justTrue :: (a -> Bool) -> (a -> Maybe Bool)
justTrue fce x = if fce x then Just True else Nothing

justFalse :: (a -> Bool) -> (a -> Maybe Bool)
justFalse fce x = if fce x then Just False else Nothing
