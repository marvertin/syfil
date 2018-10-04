{-# LANGUAGE DeriveAnyClass      #-}
{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Util.Tree (
  Tree(..),
  Finfo(..)
) where

import           Control.Arrow         (first)
import qualified Data.ByteString.Char8 as B8
import qualified Data.HashMap.Strict   as HM
import qualified Data.Map              as M
import           Data.Text
import           Data.Yaml
import           GHC.Generics

data Tree a = MFile a | MDir (M.Map String (Tree a)) deriving (Eq, Show, Generic)
data Finfo = Finfo { fisize :: Integer, fihash :: String } deriving (Eq, Show, Generic)


instance (ToJSON a) => ToJSON (Tree a) where
  -- toJSON (Finfo x y) = object ["x" .= x, "y" .= y]
  toJSON (MFile x) = toJSON x
  toJSON (MDir x)  = toJSON x

instance (FromJSON a) => FromJSON (Tree a) where
  parseJSON :: Value -> Parser (Tree a)
  parseJSON q@(String text)   = MFile <$> parseJSON q
  parseJSON q@(Object object) = (MDir . mapConvert) <$> mapM parseJSON object
   where
    mapConvert :: HM.HashMap Text v -> M.Map String v
    mapConvert = M.fromList . fmap (first unpack) . HM.toList

instance ToJSON Finfo where
  -- toJSON (Finfo x y) = object ["x" .= x, "y" .= y]
  toJSON (Finfo fisize fihash) = let val = show fisize ++ " " ++ fihash
     in toJSON val

instance FromJSON Finfo where
    parseJSON =  withText "chuj" (return . parseFinfo)
      where
        parseFinfo :: Text -> Finfo
        parseFinfo x = let (s:h:_) = unpack <$> Data.Text.words x
                 in Finfo (read s) h
