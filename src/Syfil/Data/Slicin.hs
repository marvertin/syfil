{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs               #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}

module Syfil.Data.Slicin
    (
    formatMetaFileHeader,
    parseMetaFile,
    SliceFile(..),
    Slicin(..),
    SliceCmd(..),
    AnchoredSlicin
    ) where

import           Control.Exception
import           Control.Monad
import qualified Crypto.Hash.SHA1      as Cr
import qualified Data.ByteString       as Strict
import qualified Data.ByteString.Lazy  as Lazy
import qualified Data.ByteString.UTF8  as BSU
import qualified Data.HashMap.Strict   as HM
import           Data.List
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Text             hiding (lines, unlines)
import qualified Data.Text             as T
import qualified Data.Text.IO          as TIO
import           Data.Yaml
import           GHC.Generics
import           GHC.IO.Encoding
import           System.Directory
import           System.Directory.Tree
import           System.FilePath
import           System.IO             (hFlush, stdout)
import           Text.Printf           (printf)

import           Syfil.Data.Ree
import           Util.Lib


type Slicin = DirTree SliceFile
type AnchoredSlicin = AnchoredDirTree SliceFile


data SliceFile = RegularFile Ree FilePath
              |  MetaFile SliceCmd  -- file content
              deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)

data SliceCmd = Delete | Link FilePath
  deriving (Eq, Show, Read, Generic, ToJSON, FromJSON)


parseMetaFile :: String -> SliceCmd
parseMetaFile fileContent = (parse . lines) fileContent
  where
    parse :: [String] -> SliceCmd
    parse []                   = error "meta file is empty"
    parse ("#version1" : line : _) = read line
    parse ("#yaba1" : line : _) = read line -- compatibility with old version
    parse _ = error $ "Bad version of meta file, probably old version of this tool: " ++ show fileContent

formatMetaFileHeader :: SliceCmd -> [String]
formatMetaFileHeader x = ["#version1", show x]

isSliceRegularFile :: Slicin -> Bool
isSliceRegularFile (File _ (RegularFile _ _)) = True
isSliceRegularFile _                          = False

isSliceMetaFile :: Slicin -> Bool
isSliceMetaFile (File _ (MetaFile _)) = True
isSliceMetaFile _                     = False


printSliceFile :: SliceFile ->  Maybe String
printSliceFile (RegularFile Ree{..} originalPath) = Just$ "  #" ++ show rsize ++ " " ++ toHexStr rhash ++ " \"" ++ originalPath ++ "\""
printSliceFile (MetaFile sliceCmd) = Just$  show sliceCmd

printSliceFile2 :: SliceFile -> Maybe String
printSliceFile2 (RegularFile _ originalPath) = Just originalPath
printSliceFile2 (MetaFile _)                 = Nothing

instance Stat3Compute Slicin where
  computeSizes :: Slicin -> Stat3
  computeSizes slicin = foldMap sizis slicin
    where
       sizis (RegularFile ree _) = computeSizes ree
       sizis MetaFile{}          = MonoidPlus3 (0, 0, 1)
--------------------------------------------------------



instance Dumpable Slicin where
  toDump = dirTreeToStringList printSliceFile

deriving instance Generic Slicin
deriving instance ToJSON Slicin
deriving instance FromJSON Slicin


-- deriving instance Generic IOException
-- deriving instance ToJSON IOException
-- deriving instance FromJSON IOException

instance ToJSON IOException where
     toJSON _ = String "IO exception, it will not be parsed"

instance FromJSON IOException where
     parseJSON :: Value -> Parser IOException
     parseJSON _ = error "Imposible has happend, exception wants to be parsed!"
     -- parseJSON q@(String text)   = (File "blb" . read) <$> parseJSON q


--}


  --------------------------------------------------------
  --
  -- Instances for YAML

{-
instance ToJSON Slicin where
     toJSON (File _ x)   = toJSON (show x)
     toJSON (Dir _ list) = toJSON $ M.fromList (tupl <$> list)
       where
        tupl q@(File name _) = (name, q)
        tupl q@(Dir name _)  = (name, q)

instance FromJSON Slicin where
  parseJSON :: Value -> Parser Slicin
  parseJSON q@(String text)   = (File "blb" . read) <$> parseJSON q
  parseJSON q@(Object object) = (Dir "blbe" . mapConvert) <$> mapM parseJSON object
   where
    mapConvert :: HM.HashMap Text Slicin -> [Slicin]
    mapConvert  =  fmap snd . HM.toList


instance ToJSON SliceFile where
  toJSON (RegularFile Ree{..}) = toJSON $ show rsize ++ " " ++ toHexStr rhash
  toJSON (MetaFile x)          = toJSON x

-}
