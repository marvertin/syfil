{-# LANGUAGE RecordWildCards #-}

module Syfil.IO.RestoreScriptWriter (
  createRestoreScripts
) where

import           Control.Monad
import           Data.Bifunctor
import           Data.List
import qualified Data.Map                        as M
import qualified Data.Set                        as S
import           Data.Yaml
import           System.Directory

import           Syfil.App.Context
import           Syfil.Data.Lodree
import           Syfil.IO.FileNamesC
import           Syfil.Process.RestoreScriptMaker
import           Text.Printf
import           Util.Lib

createRestoreScripts ::  Ctx -> Lodree -> IO Bool
createRestoreScripts ctx (LDir _ list) = orSequence $ map (createOneRestoreScript ctx) list

restoreFilesDefinition :: [(String, ScriptDefinition)] 
restoreFilesDefinition = [
    ("restore.sh", bashDefinition),
    ("restore.cmd", cmdDefinition),
    ("restore.csv", csvDefinition)
  ]

-- (M.Map FilePath UTCTime)
createOneRestoreScript :: Ctx -> (String, Lodree) -> IO Bool
createOneRestoreScript ctx@Ctx{..} (sliceName, lodree) = do
  orSequence $ map cretatePieceOfRestoreScript restoreFilesDefinition   

  where   
    cretatePieceOfRestoreScript :: (String, ScriptDefinition) -> IO Bool
    cretatePieceOfRestoreScript (filename, scriptDefinition) = do
        let generatedScriptFullpath = indexRoot ++ "/" ++ replaceVerticalToSlashes sliceName ++ "/" ++ filename
        mustCreate <- not <$> doesFileExist generatedScriptFullpath
        when (mustCreate) $ do
          writeFile generatedScriptFullpath (unlines $ makeRestoreScript scriptDefinition lodree)
        return mustCreate  

orSequence :: [IO Bool] -> IO Bool
orSequence list = fmap (foldr (||) False) (sequence list)

   