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
createRestoreScripts ctx (LDir _ list) = foldr (createOnRestoreScript ctx) (return False) list

restoreFilesDefinition :: [(String, ScriptDefinition)] 
restoreFilesDefinition = [
    ("restore.sh", bashDefinition),
    ("restore.cmd", cmdDefinition),
    ("restore.csv", csvDefinition)
  ]

-- (M.Map FilePath UTCTime)
createOnRestoreScript :: Ctx -> (String, Lodree) -> IO Bool -> IO Bool
createOnRestoreScript ctx@Ctx{..} (sliceName, lodree) mustCreate = do

  print $  "BUDU-INDEXOVAT: " ++ sliceName
  let scriptFileNameBash = indexRoot ++ "/" ++ replaceVerticalToSlashes sliceName ++ "/restore.sh"
  let scriptFileNameCmd = indexRoot ++ "/" ++ replaceVerticalToSlashes sliceName ++ "/restore.cmd"
  let scriptFileNameCsv = indexRoot ++ "/" ++ replaceVerticalToSlashes sliceName ++ "/restore.csv"


  mustCreate <- (||) <$> mustCreate <*>
      (
         (||) <$> (not <$> doesFileExist scriptFileNameBash) 
              <*> (
                     (||) <$> (not <$> doesFileExist scriptFileNameCmd)
                          <*> (not <$> doesFileExist scriptFileNameCsv)
                  ) 
      )
  when (mustCreate) $ do
     writeFile scriptFileNameBash (unlines $ makeRestoreScript bashDefinition lodree)
     writeFile scriptFileNameCmd (unlines $ makeRestoreScript cmdDefinition lodree)
     writeFile scriptFileNameCsv (unlines $ makeRestoreScript csvDefinition lodree)
  return mustCreate

  where   
    cretatePieceOfRestoreScript :: (String, ScriptDefinition) -> IO Bool
    cretatePieceOfRestoreScript (filename, scriptDefinition) = do
        let generatedScriptFullpath = indexRoot ++ "/" ++ replaceVerticalToSlashes sliceName ++ "/" ++ filename
        mustCreate <- not <$> doesFileExist generatedScriptFullpath
        when (mustCreate) $ do
          writeFile generatedScriptFullpath (unlines $ makeRestoreScript scriptDefinition lodree)
        return mustCreate  
