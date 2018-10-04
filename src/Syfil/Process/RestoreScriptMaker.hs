{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Syfil.Process.RestoreScriptMaker (
  makeRestoreScript,
  bashDefinition,
  cmdDefinition
)

where

import           Control.Monad
import           Data.Bifunctor
import           Data.List
import qualified Data.Map          as M
import qualified Data.Set          as S
import qualified Data.Text         as T
import           Text.Printf
import           Text.RawString.QQ

import           Syfil.Data.Lodree
import           Util.Lib


data Cmd = MkDir | CpFile FilePath (Maybe UTCTime) deriving (Show, Eq, Ord)

type ScriptDefinition = (String, (FilePath, Cmd) -> String, String)

cmdDefinition = (cmdRestoreScript1, toCmdCommand, cmdRestoreScript2)
bashDefinition = (bashRestoreScript, toShallCommand, "")

slashni = replaceBacklashesToSlashes . replaceVerticalToSlashes

makeRestoreScript :: ScriptDefinition -> M.Map FilePath UTCTime -> Lodree -> [String]
makeRestoreScript (script1, fce, script2) modificationTimes lodree =
    let cmdList =  S.toList . S.fromList $ (first namesToPath)  <$> (
         takeRestoreTuples lodree >>= (\ (rp, originalPath) ->
                (rp, CpFile originalPath (lookupModificationTime rp)) :
                zip (tails . tail $ rp) (repeat MkDir)
             ))
      in  lines  (replaceDvojDvoj 3 script1)
       ++  fmap (replaceVerticalToSlashes . fce) cmdList
       ++ lines  script2
  where
     lookupModificationTime :: RevPath -> Maybe UTCTime
     lookupModificationTime rp = (M.lookup (dropSlash . namesToPath $ rp) modificationTimes)
          --   let restoreTuples = (. bimap (slashni . namesToPath) slashni) <$> takeRestoreTuples lodreeBackupCurrent

toShallCommand :: (FilePath, Cmd) -> String
toShallCommand (path, MkDir)     = printf "ymkdir \"%s\""  (dropSlash path)
toShallCommand (path, CpFile op maybeTime) =
    printf "ycp \"%s\" \"%s\"" (dropSlash op)  (dropSlash path)
    ++ case maybeTime of
         Nothing -> ""
         Just time -> printf "\nymodtime \"%s\" \"%s\"" (dropSlash path) (show time)

toCmdCommand :: (FilePath, Cmd) -> String
toCmdCommand (path, MkDir)     = replaceSlashesToBacklashes $ printf "call :ymkdir %%ROOT%% \"%s\""  (dropSlash path)
toCmdCommand (path, CpFile op _) =
   replaceSlashesToBacklashes $ printf "call :ycp %%ROOT%% \"%s\" \"%s\"" (dropSlash op)  (dropSlash path)


dropSlash :: String -> String
dropSlash []        = []
dropSlash ('/' : x) = x
dropSlash x         = x

replaceDvojDvoj :: Int -> String -> String
replaceDvojDvoj n str = T.unpack (T.replace "DVOJDVOJTECKY" dvojdvoj (T.pack str))
  where dvojdvoj = T.pack (intercalate  "/" (replicate  n ".."))


bashRestoreScript = [r|#!/bin/bash

THIS_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null && pwd )"
SOURCE_DIR=$THIS_DIR/DVOJDVOJTECKY/data

TAD1=${1:?Specify target dir as first argument}
TARGET_DIR=${TAD1%/}   # no trailing slash

TRD1=${2:?Specify source soubtree as second argument. Write / if you want to restore all the forest.}
TRD2="/${TRD1#/}"
TREE_DIR="${TRD2%/}/"  # exactly one starting and one trailing slash

echo $TREE_DIR

ymkdir () {
  if [[ "/$1" = "$TREE_DIR"* ]]; then
    DIR=${1#"$TREE_DIR"}
    mkdir -p "$TARGET_DIR/$DIR"
  fi
}

ycp () {
 if [[ "/$2" = "$TREE_DIR"* ]]; then
    DFILE=${2#"$TREE_DIR"}
    TARGET_PATH="$TARGET_DIR/$DFILE"

    if [ -f "$TARGET_PATH" ]; then
      echo "allready exists: " "$TARGET_PATH"
    else
      echo "create: " "$TARGET_PATH"
      cp "$SOURCE_DIR/$1" "$TARGET_PATH"
    fi
   fi
}

ymodtime () {
  if [[ "/$1" = "$TREE_DIR"* ]]; then
    DIR=${1#"$TREE_DIR"}
    touch -d "$2" "$TARGET_DIR/$DIR"
  fi
}

|]

----------------------------------

cmdRestoreScript1 = [r|@echo off

IF NOT "%~2"=="" IF "%~3"=="" GOTO START
ECHO Run this script frm the directory, where it is
ECHO This script requires the next parameters:
ECHO - 1 . destination directory (NOT ends witch slash)
ECHO - 2. subfolder which to restore, should start witch treenmae and ends with slash
ECHO Allways use forward slashes
ECHO Examples:
ECHO "%~nx0" "C:/User/myname/restoredphotos photos/

GOTO :EOF

:START

set THISDIR=%~dp0
set SRCDIR=%THISDIR%DVOJDVOJTECKY\data\

set DEST=%~1
set ROOT=%~2

setlocal EnableExtensions EnableDelayedExpansion

|]

cmdRestoreScript2 = [r|@echo off

endlocal

exit /b 0

:ycp [%1 - root path;%2 - source path;%3 - destination path ]

set ZARAZKA=68e5g6s44d5f7re8778921s323x5ds4f74ss07e
set stringa=%ZARAZKA%%~3
set "modified=!stringa:%ZARAZKA%%~1=!"

IF NOT "%modified%"=="%ZARAZKA%%~3" (
       echo "%DEST%/%modified%"
       COPY "%SRCDIR%%~2" /B /V "%DEST%/%modified%"
    )

:ymkdir [%1 - root path;%2 - destination path ]

set ZARAZKA=68e5g6s44d5f7re8778921s323x5ds4f74ss07e
set stringa=%ZARAZKA%%~2
set "modified=!stringa:%ZARAZKA%%~1=!"

IF NOT "%modified%"=="%ZARAZKA%%~2" (
       mkdir "%DEST%/%modified%
    )



|]
