
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns        #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TupleSections         #-}

module Syfil.App.Config
    (
      readConfig,
      ForestDef,
      TreeDef(..),
      Cfg(..),
      checkSliceNamePattern,
    ) where

import           Control.Exception
import           Control.Monad
import           Control.Monad.Trans.Maybe
import           Data.Bifunctor
import           Data.List.Unique
import qualified Data.Map                  as M
import           Data.Maybe
import           Data.Yaml
import           GHC.Generics
import           System.Directory
import           System.FilePath
import           System.IO
import           Text.Printf
import           Text.Regex.Posix

import           Syfil.App.Ignorances
import           Syfil.IO.FileNamesC
import           Syfil.IO.SliceNameStrategy
import           Util.Lib

data TreeDef = TreeDef { tdName :: String, tdPath :: FilePath, tdPatterns :: IgnoranceDef} deriving (Show)
type ForestDef = [TreeDef]


data CfgTree = CfgTree { path :: FilePath, filter :: Maybe [String]} deriving (Show, Generic, FromJSON )
data Config = Config {
     sliceNameStrategy :: Maybe SliceNameStrategy,
     forest            :: M.Map String CfgTree
  }  deriving (Show, Generic, FromJSON )

data Cfg = Cfg {
     sliceNameStrategy' :: SliceNameStrategy,
     forestDef          :: ForestDef,
     emptyTrees         :: [String]
   } deriving (Show)

pickForestDef :: Config -> ForestDef
pickForestDef = map (\(treename, CfgTree{..}) -> TreeDef treename path (fromMaybe [] filter) )
  . M.toList . forest



readConfig :: FilePath -> IO (Maybe Cfg)
readConfig backupDir = do
  runMaybeT $ do
    let configFilePath = backupDir </> configFileName
    config <- MaybeT $ readConfigFile configFilePath
    let forestDef = pickForestDef config
    _ <- MaybeT $ checkAllIngorancePatterns forestDef
    _ <- MaybeT $ checkDuplicities forestDef
    _ <- MaybeT $ checkSliceNamePatternInCfg config
    empties <- MaybeT $ checkSourceDirs forestDef
    return $ Cfg (fromMaybe  defaultSliceNameStrategy (sliceNameStrategy config)) forestDef empties




readConfigFile :: FilePath -> IO (Maybe Config)
readConfigFile configFilePath =
  printf "Reading config \"%s\"\n" configFilePath >>
  decodeFileWithWarnings configFilePath >>= (\x -> case x of
      Left e -> do
        hPutStrLn stderr $ displayException e
        return  Nothing
      Right (warns, config) -> do
        if null warns then return $ Just config
                      else do
                         print warns
                         return Nothing
  )

checkSliceNamePatternInCfg :: Config -> IO (Maybe ())
checkSliceNamePatternInCfg Config{sliceNameStrategy=Nothing} = return $ Just()
checkSliceNamePatternInCfg Config{sliceNameStrategy=(Just strategy)} =
  if checkSliceNamePattern strategy then  return $ Just()
     else do
       hPutStrLn stderr $ "!!! Bad sliceNameStrategy pattern: " ++ show (strategy)
       return Nothing


checkDuplicities :: ForestDef -> IO (Maybe ())
checkDuplicities forest = do
   let reps = repeated $ tdPath <$> forest
   if null reps then return $ Just ()
                else printf "There are duplicit paths: %s\n" (show reps) >> return Nothing

checkAllIngorancePatterns :: ForestDef -> IO (Maybe [()])
checkAllIngorancePatterns list = do
  let list2 = (\TreeDef{..} -> (tdName, tdPatterns)) <$> list
  let list3 = list2 >>=  (\(treename, patterns) -> map (treename,) patterns)
  sequence <$> (mapM checkIngorancePatterns list3)

checkIngorancePatterns :: (String, String) -> IO (Maybe ())
checkIngorancePatterns (tree, pattern) =
   catch (do
            evaluate $ makeFilterFce [pattern] ["text"]
            return $ Just ()
         )
     (\e -> do
       hPrintf stderr "!!! Bad pattern \"%s\" for \"%s\": %s\n" pattern tree (displayException (e :: SomeException))
       return  Nothing
     )


checkSourceDirs :: ForestDef -> IO (Maybe [String])
checkSourceDirs forest = do
    printf "Checking direcotories of %d tress\n" (length forest)
    list <- forM forest (\( TreeDef treename path patterns) -> do
        checkResult <- checkSourceDir patterns path
        printf "    %-20s" treename
        case checkResult of
          Left e -> printf "      - %s\n" (displayException e) >> return (treename, 0)
          Right (filtered, total) -> printf "%2d/%2d - %s\n" filtered total path >> return (treename, filtered)
      )
    let empties = fst <$> Prelude.filter ((==0) . snd) list
    let anies = sum (snd <$> list) > 0
    if anies then do
               when (not . null $ empties) (
                 hPrintf stderr "!!! WARNING - Thees trees are unaccessible or contains no data: %s\n" (show empties)
                )
               return $ Just empties
             else do
               hPutStrLn stderr "!!! ERROR - Nothing to backup, all sources are unaccessible or contains no data."
               return Nothing


checkSourceDir :: [Expr2] -> FilePath -> IO (Either IOException (Int, Int))
checkSourceDir exprs path = try $ do
    list <- listDirectory path
    return (length $ Prelude.filter (makeFilterFce exprs . (:[])) list, length list)
