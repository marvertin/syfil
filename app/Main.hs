{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections  #-}

module Main where

import           Control.Monad
import           Control.Monad
import           Data.Either
import           Data.Maybe
import           Data.Semigroup                ((<>))
import           Data.Time.Clock
import           Data.Version                  (showVersion)
import           GHC.IO.Encoding
import           Options.Applicative
import qualified Paths_syfil                   (version)
import           System.Directory
import           System.Directory.Tree
import           System.Environment
import qualified System.Environment.Executable as SEE
import           System.Exit
import           System.FilePath.Find
import           System.IO
import           System.TimeIt
import           Text.Printf

import           Syfil.App.Consts
import           Syfil.App.Context
import           Syfil.Command.Backup
import           Syfil.Command.MarkDuplicities
import           Syfil.IO.FileNamesC
import           Util.Types

data Options =
  NormalCommand   { dir    :: String
              , optCommand :: Command
              }
  | Version
  | NoArgs
  deriving Show

-- putStrLn "→"

data WhichSlice
  = JustSlice String
  | LastSlice
  | PrevSlice
   deriving (Show)

data Command
  = Backup
  | Restore {
        whichSlice :: WhichSlice,
        destDir    :: String,
        fromDir    :: String,     -- relative dir
        checkDest  :: Bool
      }
  | MarkDuplicities {
        markedDir :: String
      }
 deriving (Show)


programInstallDir = "This value direct to use " ++ pgmname ++ " install directory"

backupOptions :: Parser Command
backupOptions = pure Backup


whichSliceOpt :: Parser WhichSlice
whichSliceOpt =
        JustSlice <$> strOption
            ( long "slice"
           <> metavar "SLICE-NAME"
           <> help "get this slice")
    <|> flag' LastSlice
            ( long "last-slice"
           <> help "get the last slice backed up")
    <|> flag' PrevSlice
            ( long "prev-slice"
           <> help "get the previouse slice backed up")


restoreOptions :: Parser Command
restoreOptions = Restore
    <$> whichSliceOpt
    <*> strOption
        ( long "dest-dir"
       <> metavar "DEST-DIR"
       <> help "Destination directory for restoring data. It must exists and must be empty."
        )
    <*> strOption
        ( long "from-dir"
       <> metavar "FROM-DIR"
       <> help "Subdirectory which you want to restore. (Default is the whole forest.)"
       <> value ""
        )
    <*> switch
        ( long "check-dest"
       <> help "Not to check existence and emptiness of the dest directory."
        )

markDuplicitiesOptions = MarkDuplicities
    <$> strOption
        ( long "marked-dir"
       <> metavar "MARKED-DIR"
       <> help "Directory in which the duplicities will be marked by renaming files allready backed up."
        )


normalCommand :: FilePath -> Parser Options
normalCommand defaultDir = NormalCommand
      <$> strOption
          ( long "dir"
         <> short 'd'
         <> metavar "BACKUP-DIR"
         <> help ("Directory with backup, must contains \"" ++ configFileName ++ "\" file. (default: directory where the executable of " ++ pgmname ++ " program is)")
         <> value defaultDir
         <> showDefault
            )
--      <*> switch
--          ( long "version"
--         <> help "Display version" )
      <*> hsubparser
          (  command "backup" (info backupOptions ( progDesc "Backup your system" ))
          <> command "restore" (info restoreOptions ( progDesc "Restore backed up files" ))
          <> command "mark-duplicities" (info markDuplicitiesOptions ( progDesc "Find duplicities in another directory and mark them" ))
          )

options :: FilePath -> Parser Options
options defaultDir =
         normalCommand defaultDir
     <|> flag' Version
             ( long "version"
            <> help "Display version")--      <*> switch
     <|> pure NoArgs
--          ( long "version"
--         <> help "Display version" )

main = do
  setLocaleEncoding utf8
  putStrLn $ pgmname ++ "  " ++ showVersion Paths_syfil.version ++ " - " ++ pgmshortdesc
  exitCode <- timeIt  main'
  exitWith exitCode

main' :: IO ExitCode
main' = do
   (pathWithProgram, _) <- SEE.splitExecutablePath
   let opts = info ((options pathWithProgram) <**> helper)
          ( fullDesc
         <> progDesc "backing up structure without change and duplicity"
         <> header (pgmname ++ pgmshortdesc))

   doProcessing =<< execParser opts


doProcessing :: Options -> IO ExitCode

doProcessing Version = do
  putStrLn $ pgmname ++ " " ++ showVersion Paths_syfil.version ++ " - safe your files"
  return ExitSuccess

doProcessing NoArgs = do
  (pathWithProgram, _) <- SEE.splitExecutablePath
  isHereConfig <- doesFileExist (pathWithProgram ++ "/" ++ configFileName)
  if isHereConfig then do
    exitCode <- withContext pathWithProgram cmdBackup
    isTerm <- hIsTerminalDevice stdout
    when (isTerm) $ do
      putStrLn "\nPress ENTER to continue."
      getLine >> return ()
    return exitCode
  else do
    putStrLn "No config no args .... kecy"
    return ExitSuccess

doProcessing NormalCommand{dir = enteredBackupDir, optCommand} = do
  (pathWithProgram, _) <- SEE.splitExecutablePath
  backupDirAbs <- makeAbsolute $
     if enteredBackupDir == programInstallDir then pathWithProgram
                                              else enteredBackupDir
  case optCommand of
    Backup -> withContext backupDirAbs cmdBackup
    (MarkDuplicities dir) -> withContext backupDirAbs (cmdMarkDuplicities dir)
    _ -> do
      hPutStrLn stderr $ "NOT IMPLEMENTED: " ++ (show optCommand)
      return $ ExitFailure 10
