module Syfil.IO.FileNamesC (
  metaSuffix,
  configFileName,
  sliceIndexName,
  sliceLogicalTree_suffix,
  modificationTimesFileName,
  indexSubdir,
  dataSubdir,
  logSubdir,
  sliceLogName,
  mainLogName,
  indexVersion,

)
where

import           System.Directory.Tree (FileName)
--import           System.FilePath (FileName)

metaSuffix = ".syfil"

configFileName = "syfil-config.yaml" :: FileName

sliceIndexName = indexVersion ++ "_sliceIndex.yaml"
sliceLogicalTree_suffix = "_logical-tree.yaml"

sliceLogName = "slice-backup.log"
mainLogName = "summary.log"
indexSubdir = "index"
dataSubdir = "data"
logSubdir = "log"
modificationTimesFileName = "modification-times.yaml"

indexVersion = "1"
