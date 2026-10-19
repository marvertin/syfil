# Changelog for Syfil

## 1.2.4 - 2026-10-19

- Use Hspec and QuickCheck for a unified automated test suite; replace the old
  Debug-based entry point and retain metadata and deduplication regressions.
- Test comparison, storage counts, generated backup histories, restore manifests
  and the actual source-scan/write/read/index/restore pipeline on temporary files.

- Prevent cyclic deduplication links between new directories and their files.
- Choose link targets only from history or data already scheduled for storage.
- Avoid publishing directories with same-slice links, preventing long dependency
  chains from exceeding the restore reader's expansion limit.
- Test initial and incremental backups, directory order, renames and restoration.

## 1.2.3 - 2026-10-19

- Accept Windows CRLF line endings in current and legacy backup metadata on Unix.
- Fix a crash while creating indexes for backups originally written on Windows.
- Add regression tests for metadata parsing and YAML serialization.

## 1.0.0.0 - 2018-10-04

First distributed version. 
