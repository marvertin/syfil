# syfil
Safe Your FILes - program to backup computer data

Run automated tests with `stack test`. See [test/README.md](test/README.md) for
test groups, reproducible generated cases and full command-line regression tests.

### Resumable source hash cache

Source hashes are journalled in `index/2_<tree-name>.hash-cache.jsonl`.
This format is independent of the existing version 1 slice indexes. Each line
contains a checksummed JSON payload; the last valid file record for a path wins.
The header identifies the canonical source path, schema version and SHA-1
algorithm. Source directories and configured filters are still scanned on every
run; entries in the cache never introduce files into a backup.

A matching file size and modification time permit reuse of a hash. This is the
same optimization as before, not independent verification of file contents.
New hashes are appended only after the file's size and timestamp have been
checked again after reading. Completed records survive an interrupted scan.
The journal is flushed after each record and synchronized at least every 1,000
new hashes or 5 seconds, checked between files. An interrupted current file or
unsynchronized recent records may have to be read again. Scan logs report the
number of reused and newly calculated hashes.

The old `index/1_<tree-name>.yaml` is imported when there is no usable new cache
and retained unchanged for compatibility. A scan without source errors compacts
the cache to current files. It writes and validates a `.new` snapshot with a
completion footer before deleting the old cache and renaming the replacement.
On restart, a complete replacement is used if the main journal is missing or
corrupt; an intact main journal takes precedence because it can contain newer
appends. Incomplete replacements and damaged journals are retained under
`.rejected-*` and `.damaged-*` diagnostic names. A partial final line is discarded
before further appends. Invalid records are reported and never used.

An operating-system file lock covers loading, recovery, appending and compaction
and is released automatically after process termination. Another writer is
rejected. Cache I/O errors disable persistent caching with a visible warning;
they do not substitute old hashes for failed source reads or stop backup copying.
On Unix, files and replacement directory changes are synchronized with `fsync`.
The Windows build currently flushes handles but cannot promise power-loss
durability and reports that limitation. No cache design can override a device
that falsely reports durable writes. Loss of this disposable cache requires
rehashing; the cache is not evidence that a backup is complete.
