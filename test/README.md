# Automated tests

Run the complete Hspec suite from the project root:

```sh
stack test
```

Stack installs the test-only dependencies Hspec, QuickCheck and temporary from
the configured snapshot. A failing example or property makes the command fail.
Hspec reports each example independently, so a failure does not hide subsequent
results. No production backup or configured source directory is used.

## Running selected tests

```sh
# One group (the match is a substring of the displayed test path)
stack test --test-arguments='--match "Backup planning"'

# More generated cases, with a fixed seed for reproducing the run
stack test --test-arguments='--qc-max-success=1000 --seed=12345'

# List examples without running them
stack test --test-arguments='--dry-run'
```

By default QuickCheck tries 100 generated cases per property. Failure output
contains a seed and rerun command. The backup-history generator also shrinks
failing histories by removing snapshots and entries. Generated histories keep
one baseline file and stable file/directory roles; they do not cover every
possible filesystem state or operation.

## Layout and coverage

`test/spec/Spec.hs` explicitly registers the spec modules:

| Module | What it checks |
| --- | --- |
| `MetadataSpec` | Current and legacy headers, LF/CRLF, optional final newline, invalid input, generated path round trips through text and YAML |
| `TreeSpec` | Empty directories, counts/sizes, Unicode paths, duplicate indexes, comparison of unchanged/added/deleted/modified nodes and type changes |
| `BackupSpec` | Physical copy counts, empty-file deduplication, renames, unchanged snapshots, modifications/deletions, generated histories, all six duplicate-directory orders and depths 21/25/40 |
| `RestoreSpec` | CSV logical/physical paths, timestamps, unique parent directories and usable physical targets after deduplication |
| `DiskSpec` | Real binary/empty source files scanned, written, read back, serialized to a YAML index and restored byte for byte; timestamps and copy counts |

`Support` supplies small in-memory fixtures and a model of the files physically
stored by a copy plan. Snapshot checks compare all logical paths, sizes and
hashes, then verify that every restore target exists in the physical file map
and has the expected content. They also reject chains of links within one slice.
Disk tests use a unique temporary directory cleaned up after success or failure.

The old `test/Spec.hs` depended on an absent `Debug` module. Its entry point and
the standalone metadata/deduplication runners are replaced by this unified suite.
To add coverage, create a module exporting `spec :: Spec` under `test/spec/`,
import it in `Spec.hs`, and add its `spec` to `main`.

## Full executable regression fixtures

The Python scripts in [dedup/README.md](dedup/README.md) additionally exercise
the command-line application and its official `restore.csv`. Run them manually
when changing the backup pipeline. They require Python 3 and a freshly built
executable, keep their output for inspection, and are not part of `stack test`.

Framework documentation: [Hspec](https://hspec.github.io/) and
[QuickCheck integration](https://hspec.github.io/quickcheck.html).
