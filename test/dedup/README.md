# Deduplication regression

Run the Hspec suite (including metadata and all six orders of `A`, `A 2`, `A 3`):

```sh
stack test
```

Run the real executable against a small, disposable fixture:

```sh
stack build syfil:exe:syfil
python3 test/dedup/reproduce.py \
  "$(stack path --local-install-root)/bin/syfil" \
  /tmp/syfil-dedup-example
```

The output directory must not exist. The script retains all source files,
backup slices, restored files, command output and `result.json` for inspection.
It never accesses a production backup.

The first backup stores one baseline file. The next backup adds three identical
directories containing two new contents and a copy of the baseline content.
A third scan makes Syfil generate the second slice's official `restore.csv`.
The script restores every file listed there and compares the complete set of
relative paths and SHA-256 hashes with the source (10 files).
It also retains `backup-through-002`, the exact backup state before that third
run. This matters because the old binary can itself repair data in a later slice.

The original 1.2.3 executable exits successfully on all three runs, but restores
only 4 of 10 files: the two new contents have no physical copies. The repaired
planner stores exactly two new physical files and restores all 10 files.

To test upgrading an existing broken fixture without deleting its indexes or
source hash cache, run:

```sh
python3 test/dedup/migrate.py \
  "$(stack path --local-install-root)/bin/syfil" \
  /path/to/broken-fixture /tmp/syfil-dedup-upgrade
```

This copies the source and `backup-through-002`, runs the newer binary on the existing backup, restores
the new snapshot and compares all paths and SHA-256 hashes. It also checks that
old slice data and source files remain unchanged, two new physical files were
added, and a repeat backup creates no further slice.

Deep dependency regression (26 overlapping directory trees, 351 files):

```sh
python3 test/dedup/chain.py \
  "$(stack path --local-install-root)/bin/syfil" /tmp/syfil-chain-example 25
```

The intermediate planner published directories containing same-slice links.
This produced acyclic chains exceeding the reader's 20-pass expansion limit:
only 336 of 351 paths appeared in the first snapshot's restore manifest.
The corrected planner publishes a new directory as a link target only when
it contains no links into the new slice. File deduplication still points to
physical insertions, and historical directory links remain available. This
bounds same-slice expansion independently of input depth, at the cost of
more directory/file metadata for some nested duplicate trees. The model tests
cover depths 21, 25 and 40 and assert the absence of link-to-link dependencies.
