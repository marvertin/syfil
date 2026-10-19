"""Run a real backup and restore a tiny deduplication fixture from restore.csv.

Usage: python3 test/dedup/reproduce.py BINARY NEW_OUTPUT_DIRECTORY
The output directory is retained, including source, backup, restored files and logs.
"""

import hashlib
import json
from pathlib import Path
import shutil
import subprocess
import sys


def files(root):
    return {p.relative_to(root).as_posix(): hashlib.sha256(p.read_bytes()).hexdigest()
            for p in root.rglob("*") if p.is_file()}


def main():
    binary = Path(sys.argv[1]).resolve()
    work = Path(sys.argv[2]).resolve()
    work.mkdir(parents=True, exist_ok=False)
    source = work / "source"
    repo = work / "backup"
    source.mkdir()
    repo.mkdir()
    (repo / "syfil-config.yaml").write_text(
        "forest:\n  demo:\n    path: " + json.dumps(str(source)) + "\n")

    def backup(n):
        result = subprocess.run([str(binary), "--dir", str(repo), "backup",
                                 "+RTS", "-N2", "-RTS"],
                                stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        (work / f"backup-{n}.log").write_bytes(result.stdout)
        if result.returncode:
            raise RuntimeError(f"Backup {n} failed: {result.stdout.decode(errors='replace')}")

    # Establish a historical slice, then introduce three identical directories.
    (source / "baseline.txt").write_bytes(b"Already backed up.\n")
    backup(1)
    for name in ["A", "A 2", "A 3"]:
        folder = source / name
        (folder / "nested").mkdir(parents=True)
        (folder / "a.txt").write_bytes(b"New content one.\n")
        (folder / "nested" / "b.txt").write_bytes(b"New content two.\n")
        (folder / "historical.txt").write_bytes(b"Already backed up.\n")
    backup(2)
    # Retain the exact backup state before the scan that generates restore.csv.
    # On the buggy binary that scan may itself repair data in a later slice.
    shutil.copytree(repo, work / "backup-through-002")
    # The next scan generates the previous slice's official restore manifest.
    backup(3)
    manifests = sorted((repo / "index").glob("*/*/restore.csv"))
    manifest = next(p for p in manifests if p.parent.name == "002")
    restored = work / "restored"
    restored.mkdir()
    physical_missing = []
    for row in manifest.read_text().splitlines():
        kind, logical, physical, *_ = row.split("|")
        if kind != "FILE":
            continue
        relative = Path(logical).relative_to("demo")
        target = restored / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        stored = repo / "data" / physical
        if stored.is_file():
            shutil.copyfile(stored, target)
        else:
            physical_missing.append(physical)

    expected = files(source)
    actual = files(restored)
    new_slice = next((repo / "data").glob("*/002"))
    physical_files = [p for p in new_slice.rglob("*") if p.is_file()
                      and not p.name.endswith((".syfil", ".yaml"))]
    report = {
        "backup_exit_codes": [0, 0, 0],
        "expected_files": len(expected),
        "restored_files": len(actual),
        "missing": sorted(expected.keys() - actual.keys()),
        "extra": sorted(actual.keys() - expected.keys()),
        "different_content": sorted(p for p in expected.keys() & actual.keys()
                                    if expected[p] != actual[p]),
        "missing_physical_targets": physical_missing,
        "new_physical_files": len(physical_files),
        "manifest": str(manifest),
    }
    report["passed"] = expected == actual and not physical_missing
    (work / "result.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report, indent=2))
    return 0 if report["passed"] else 1


if __name__ == "__main__":
    sys.exit(main())
