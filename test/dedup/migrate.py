"""Test repairing an existing reproduce.py fixture using a newer executable.

Usage: python3 test/dedup/migrate.py NEW_BINARY OLD_FIXTURE NEW_OUTPUT_DIRECTORY
Copies the fixture first; the original broken backup is retained unchanged.
"""

import json
from pathlib import Path
import shutil
import subprocess
import sys

sys.dont_write_bytecode = True
from reproduce import files


def main():
    binary = Path(sys.argv[1]).resolve()
    original = Path(sys.argv[2]).resolve()
    work = Path(sys.argv[3]).resolve()
    before = json.loads((original / "result.json").read_text())
    assert not before["passed"] and before["missing"], "Expected a broken fixture"
    work.mkdir(parents=True, exist_ok=False)
    shutil.copytree(original / "source", work / "source")
    shutil.copytree(original / "backup-through-002", work / "backup")
    source = work / "source"
    repo = work / "backup"
    (repo / "syfil-config.yaml").write_text(
        "forest:\n  demo:\n    path: " + json.dumps(str(source)) + "\n")
    expected = files(source)
    old_data = files(repo / "data")
    old_slices = sorted(str(p.relative_to(repo / "data"))
                        for p in (repo / "data").glob("*/*") if p.is_dir())
    # Retain the old indexes and cache: this tests a normal upgrade, not a rescan.
    assert (repo / "index" / "1_demo.yaml").is_file()

    def backup(label):
        result = subprocess.run([str(binary), "--dir", str(repo), "backup",
                                 "+RTS", "-N2", "-RTS"],
                                stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        (work / f"upgrade-{label}.log").write_bytes(result.stdout)
        if result.returncode:
            raise RuntimeError(result.stdout.decode(errors="replace"))

    backup("repair")
    slices = sorted(str(p.relative_to(repo / "data"))
                    for p in (repo / "data").glob("*/*") if p.is_dir())
    added = sorted(set(slices) - set(old_slices))
    assert len(added) == 1, f"Expected one new slice, got {added}"
    # Generate the official restore.csv for the repaired snapshot. This run
    # should find no differences and therefore create no further data slice.
    backup("verify")
    manifest = repo / "index" / added[0] / "restore.csv"
    restored = work / "restored-upgrade"
    restored.mkdir()
    missing_targets = []
    for row in manifest.read_text().splitlines():
        kind, logical, physical, *_ = row.split("|")
        if kind != "FILE":
            continue
        target = restored / Path(logical).relative_to("demo")
        target.parent.mkdir(parents=True, exist_ok=True)
        stored = repo / "data" / physical
        if stored.is_file():
            shutil.copyfile(stored, target)
        else:
            missing_targets.append(physical)

    actual = files(restored)
    after_data = files(repo / "data")
    preserved = all(after_data.get(p) == digest for p, digest in old_data.items())
    final_slices = sorted(str(p.relative_to(repo / "data"))
                          for p in (repo / "data").glob("*/*") if p.is_dir())
    physical_files = [p for p in (repo / "data" / added[0]).rglob("*")
                      if p.is_file() and not p.name.endswith((".syfil", ".yaml"))]
    report = {
        "before_restored_files": before["restored_files"],
        "expected_files": len(expected),
        "restored_files": len(actual),
        "missing": sorted(expected.keys() - actual.keys()),
        "extra": sorted(actual.keys() - expected.keys()),
        "different_content": sorted(p for p in expected.keys() & actual.keys()
                                    if expected[p] != actual[p]),
        "missing_physical_targets": missing_targets,
        "new_slice": added[0],
        "new_physical_files": len(physical_files),
        "old_slice_data_unchanged": preserved,
        "verification_created_no_slice": slices == final_slices,
        "source_unchanged": files(source) == expected,
        "manifest": str(manifest),
    }
    report["passed"] = (expected == actual and not missing_targets and preserved
                        and slices == final_slices and files(source) == expected
                        and len(physical_files) == 2)
    (work / "upgrade-result.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report, indent=2))
    return 0 if report["passed"] else 1


if __name__ == "__main__":
    sys.exit(main())
