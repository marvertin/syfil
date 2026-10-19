"""Exercise deeply nested same-slice dependencies through the actual CLI.

Usage: python3 test/dedup/chain.py BINARY NEW_OUTPUT_DIRECTORY [DEPTH]
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
    work = Path(sys.argv[2]).resolve()
    depth = int(sys.argv[3]) if len(sys.argv) > 3 else 25
    work.mkdir(parents=True, exist_ok=False)
    source = work / "source"
    repo = work / "backup"
    source.mkdir()
    repo.mkdir()
    (repo / "syfil-config.yaml").write_text(
        "forest:\n  demo:\n    path: " + json.dumps(str(source)) + "\n")
    previous = source / "D100"
    previous.mkdir()
    (previous / "leaf.txt").write_bytes(b"base")
    for i in range(1, depth + 1):
        current = source / f"D{100 + i}"
        current.mkdir()
        shutil.copytree(previous, current / "child")
        (current / "own.txt").write_text(str(i))
        previous = current

    expected = files(source)
    exit_codes = []
    for n in [1, 2]:
        result = subprocess.run([str(binary), "--dir", str(repo), "backup",
                                 "+RTS", "-N2", "-RTS"],
                                stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
        (work / f"backup-{n}.log").write_bytes(result.stdout)
        exit_codes.append(result.returncode)
        if result.returncode:
            raise RuntimeError(result.stdout.decode(errors="replace"))

    manifest = next((repo / "index").glob("*/001/restore.csv"))
    restored = work / "restored"
    restored.mkdir()
    physical_missing = []
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
            physical_missing.append(physical)
    actual = files(restored)
    report = {
        "depth": depth,
        "backup_exit_codes": exit_codes,
        "expected_files": len(expected),
        "restored_files": len(actual),
        "missing": sorted(expected.keys() - actual.keys()),
        "extra": sorted(actual.keys() - expected.keys()),
        "different_content": sorted(p for p in expected.keys() & actual.keys()
                                    if expected[p] != actual[p]),
        "missing_physical_targets": physical_missing,
        "passed": expected == actual and not physical_missing,
    }
    (work / "result.json").write_text(json.dumps(report, indent=2) + "\n")
    print(json.dumps(report, indent=2))
    return 0 if report["passed"] else 1


if __name__ == "__main__":
    sys.exit(main())
