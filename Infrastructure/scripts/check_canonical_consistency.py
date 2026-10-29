#!/usr/bin/env python3
"""Detect unintended duplication or divergence between canonical sources.

The repository sometimes maintains the same logical workflow under two
canonical paths (for example, `literature-intake` as both a command playbook
under `Infrastructure/commands/` and a reusable skill under
`Infrastructure/skills/`). This is intentional in that case but is a
maintenance hazard everywhere else: if two canonical files share a name and
no one has declared the duplication intentional, edits will drift apart.

This script:

1. Detects every name that appears as a canonical surface under more than
   one of (agents/, commands/, rules/, templates/, skills/, style/skills/).
2. Compares each detected pair against an explicit allow-list. Allowed
   pairs must declare the reason and the maintainer responsibility.
3. Fails when an unapproved duplication is found.

Use this whenever introducing a new canonical file. Adjust the allow-list
only after deciding the divergence is deliberate and documenting why.
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path
from typing import Dict, Iterable, List, Set, Tuple


# Pairs of (location_A, location_B) where A and B legitimately share the same
# canonical basename. Each entry is (sorted_tuple_of_locations, name, reason).
ALLOWED_DUPLICATIONS: Tuple[Tuple[Tuple[str, ...], str, str], ...] = (
    (
        ("commands", "skills"),
        "literature-intake",
        (
            "literature-intake exists as both a slash-command playbook "
            "(Infrastructure/commands/literature-intake.md) and as a "
            "reusable skill (Infrastructure/skills/literature-intake.md). "
            "The two bodies are intentionally different views of the same "
            "workflow. When editing one, review the other and keep their "
            "definitions of source priority, metadata, and workflow steps "
            "in lockstep even though the prose differs."
        ),
    ),
    (
        ("rules", "templates"),
        "constitutional-governance",
        (
            "constitutional-governance exists as both an empty project-local "
            "rule (Infrastructure/rules/constitutional-governance.md) and as "
            "a populated template (Infrastructure/templates/constitutional-governance.md) "
            "that the rule file is meant to be filled in from. The template "
            "tells contributors to copy it to the rules location and customize. "
            "Do not collapse these two files. When editing the template's "
            "structure (article numbering, examples), confirm the rule file "
            "either remains empty or has been updated by a project owner."
        ),
    ),
)


CANONICAL_LOCATIONS: Tuple[Tuple[str, str], ...] = (
    ("agents", "Infrastructure/agents"),
    ("commands", "Infrastructure/commands"),
    ("rules", "Infrastructure/rules"),
    ("templates", "Infrastructure/templates"),
    ("skills", "Infrastructure/skills"),
    ("style/skills", "Infrastructure/style/skills"),
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Check canonical-source duplication policy.")
    parser.add_argument(
        "--repo-root",
        default=None,
        help="Repository root path. Defaults to script-derived root.",
    )
    return parser.parse_args()


def discover_canonical_names(dir_path: Path) -> Set[str]:
    """Return canonical names (no extension) including structured skills."""
    if not dir_path.is_dir():
        return set()
    out: Set[str] = set()
    for entry in dir_path.iterdir():
        if entry.is_file() and entry.suffix == ".md" and entry.name != "README.md":
            out.add(entry.stem)
        elif entry.is_dir() and (entry / "SKILL.md").is_file():
            out.add(entry.name)
    return out


def main() -> int:
    args = parse_args()
    script_root = Path(__file__).resolve().parents[2]
    repo_root = Path(args.repo_root).resolve() if args.repo_root else script_root

    if not repo_root.exists():
        print(f"ERROR: repo root does not exist: {repo_root}", file=sys.stderr)
        return 1

    name_to_locations: Dict[str, List[str]] = {}
    total_canonical_files = 0
    for label, rel in CANONICAL_LOCATIONS:
        names = discover_canonical_names(repo_root / rel)
        total_canonical_files += len(names)
        for name in names:
            name_to_locations.setdefault(name, []).append(label)

    duplications: List[Tuple[str, Tuple[str, ...]]] = []
    for name, locations in name_to_locations.items():
        if len(locations) > 1:
            duplications.append((name, tuple(sorted(locations))))

    allowed_index = {(locs, name): reason for (locs, name, reason) in ALLOWED_DUPLICATIONS}

    failures: List[str] = []
    approved: List[str] = []

    for name, locations in sorted(duplications):
        key = (locations, name)
        if key in allowed_index:
            approved.append(f"{name} in {','.join(locations)}: ALLOWED")
        else:
            failures.append(
                f"Unapproved canonical duplication: '{name}' appears in "
                f"{', '.join(locations)}. "
                f"Either rename one of the files or add an entry to "
                f"ALLOWED_DUPLICATIONS in this script with the rationale."
            )

    if failures:
        print(
            f"Canonical consistency check failed: {len(failures)} unapproved duplication(s).",
            file=sys.stderr,
        )
        for f in failures:
            print(f"- {f}", file=sys.stderr)
        if approved:
            print(
                f"\n(Also detected {len(approved)} allowed duplication(s); these are not failures.)",
                file=sys.stderr,
            )
            for a in approved:
                print(f"- {a}", file=sys.stderr)
        return 1

    print(
        f"Canonical consistency check passed: {total_canonical_files} canonical file(s) scanned, "
        f"{len(approved)} allowed duplication(s) recorded."
    )
    for a in approved:
        print(f"- {a}")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
