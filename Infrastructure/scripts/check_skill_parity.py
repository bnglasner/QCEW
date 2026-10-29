#!/usr/bin/env python3
"""Assert canonical Infrastructure surfaces are mirrored in every adapter.

This complements `manage_generated_copies.sh check`. The sync script catches
content drift between canonical and generated trees. This script catches
*structural* parity gaps that the diff-based check could miss, especially
when contributors add content under nonstandard layouts. Specifically:

1. Every canonical agent under Infrastructure/agents/ must appear as both
   `.claude/agents/<name>.md` and `.codex/agents/<name>.toml` (Codex native
   wrapper) and `.codex/agents/<name>.md` (parity mirror).
2. Every canonical command under Infrastructure/commands/ must appear under
   `.claude/commands/` and `.codex/commands/`. Commands without a dedicated
   skill (canonical Infrastructure/skills/<name>.md or .../<name>/SKILL.md)
   must additionally appear as a SKILL bundle under `.codex/skills/` and
   `.agents/skills/`.
3. Every canonical skill (flat or structured, under Infrastructure/skills/
   or Infrastructure/style/skills/) must appear under each of
   `.claude/skills/<name>/SKILL.md`, `.codex/skills/<name>/SKILL.md`, and
   `.agents/skills/<name>/SKILL.md`.
4. No adapter file may correspond to a canonical name that no longer exists
   (orphan detection).

Exit code 0 on success, 1 on any structural mismatch.
"""

from __future__ import annotations

import argparse
import sys
from pathlib import Path
from typing import Iterable, List, Set, Tuple


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Check Infrastructure-to-adapter structural parity.")
    parser.add_argument(
        "--repo-root",
        default=None,
        help="Repository root path. Defaults to script-derived root.",
    )
    return parser.parse_args()


def discover_canonical_basenames(dir_path: Path) -> Set[str]:
    """Return canonical basenames (no extension, no README) under `dir_path`."""
    if not dir_path.is_dir():
        return set()
    out: Set[str] = set()
    for entry in dir_path.iterdir():
        if entry.is_file() and entry.suffix == ".md" and entry.name != "README.md":
            out.add(entry.stem)
    return out


def discover_canonical_skill_names(dir_path: Path) -> Set[str]:
    """Return canonical skill names under `dir_path` for both flat and structured layouts."""
    if not dir_path.is_dir():
        return set()
    out: Set[str] = set()
    for entry in dir_path.iterdir():
        if entry.is_file() and entry.suffix == ".md" and entry.name != "README.md":
            out.add(entry.stem)
        elif entry.is_dir() and (entry / "SKILL.md").is_file():
            out.add(entry.name)
    return out


def discover_adapter_skill_names(skills_dir: Path) -> Set[str]:
    """Return adapter skill names: subdirectories of `skills/` containing SKILL.md."""
    if not skills_dir.is_dir():
        return set()
    out: Set[str] = set()
    for entry in skills_dir.iterdir():
        if entry.is_dir() and (entry / "SKILL.md").is_file():
            out.add(entry.name)
    return out


def main() -> int:
    args = parse_args()
    script_root = Path(__file__).resolve().parents[2]
    repo_root = Path(args.repo_root).resolve() if args.repo_root else script_root

    if not repo_root.exists():
        print(f"ERROR: repo root does not exist: {repo_root}", file=sys.stderr)
        return 1

    infra = repo_root / "Infrastructure"
    failures: List[str] = []
    checked_summary: List[Tuple[str, int]] = []

    # --- 1. Agent parity --------------------------------------------------
    canonical_agents = discover_canonical_basenames(infra / "agents")
    checked_summary.append(("Canonical agents", len(canonical_agents)))

    for agent in sorted(canonical_agents):
        for required in (
            repo_root / ".claude" / "agents" / f"{agent}.md",
            repo_root / ".codex" / "agents" / f"{agent}.md",
            repo_root / ".codex" / "agents" / f"{agent}.toml",
        ):
            if not required.exists():
                failures.append(
                    f"Missing adapter file for canonical agent '{agent}': "
                    f"{required.relative_to(repo_root)}"
                )

    # Orphan adapter agents: present in adapter, missing from canonical.
    for adapter_dir in (repo_root / ".claude" / "agents", repo_root / ".codex" / "agents"):
        if not adapter_dir.is_dir():
            continue
        for entry in adapter_dir.iterdir():
            if entry.is_file() and entry.suffix in {".md", ".toml"}:
                stem = entry.stem
                if stem not in canonical_agents:
                    failures.append(
                        f"Orphan adapter agent file (no canonical source): "
                        f"{entry.relative_to(repo_root)}"
                    )

    # --- 2. Command parity ------------------------------------------------
    canonical_commands = discover_canonical_basenames(infra / "commands")
    checked_summary.append(("Canonical commands", len(canonical_commands)))

    canonical_skills_flat = discover_canonical_skill_names(infra / "skills")
    canonical_skills_style = discover_canonical_skill_names(infra / "style" / "skills")
    canonical_dedicated_skill_names = canonical_skills_flat | canonical_skills_style

    for cmd in sorted(canonical_commands):
        for required in (
            repo_root / ".claude" / "commands" / f"{cmd}.md",
            repo_root / ".codex" / "commands" / f"{cmd}.md",
        ):
            if not required.exists():
                failures.append(
                    f"Missing adapter file for canonical command '{cmd}': "
                    f"{required.relative_to(repo_root)}"
                )
        # Commands without a dedicated canonical skill must surface as a
        # SKILL bundle under both Codex skill trees.
        if cmd not in canonical_dedicated_skill_names:
            for required_skill in (
                repo_root / ".codex" / "skills" / cmd / "SKILL.md",
                repo_root / ".agents" / "skills" / cmd / "SKILL.md",
            ):
                if not required_skill.exists():
                    failures.append(
                        f"Missing command-as-skill bundle for command '{cmd}': "
                        f"{required_skill.relative_to(repo_root)}"
                    )

    # Orphan adapter commands.
    for adapter_dir in (repo_root / ".claude" / "commands", repo_root / ".codex" / "commands"):
        if not adapter_dir.is_dir():
            continue
        for entry in adapter_dir.iterdir():
            if entry.is_file() and entry.suffix == ".md" and entry.name != "README.md":
                if entry.stem not in canonical_commands:
                    failures.append(
                        f"Orphan adapter command file (no canonical source): "
                        f"{entry.relative_to(repo_root)}"
                    )

    # --- 3. Skill parity --------------------------------------------------
    canonical_all_skills = canonical_dedicated_skill_names
    checked_summary.append(("Canonical skills (flat + structured)", len(canonical_all_skills)))

    for skill in sorted(canonical_all_skills):
        for required in (
            repo_root / ".claude" / "skills" / skill / "SKILL.md",
            repo_root / ".codex" / "skills" / skill / "SKILL.md",
            repo_root / ".agents" / "skills" / skill / "SKILL.md",
        ):
            if not required.exists():
                failures.append(
                    f"Missing adapter SKILL.md for canonical skill '{skill}': "
                    f"{required.relative_to(repo_root)}"
                )

    # Orphan adapter skills: present as a skill bundle but with no canonical
    # source (either as a dedicated canonical skill or as a command for the
    # Codex skill trees).
    valid_skill_names_per_adapter = {
        ".claude": canonical_all_skills,
        ".codex": canonical_all_skills | canonical_commands,
        ".agents": canonical_all_skills | canonical_commands,
    }
    for adapter_name, valid_set in valid_skill_names_per_adapter.items():
        adapter_skills_dir = repo_root / adapter_name / "skills"
        for skill in sorted(discover_adapter_skill_names(adapter_skills_dir)):
            if skill not in valid_set:
                failures.append(
                    f"Orphan adapter skill bundle (no canonical source): "
                    f"{adapter_skills_dir.relative_to(repo_root)}/{skill}/SKILL.md"
                )

    # --- 4. Rules and templates parity ------------------------------------
    for kind in ("rules", "templates"):
        canonical_set = discover_canonical_basenames(infra / kind)
        checked_summary.append((f"Canonical {kind}", len(canonical_set)))
        for name in sorted(canonical_set):
            for required in (
                repo_root / ".claude" / kind / f"{name}.md",
                repo_root / ".codex" / kind / f"{name}.md",
            ):
                if not required.exists():
                    failures.append(
                        f"Missing adapter file for canonical {kind[:-1]} '{name}': "
                        f"{required.relative_to(repo_root)}"
                    )
        # Orphan detection
        for adapter_dir in (repo_root / ".claude" / kind, repo_root / ".codex" / kind):
            if not adapter_dir.is_dir():
                continue
            for entry in adapter_dir.iterdir():
                if entry.is_file() and entry.suffix == ".md" and entry.name != "README.md":
                    if entry.stem not in canonical_set:
                        failures.append(
                            f"Orphan adapter {kind[:-1]} file (no canonical source): "
                            f"{entry.relative_to(repo_root)}"
                        )

    # --- Report -----------------------------------------------------------
    if failures:
        print(
            f"Skill parity check failed: {len(failures)} structural mismatch(es).",
            file=sys.stderr,
        )
        for f in failures:
            print(f"- {f}", file=sys.stderr)
        return 1

    summary_str = ", ".join(f"{label}: {count}" for label, count in checked_summary)
    print(f"Skill parity check passed ({summary_str}).")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
