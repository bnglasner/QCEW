#!/usr/bin/env python3
"""Validate internal markdown path references used by template docs.

Walks every Markdown file in the canonical and adapter trees plus the
root-level adapter instructions. Any code-span or link target that points
inside the repo (Infrastructure/, .claude/, .codex/, .agents/, code/, data/,
drafts/, output/, or one of the named root files) is resolved and verified
to exist on disk. Targets outside that allow-list are skipped silently.
"""

from __future__ import annotations

import argparse
import re
import sys
from pathlib import Path
from typing import Iterable, List


CODE_SPAN_RE = re.compile(r"`([^`\n]+)`")
MD_LINK_RE = re.compile(r"\[[^\]]+\]\(([^)\s]+)(?:\s+\"[^\"]*\")?\)")
LINE_SUFFIX_RE = re.compile(r":\d+(?::\d+)?$")

# Anything that begins with one of these prefixes is in scope for validation.
# Adapter trees (.claude/, .codex/, .agents/) and the root-level adapter
# instruction files (CLAUDE.md, AGENTS.md) are included so broken references
# inside the generated mirrors do not silently ship.
ALLOWED_INTERNAL_PREFIXES = (
    "Infrastructure/",
    ".codex/",
    ".claude/",
    ".agents/",
    "code/",
    "data/",
    "drafts/",
    "output/",
    "PROJECT.md",
    "README.md",
    "Makefile",
    "CLAUDE.md",
    "AGENTS.md",
)

# Top-level Markdown files we treat as in-scope alongside Infrastructure/.
ROOT_MARKDOWN_FILES = (
    "README.md",
    "PROJECT.md",
    "CLAUDE.md",
    "AGENTS.md",
)

# Adapter directories that the validator also walks for internal references.
ADAPTER_DIRS = (
    ".claude",
    ".codex",
    ".agents",
)


def parse_args() -> argparse.Namespace:
    parser = argparse.ArgumentParser(description="Check internal markdown path references.")
    parser.add_argument(
        "--repo-root",
        default=None,
        help="Repository root path. Defaults to script-derived root.",
    )
    return parser.parse_args()


def iter_markdown_files(repo_root: Path) -> Iterable[Path]:
    """Yield every Markdown file in scope for reference validation.

    Order: top-level files, then Infrastructure/ recursively, then each
    adapter tree recursively. Skips dotfiles other than the adapter dirs
    themselves, and skips anything under .git/.
    """

    seen: set[Path] = set()

    def maybe_yield(path: Path) -> Iterable[Path]:
        resolved = path.resolve()
        if resolved in seen:
            return
        seen.add(resolved)
        yield path

    for name in ROOT_MARKDOWN_FILES:
        path = repo_root / name
        if path.exists():
            yield from maybe_yield(path)

    infra_root = repo_root / "Infrastructure"
    if infra_root.exists():
        for md in sorted(infra_root.rglob("*.md")):
            yield from maybe_yield(md)

    for adapter in ADAPTER_DIRS:
        adapter_root = repo_root / adapter
        if not adapter_root.exists():
            continue
        for md in sorted(adapter_root.rglob("*.md")):
            yield from maybe_yield(md)


def normalize_candidate(raw: str) -> str:
    candidate = raw.strip().strip("<>").strip("\"'").rstrip(".,;:")
    if "#" in candidate:
        candidate = candidate.split("#", 1)[0]
    if "?" in candidate:
        candidate = candidate.split("?", 1)[0]
    candidate = LINE_SUFFIX_RE.sub("", candidate)
    return candidate


def should_check(candidate: str) -> bool:
    if not candidate:
        return False
    if " " in candidate:
        return False
    if any(ch in candidate for ch in ("*", "{", "}", "$", "|")):
        return False
    if any(token in candidate for token in ("YYYY-", "[", "]", "<", ">", "TBD")):
        return False
    if candidate.startswith(("http://", "https://", "mailto:", "tel:", "data:")):
        return False
    return candidate.startswith(ALLOWED_INTERNAL_PREFIXES)


def resolve_candidate(repo_root: Path, source_file: Path, candidate: str) -> Path:
    if candidate.startswith("./"):
        return (source_file.parent / candidate[2:]).resolve()
    if candidate.startswith("../"):
        return (source_file.parent / candidate).resolve()
    return (repo_root / candidate).resolve()


def collect_candidates(line: str) -> List[str]:
    candidates: List[str] = []
    candidates.extend(MD_LINK_RE.findall(line))
    candidates.extend(CODE_SPAN_RE.findall(line))
    return candidates


def main() -> int:
    args = parse_args()
    script_root = Path(__file__).resolve().parents[2]
    repo_root = Path(args.repo_root).resolve() if args.repo_root else script_root

    if not repo_root.exists():
        print(f"ERROR: repo root does not exist: {repo_root}", file=sys.stderr)
        return 1

    missing: List[str] = []
    checked_count = 0
    file_count = 0

    for md_file in iter_markdown_files(repo_root):
        file_count += 1
        try:
            lines = md_file.read_text(encoding="utf-8").splitlines()
        except UnicodeDecodeError:
            lines = md_file.read_text(encoding="latin-1").splitlines()

        for idx, line in enumerate(lines, start=1):
            for raw in collect_candidates(line):
                candidate = normalize_candidate(raw)
                if not should_check(candidate):
                    continue

                checked_count += 1
                resolved = resolve_candidate(repo_root, md_file, candidate)
                if not resolved.exists():
                    relative_md = md_file.relative_to(repo_root)
                    missing.append(f"{relative_md}:{idx} -> {candidate}")

    if missing:
        print(
            f"Internal path reference check failed: {len(missing)} missing reference(s) "
            f"across {checked_count} checked reference(s) in {file_count} file(s).",
            file=sys.stderr,
        )
        for item in missing:
            print(f"- {item}", file=sys.stderr)
        return 1

    print(
        f"Internal path reference check passed: {checked_count} reference(s) "
        f"validated across {file_count} file(s)."
    )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
