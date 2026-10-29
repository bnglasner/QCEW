# Requirements Specification: QCEW Reorganization, Rebuild, and Audit

**Date:** 2026-06-05
**Status:** DRAFT
**Suggested Save Location:** `Infrastructure/specs/2026-06-05_qcew-reorg-rebuild-audit.md`

## Objective

Reconcile the legacy 2023 QCEW analysis with the Template V2 infrastructure: relocate legacy materials into the template structure, refactor the monolithic analysis script into a modular, parameterized R pipeline targeting the latest available QCEW release (2025 Q4), and run a full audit (review agents + `make maintenance-check` + manual review) before handoff.

## Background

The repository began as a single-script QCEW analysis (`QCEW_Analysis_2022_q3_date_3_08_2023.R`, ~910 lines) and recently had the Template V2 AI-research shell layered on top. The two have not been reconciled: `PROJECT.md` is empty, `README.md` still describes the template, and legacy code, outputs, and data sit loose in the repo root. The legacy script hardcodes Dropbox paths, year literals, and factor levels; it also contains at least one substantive bug and several undocumented analytical choices (catalogued during the audit phase).

## Requirements

### MUST Have (Non-Negotiable)

- [ ] `PROJECT.md` populated with title "QCEW Employment & Establishment Dynamics" and the research question, data-in-scope, deliverables, and constraints derived from the legacy analysis.
- [ ] Legacy materials relocated per the "relocate + clean" decision: outputs to `output/figures/`, reference/input data to `data/raw/`, the QCEW file-layout workbook to `Infrastructure/references/literature/data_dictionaries/`, the original monolithic script and rendered tweet draft archived under `archive/`, and cruft (`.Rhistory`, `.DS_Store`) deleted.
- [ ] Monolithic script refactored into numbered, single-purpose scripts under `code/`, following Ben's R style guide (header block, `rm/scipen/set.seed`, section banners, numbered steps, `message()` progress, descriptive variable suffixes, no custom functions without approval, `.rds` + `.parquet` checkpoints).
- [ ] Pipeline parameterized to the latest available QCEW quarter (2025 Q4 as of this date), with target year/quarter and lookback window set in one place — no hardcoded year literals or factor levels scattered through the figure code.
- [ ] `code/run_all.R` wired with `TRUE/FALSE` flags at top and each script sourced in a fresh `new.env(parent = globalenv())`.
- [ ] `README.md` rewritten to describe the QCEW project (not the template).
- [ ] Full audit run: repo review agents on the refactored code, `make maintenance-check`, and a manual logic review; findings collected in `review-reports/`.
- [ ] No fabricated data, citations, or results; outputs with claims carry an Evidence section (Sources / Confidence / Assumptions).
- [ ] All known legacy bugs and undocumented restrictions surfaced in the audit (not silently "fixed" without being flagged); behavior-changing fixes are made explicit.

### SHOULD Have (Preferred)

- [ ] Each refactored script preserves the original analytical intent (same figures, same OTY logic) while generalizing the quarter.
- [ ] R scripts syntax-validated via `Rscript -e 'parse()'` if R can be installed in the sandbox; otherwise careful manual parse review with rationale noted.
- [ ] Legacy 2022 Q3 outputs kept distinct from future rebuilt outputs (dated filenames retained; placed in a `legacy_2022q3/` subfolder within `output/figures/` to avoid confusion with fresh runs).
- [ ] Orphan data files (incomplete `cb_2018_us_state_500k.shp` lacking sidecars; unused `gadm36_USA_2_sp.rds`) identified and dispositioned with a note rather than silently relocated.
- [ ] Session log written per `Infrastructure/rules/session-logging.md`.

### MAY Have (Optional, If Time)

- [ ] A short `code/README.md` or header comment documenting the pipeline run order.
- [ ] EIG style alignment of figures (palette/tokens) flagged as a follow-up rather than executed in this pass.
- [ ] A parameter to switch the geographic spotlight (currently Washington, Maine) without editing figure code.

## Clarity Status

| Aspect | Status | Notes |
|--------|--------|-------|
| Project title | CLEAR | "QCEW Employment & Establishment Dynamics" (Ben selected). |
| Rebuild meaning | CLEAR | Refactor + update to latest quarter (2025 Q4), parameterized. |
| Legacy file disposition | CLEAR | Relocate + clean (Ben selected). |
| Audit scope | CLEAR | Both manual + template tooling (Ben selected). |
| Target quarter | CLEAR | 2025 Q4 is the latest available release (BLS, released 2026-06-02). |
| Can the pipeline run in this environment | CLEAR | No. Sandbox is Linux without R and cannot reach BLS/Census APIs. All code work + static/syntax verification happens here; live execution and data pulls happen on Ben's Windows machine. |
| Where stale 2022 Q3 outputs live | ASSUMED | `output/figures/legacy_2022q3/`, dated names retained. Ben can override to a flat `output/figures/` or `archive/`. |
| Whether to fix the establishment-map color-variable bug | ASSUMED | Flag in audit AND apply the corrected fix in the refactor (assign from `_pct_chg`, not `_chg`), with an inline comment marking the change. Ben can revert. |
| Session-log cadence | ASSUMED | Milestone-based for this project unless Ben specifies otherwise. |
| Census population source currency | ASSUMED | `tidycensus::get_estimates()` vintage/availability is verified during refactor; if the API/product changed, flag and propose the current equivalent rather than silently swapping. |

## Success Criteria

- `PROJECT.md` has a title and the guardrail gate is satisfied.
- Repo root contains no loose legacy outputs, data, or cruft; everything lives in its template-appropriate location.
- `code/` contains modular numbered scripts plus a wired `run_all.R`; no hardcoded year/quarter literals remain in figure logic.
- `README.md` describes the QCEW project.
- `review-reports/` contains the audit outputs; `make maintenance-check` result is recorded; manual findings documented.
- A session log records decisions, the known-bug disposition, and remaining handoff items (chiefly: Ben runs the pipeline on Windows to regenerate 2025 Q4 outputs).

## Approval

[ ] User approved: __________
