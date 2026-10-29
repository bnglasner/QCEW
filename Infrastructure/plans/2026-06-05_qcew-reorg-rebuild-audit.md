# Plan: QCEW Reorganization, Rebuild, and Audit

**Date:** 2026-06-05
**Status:** COMPLETED (reorg/refactor/audit) — CE-001 migration + live 2025 Q4 run done 2026-06-06; see `Infrastructure/session_logs/2026-06-06_qcew-pipeline-run-and-style.md`. Figure EIG/Tufte restyle in progress.
**Spec:** `Infrastructure/specs/2026-06-05_qcew-reorg-rebuild-audit.md`

## Approach

Five phases, executed in order, each small and reversible. Code work and verification happen in this sandbox; live data pulls and full execution are handed off to Ben's Windows machine (no R / no BLS-Census network access here).

## Phase 1 — Project metadata (low risk)

1. Populate `PROJECT.md`: title, research question (county- and industry-level over-the-year employment, establishment, and wage dynamics from the QCEW), data-in-scope table (QCEW via `blscrapeR`/BLS API, Census population estimates via `tidycensus`, TIGER county geometry via `tigris`, county lat/long reference), deliverables (animated maps, national/industry time series, county ranking charts, geographic spotlights), constraints (Linux sandbox cannot run the pipeline; APIs require keys).

## Phase 2 — Reorganize legacy files ("relocate + clean")

Use `git mv` where possible to preserve history. Target layout:

| Legacy item | Destination |
|---|---|
| `*.png`, `*.gif` (12 figure outputs) | `output/figures/legacy_2022q3/` |
| `QCEW_Analysis_2022_q3_date_3_08_2023.R` | `archive/QCEW_Analysis_2022_q3_date_3_08_2023.R` |
| `rmarkdown_tweet_drafts.Rmd` + `.html` | `archive/` |
| `us_county_latlng.txt` | `data/raw/` |
| `cb_2018_us_state_500k.shp` (incomplete — no sidecars) | `data/raw/` + note in audit (unusable as-is; script uses `tigris` instead) |
| `gadm36_USA_2_sp.rds` (unused by script) | `data/raw/` + note in audit (orphan) |
| `naics-based-quarterly-layout-xlsx.xlsx` (QCEW file layout = data dictionary) | `Infrastructure/references/literature/data_dictionaries/` |
| `.Rhistory` (root + `code/`), `.DS_Store` | delete |

Update `.gitignore` to add `.Rhistory`, `*.Rproj`, and R/data artifact patterns. Confirm root is clean afterward.

## Phase 3 — Refactor into modular pipeline (core work)

Decompose the monolith into single-purpose scripts under `code/`, each following Ben's R style. Parameters centralized in `run_all.R` (or a `00_parameters.R` sourced first): `target_year_int`, `target_qtr_int` (= 2025, 4), `lookback_years_int`, percent-change display cap, population threshold, spotlight FIPS list.

- `code/run_all.R` — orchestrator: parameters + `TRUE/FALSE` flags + user-keyed paths (replacing `setwd`/Dropbox) + sources each script in `new.env(parent = globalenv())`. Built on the existing template skeleton.
- `code/01_build_dataset.R` — industry crosswalk; load county lat/long, TIGER counties, Census population; loop QCEW industry + national pulls across the lookback window (single year-loop, not copy-pasted blocks); combine; checkpoint to `.rds` + `.parquet`.
- `code/02_build_map_data.R` — construct the county `dynamic` map dataset; apply the ±cap (corrected: cap is built from `_pct_chg`, with the legacy bug flagged).
- `code/03_figures_maps.R` — animated OTY employment + establishment maps.
- `code/04_figures_timeseries.R` — national all-industry OTY series; industry-level establishment and employment slope charts (factor levels generated from the data, not hardcoded).
- `code/05_figures_rankings.R` — top/bottom-five counties for employment, establishment, and wage growth (the `state != "53"` exclusion either removed or documented).
- `code/06_figures_spotlight.R` — NAICS 54 map; Washington and Maine industry spotlights, generalized over a FIPS parameter.

Figures write to `output/figures/` with `file_date`-stamped names. No behavior changed silently — each correction carries an inline comment.

## Phase 4 — Rewrite README

Replace template README with project documentation: overview, data sources and access notes, pipeline run order, parameters, outputs, and the Windows-execution handoff note. Preserve a short pointer to the Infrastructure template for provenance.

## Phase 5 — Full audit

1. **Manual + agent code review** — apply `code-quality-rules`, `methodology-rules`, and `ai-skeptic-rules` to the refactored scripts; write HTML reports to `review-reports/` (code-error, methodology, ai-skeptic). Known items to confirm: establishment-map color bug; undocumented `state != "53"` filter; `left_join` without `by=`; population/vintage currency; QCEW `own_code == 5` private-sector restriction; ±25% display cap.
2. **`make maintenance-check`** — run and record (brain-check, parity-check, literature-check, path references, catalog staleness). Investigate any failure introduced by the reorg (esp. internal path references and the data-dictionary addition).
3. **Syntax validation** — attempt R install in sandbox for `Rscript -e 'parse()'` on each script; else manual parse review.

## Phase 6 — Verify and log

Per `.claude/rules/verification-protocol.md`: confirm root is clean, all referenced paths resolve, scripts parse, `maintenance-check` status recorded, key relocations spot-checked. Write a session log to `Infrastructure/session_logs/` with an Evidence section and a clear handoff (Ben runs `run_all.R` on Windows with API keys to regenerate 2025 Q4 outputs).

## Files to modify / create

- Modify: `PROJECT.md`, `README.md`, `.gitignore`, `code/run_all.R`
- Create: `code/01..06_*.R`, `review-reports/*.html`, `Infrastructure/session_logs/2026-06-05_*.md`
- Move: legacy outputs/data/script/draft (Phase 2 table)
- Delete: `.Rhistory` (x2), `.DS_Store` files

## Verification steps

- Root listing shows no loose `.png/.gif/.R/.txt/.shp/.rds/.xlsx/.Rmd/.html` legacy files.
- `grep` for hardcoded `2022`/`2021` literals in `code/` returns only parameterized references.
- `Rscript --vanilla -e 'parse("code/XX.R")'` succeeds for each script (if R available).
- `make maintenance-check` output captured.
- Each audit report exists and is non-empty.

## Risks / tradeoffs

- **Cannot execute the pipeline here** — refactor correctness is verified statically; Ben must run on Windows. Documented in handoff.
- **API/vintage drift** — `tidycensus::get_estimates()` and `qcew_api()` behavior may have changed since 2023; flagged, not silently rewritten.
- **Behavior changes from bug fixes** — applied with inline markers and listed in the session log so Ben can accept or revert.
- **Stale-output placement** — `legacy_2022q3/` subfolder is an assumption; trivially movable.

## Approval

[ ] User approved: __________
