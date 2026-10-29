# QCEW Rebuild — Audit Summary

Date: 2026-06-05 · Scope: refactored `code/` pipeline + archived legacy script + repository infrastructure.

## Reports

- `code-error-report.html` — definitive bugs and efficiency findings (CE-001 … CE-008).
- `methodology-report.html` — design, sample, and disclosure findings (MR-001 … MR-007).
- `ai-skeptic-report.html` — fabrication/correctness, assumption inventory, edge cases (AS-001 … AS-004).

## Headline items

1. **CE-001 / AS-003 (CRITICAL, RESOLVED 2026-06-06):** `blscrapeR::qcew_api()` was removed from the package. Migrated to the BLS open-data slice CSVs via `read_qcew_slice()` (in `00_setup.R`); all four call sites swapped. The pipeline now runs end-to-end. See `Infrastructure/session_logs/2026-06-06_qcew-pipeline-run-and-style.md`.
2. **AS-001 (CRITICAL, Fixed):** the legacy establishment maps colored counties by the *count* change instead of the *percent* change — the color scale contradicted its own legend. Corrected in the refactor with inline markers.
3. **CE-002 (HIGH, Fixed):** hard-coded year-quarter factor levels in the slope charts broke generalization; now derived from the data.

### Resolved during the 2026-06-06 run/restyle session

- **AS-002 (RESOLVED):** the Vintage 2024 PEP "characteristics" product no longer returns a "Both sexes" total (only Male/Female), so the legacy filter returned 0 rows. Total county population is now reconstructed as Male + Female. Flagged inline as a behavior change.
- **MR-002 (RESOLVED):** `get_estimates(vintage = 2024L)` set explicitly.
- **MR-005 (RESOLVED):** national private all-industry totals are printed as a QA check in `01` (12,122,185 establishments; 133,863,283 month-3 employment; OTY emp +0.3% / estab +1.9% for 2025 Q4) and cross-checked against the raw BLS slice.
- **AS-004 (ADDRESSED):** ranking scripts now warn when fewer than 10 bars remain.
- **CE-005 (map label, ADDRESSED):** map time encoding changed to `year + (qtr-1)/4` so the animation subtitle shows the correct calendar year for Q4.
- **Figures:** brought into EIG + Tufte compliance (token palette, small multiples, accessible diverging maps). See the session log.

## Tooling results

- `make maintenance-check`: **7 passed, 0 failed** (brain-check, skill-parity, metadata-coverage, canonical-consistency, literature-catalog, internal-paths, catalog-staleness).
- Structural validation of all R scripts: balanced delimiters, base pipe (`|>`) throughout, no stray `%>%`, no leaked year literals in figure logic.
- R syntax execution / edge-case tests: **run 2026-06-06** — R 4.5.2 on macOS; `run_all.R` completed end-to-end against live BLS/Census data and wrote all 2025 Q4 checkpoints and figures.

## Verification standard (per repository guardrails)

**Sources:** `archive/QCEW_Analysis_2022_q3_date_3_08_2023.R` (line-level read); refactored `code/*.R`; blscrapeR NEWS (keberwein/blscrapeR); tidycensus documentation (walker-data.com); BLS QCEW release calendar.
**Confidence:** High for CE-001/AS-001/CE-002 (verified directly). Medium for MR-002 (vintage interaction) and AS-004 (edge cases, untested).
**Assumptions:** the migrated QCEW reader will return the same column schema; QCEW 2025 Q4 layout matches the historical NAICS quarterly layout. Both labeled, not assumed silently.
