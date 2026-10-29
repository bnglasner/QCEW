# QCEW Employment & Establishment Dynamics

Tracking over-the-year change in employment, establishments, and average weekly wages
across U.S. counties and industries, using the most recent Quarterly Census of
Employment and Wages (QCEW) release from the Bureau of Labor Statistics.

The analysis produces animated county maps, national and industry-level time series,
county ranking charts, and geographic spotlights. It is built to refresh to a new QCEW
quarter by changing a single parameter block.

## Data sources

| Source | Accessed via | Notes |
|---|---|---|
| QCEW (private sector, `own_code == 5`) | BLS QCEW open-data slice CSVs via `read_qcew_slice()` (in `00_setup.R`) | Industry and area slices read from `https://data.bls.gov/cew/data/api/[YEAR]/[QTR]/[SLICE]/[CODE].csv`, cached under `data/raw/qcew_slices/`. Replaces the removed `blscrapeR::qcew_api()` (audit CE-001). File layout in `Infrastructure/references/literature/data_dictionaries/`. |
| County population estimates | `tidycensus::get_estimates()` | Requires a Census API key. Thresholds the ranking charts at 100,000 population. |
| County geometry | `tigris::counties()` | Cartographic boundaries; Alaska and Hawaii excluded for presentation. |
| County latitude/longitude | `data/raw/us_county_latlng.txt` | Static reference for map point placement. |

## Repository layout

- `code/` — the analysis pipeline (see below).
- `data/raw/` — input reference data (county lat/long, legacy geographic files).
- `data/processed/` — pipeline checkpoints written as `.rds` and `.parquet` (gitignored output of `01`/`02`).
- `output/figures/` — generated figures; `output/figures/legacy_2022q3/` holds the original 2022 Q3 deliverables.
- `output/tables/` — generated tables.
- `archive/` — the original monolithic 2023 script and tweet drafts, preserved for provenance.
- `Infrastructure/` — shared AI workflow, guardrails, review agents, and EIG style system (template scaffolding; see "Project infrastructure" below).

## Pipeline

Scripts are numbered and run in order by `code/run_all.R`. Each script is self-contained:
it opens with `rm(list = ls())`, sources `code/00_setup.R` for packages, paths, and
parameters, reads its inputs from `data/processed/`, and writes its outputs back to disk.
Scripts communicate only through those checkpoints, so any step can be re-run on its own.

| Script | Purpose |
|---|---|
| `00_setup.R` | Packages, user-keyed project paths, and the single parameter block (target year/quarter, lookback window, display caps, population threshold, spotlight areas, industry crosswalk). |
| `01_build_dataset.R` | Pulls QCEW industry and national panels, county population, and county geometry; writes checkpoints. |
| `02_build_map_data.R` | Assembles the county-level over-the-year map dataset and applies the color-scale cap. |
| `03_figures_maps.R` | Animated over-the-year county maps for employment and establishments. |
| `04_figures_timeseries.R` | National all-industry over-the-year series and industry-level establishment and employment slope charts. |
| `05_figures_rankings.R` | Top and bottom five counties for employment, establishment, and average weekly wage growth. |
| `06_figures_spotlight.R` | NAICS 54 (professional services) county map and per-state industry spotlights. |

## How to run

1. Install R (Windows or macOS) and the packages listed in `code/00_setup.R`.
2. Set a Census API key for `tidycensus` (`tidycensus::census_api_key("<KEY>", install = TRUE)`).
3. Add your machine's user name and repository path to `project_directories` in both
   `code/run_all.R` and `code/00_setup.R`.
4. To target a different release, change the parameter block at the top of `code/00_setup.R`
   (`target_year_int`, `target_qtr_int`, and related parameters). Nothing else needs editing.
5. Run `code/run_all.R`. Use the `TRUE`/`FALSE` flags near the top to select which steps run.

The pipeline pulls live data from the BLS and Census APIs and uses geospatial packages
(`sf`, `tigris`, `tidycensus`, `gganimate`), so it runs locally in R rather than in a
restricted or headless environment.

## Outputs

Figures are written to `output/figures/` with a `file_date` suffix derived from the target
quarter (for example, `_2025_qrt4`). The 2022 Q3 figures from the original analysis remain
under `output/figures/legacy_2022q3/`.

Figures follow the EIG figure style (token-derived palette, "Figure N." sentence-case
captions, `Source: BLS QCEW, YYYY Qn.` lines, horizontal gridlines only, no plot borders)
and Tufte visual-design principles (the maps use an accessible terra-cotta→cream→forest-green
diverging scale with area-proportional points; the industry charts are small multiples). The
EIG theme is sourced from `Infrastructure/style/themes/r/eig_theme.R`.

## Project infrastructure

This repository sits on top of a reusable AI-assisted research template. `PROJECT.md`
holds project context, `Infrastructure/` holds shared workflow, guardrails, review agents,
and the EIG style system, and the `Makefile` exposes maintenance checks
(`make maintenance-check`). Audit reports from the 2026 rebuild are in `review-reports/`,
and the rebuild plan and spec are in `Infrastructure/plans/` and `Infrastructure/specs/`.
