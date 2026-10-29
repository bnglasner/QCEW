# `code/` — QCEW pipeline

Run order, orchestrated by `run_all.R`. Each script opens with `rm(list = ls())`,
sources `00_setup.R` for packages, paths, and parameters, reads inputs from
`data/processed/`, and writes outputs back to disk. To refresh the analysis to a new
QCEW release, change only the parameter block in `00_setup.R`.

| Step | Script | Reads | Writes |
|---|---|---|---|
| setup | `00_setup.R` | — | shared objects (packages, paths, parameters, industry crosswalk) |
| 1 | `01_build_dataset.R` | BLS/Census APIs, `data/raw/us_county_latlng.txt` | QCEW industry/national panels, population, geometry checkpoints in `data/processed/` |
| 2 | `02_build_map_data.R` | `data/processed/` checkpoints | `map_data_dynamic.rds` |
| 3 | `03_figures_maps.R` | `map_data_dynamic.rds` | animated OTY county maps (`.gif`) |
| 4 | `04_figures_timeseries.R` | national panel | national series + industry slope charts (`.png`) |
| 5 | `05_figures_rankings.R` | industry panel, population | county high/low charts (`.png`) |
| 6 | `06_figures_spotlight.R` | geometry, live area pulls | NAICS 54 map + per-state spotlights (`.png`) |

Use the `TRUE`/`FALSE` flags at the top of `run_all.R` to select which steps run.
Steps 1 and 2 build the data that steps 3–6 consume.
