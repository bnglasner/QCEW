# 02_build_map_data -- assemble the county-level over-the-year map dataset
# Author - Ben Glasner
# research title - QCEW Employment & Establishment Dynamics
# research question - How are employment and establishments changing across U.S.
#   counties over the year in the latest QCEW release?

rm(list = ls())
options(scipen = 999)
set.seed(42)

# 0) Load shared setup (packages, paths, parameters). Working directory = repo root.
source(file.path("code", "00_setup.R"))

# 1) Load checkpoints from 01_build_dataset.R
qcew_industry_panel <- readRDS(file.path(path_processed, "qcew_industry_panel.rds"))
counties_geo        <- readRDS(file.path(path_processed, "counties_geo.rds"))

#############################################################
###             County Centroids (point layer)           ###
#############################################################
# 2) Derive one point per county polygon from the geometry itself, so map points always
#    match the polygon vintage and FIPS scheme. st_point_on_surface guarantees the point
#    falls inside the polygon. This replaces the retired static us_county_latlng.txt, which
#    lacked the 2024 Connecticut planning regions and Oglala Lakota County, SD (their points
#    were missing). See 01_build_dataset.R and the session log.
suppressWarnings(centroid_sf <- st_point_on_surface(counties_geo))
county_centroids <- data.frame(
  GEOID = as.character(centroid_sf$GEOID),
  lng   = st_coordinates(centroid_sf)[, 1],
  lat   = st_coordinates(centroid_sf)[, 2],
  stringsAsFactors = FALSE
)

# 2b) CONNECTICUT CONTINUITY (full-continuity fix, 2026-06-06): Connecticut replaced its 8
#     legacy counties (FIPS 0900x) with 9 planning regions (091xx) in QCEW data starting in
#     2024. The 2024 cartographic geometry above has only the planning regions, so CT's REAL
#     2022-2023 county data (present in the QCEW pull as 0900x) had no centroid and was
#     dropped, leaving CT blank pre-2024. We append legacy-county centroids from the 2021
#     boundaries (the last vintage with 0900x) so those real county values are plotted. No
#     data are estimated: pre-2024 CT shows real legacy-county values at legacy-county
#     locations; 2024+ shows real planning-region values. (The static basemap uses the
#     post-2024 regions, so pre-2024 CT points sit over the current outline.)
suppressMessages(suppressWarnings(
  ct_legacy_geo <- tigris::counties(state = "09", cb = TRUE, resolution = "500k", year = 2021)
))
suppressWarnings(ct_legacy_sf <- st_point_on_surface(ct_legacy_geo))
ct_legacy_centroids <- data.frame(
  GEOID = as.character(ct_legacy_sf$GEOID),
  lng   = st_coordinates(ct_legacy_sf)[, 1],
  lat   = st_coordinates(ct_legacy_sf)[, 2],
  stringsAsFactors = FALSE
)
county_centroids <- rbind(
  county_centroids,
  ct_legacy_centroids[!ct_legacy_centroids$GEOID %in% county_centroids$GEOID, ]
)

#############################################################
###             Build County Map Point Data              ###
#############################################################
# 3) Over-the-year change measures, one row per county-quarter, joined to centroids.
#    CE-005 label fix (2026-06-06, cosmetic only): quarter encoded as (qtr-1)/4 so the
#    INTEGER part of year_quarter is the correct calendar year (Q1=.0 ... Q4=.75); the
#    legacy year + qtr/4 sent Q4 to the next year, mislabeling the animation subtitle.
#    inner_join to centroids keeps only counties present in the cartographic geometry
#    (drops Alaska, statewide 999, MSA C-codes, territories) -- same scope as before.
map_data_dynamic <- qcew_industry_panel |>
  mutate(year_quarter = year + (qtr - 1L) / 4) |>
  rename("GEOID" = "area_fips") |>
  select(GEOID, year_quarter,
         oty_month3_emplvl_chg,
         oty_month3_emplvl_pct_chg,
         oty_qtrly_estabs_chg,
         oty_qtrly_estabs_pct_chg) |>
  na.omit() |>
  distinct() |>
  inner_join(county_centroids, by = "GEOID")

# 4) Cap the percent-change measures at +/- pct_cap_num for color-scale readability
map_data_dynamic$oty_month3_emplvl_pct_chg_edit <- map_data_dynamic$oty_month3_emplvl_pct_chg
map_data_dynamic$oty_month3_emplvl_pct_chg_edit[map_data_dynamic$oty_month3_emplvl_pct_chg_edit > pct_cap_num] <- pct_cap_num
map_data_dynamic$oty_month3_emplvl_pct_chg_edit[map_data_dynamic$oty_month3_emplvl_pct_chg_edit < (-pct_cap_num)] <- (-pct_cap_num)

# refactor fix (2026-06-05): legacy code seeded the establishment color variable from
# oty_qtrly_estabs_chg (the COUNT change) instead of oty_qtrly_estabs_pct_chg (the
# PERCENT change). Corrected to use the percent change. See review-reports/.
map_data_dynamic$oty_qtrly_estabs_pct_chg_edit <- map_data_dynamic$oty_qtrly_estabs_pct_chg
map_data_dynamic$oty_qtrly_estabs_pct_chg_edit[map_data_dynamic$oty_qtrly_estabs_pct_chg_edit > pct_cap_num] <- pct_cap_num
map_data_dynamic$oty_qtrly_estabs_pct_chg_edit[map_data_dynamic$oty_qtrly_estabs_pct_chg_edit < (-pct_cap_num)] <- (-pct_cap_num)

# 5) Save checkpoint. Now a plain data.frame (points), so both .rds and .parquet are written.
saveRDS(map_data_dynamic, file.path(path_processed, "map_data_dynamic.rds"))
arrow::write_parquet(map_data_dynamic,
                     file.path(path_processed, "map_data_dynamic.parquet"),
                     compression = "snappy")

message("02_build_map_data.R complete | map point rows = ", nrow(map_data_dynamic),
        " | counties with centroid = ", nrow(county_centroids))
