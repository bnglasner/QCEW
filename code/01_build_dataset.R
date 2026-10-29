# 01_build_dataset -- pull and assemble QCEW, population, and geometry inputs
# Author - Ben Glasner
# research title - QCEW Employment & Establishment Dynamics
# research question - How are employment, establishments, and average weekly wages
#   changing across U.S. counties and industries over the year in the latest QCEW release?

rm(list = ls())
options(scipen = 999)
set.seed(42)

# 0) Load shared setup (packages, paths, parameters). Working directory = repo root.
source(file.path("code", "00_setup.R"))

#############################################################
###                 Reference Geography                   ###
#############################################################
# 1) County cartographic boundaries, excluding Alaska (02) and Hawaii (15) for presentation
#    NOTE (2026-06-06): the static data/raw/us_county_latlng.txt reference is RETIRED. Map
#    point coordinates are now derived from these county polygons' centroids (in
#    02_build_map_data.R), so they always match the geometry vintage and cover counties the
#    stale file lacked (e.g. the 2024 Connecticut planning regions, Oglala Lakota SD).
counties_geo <- tigris::counties(cb = TRUE, resolution = "500k", year = counties_geo_year_int)
state_fips_num <- as.numeric(as.character(counties_geo$STATEFP))
keep_state_flag <- state_fips_num <= 56 & counties_geo$STATEFP != "02" & counties_geo$STATEFP != "15"
counties_geo <- counties_geo[keep_state_flag, ]

# 3) County population estimates (used to threshold the ranking charts)
#    MR-002 fix (2026-06-06): vintage is now set explicitly so the population year is
#    recorded rather than implicit. The threshold (pop >= 100,000) uses this vintage while
#    the QCEW measures are the target quarter -- a small, disclosed mismatch (see README).
county_population <- tidycensus::get_estimates(
  geography = "county",
  product = "characteristics",
  breakdown = c("SEX"),
  breakdown_labels = TRUE,
  vintage = 2024L
) |>
  # BEHAVIOR CHANGE (AS-002, 2026-06-06): the Vintage 2024 PEP "characteristics" product no
  # longer returns a "Both sexes" total row -- it returns only "Male" and "Female". The
  # legacy `filter(SEX == "Both sexes")` therefore returned zero rows. Total county
  # population is reconstructed as Male + Female, which is identical to the legacy total and
  # preserves the intent of the >= 100,000-population ranking filter.
  group_by(GEOID, NAME) |>
  summarise(population = sum(value), .groups = "drop") |>
  rename("area_fips" = "GEOID")

#############################################################
###                      QCEW Pulls                       ###
#############################################################
# Layout: Infrastructure/references/literature/data_dictionaries/naics-based-quarterly-layout-xlsx.xlsx
# https://www.bls.gov/cew/about-data/downloadable-file-layouts/quarterly/naics-based-quarterly-layout.htm
# We pull private-sector (own_code == 5) data for the target year and the prior
# (lookback_years_int - 1) years. The target year is truncated at target_qtr_int.
#
# CE-001 migrated (2026-06-06): blscrapeR::qcew_api() was removed from the package, so the
# pulls below read the BLS QCEW open-data slice CSVs via read_qcew_slice() (defined in
# 00_setup.R). The reader returns the same columns the pipeline expects and the year-quarter
# grid loop is unchanged -- only the qcew_api() call was swapped for read_qcew_slice().
# Endpoint: https://data.bls.gov/cew/data/api/[YEAR]/[QTR]/[SLICE]/[CODE].csv

# 4) Build the year-quarter pull grid
first_year_int <- target_year_int - (lookback_years_int - 1L)
pull_grid_df <- do.call(rbind, lapply(first_year_int:target_year_int, function(yr) {
  max_qtr <- if (yr == target_year_int) target_qtr_int else 4L
  data.frame(year_int = yr, qtr_int = seq_len(max_qtr))
}))
message("QCEW pull grid: ", nrow(pull_grid_df), " year-quarter pulls (",
        first_year_int, " to ", target_year_int, ")")

# 5) Industry slice (sliceCode 10 = total, all industries) across the grid
industry_list <- vector("list", nrow(pull_grid_df))
for (i in seq_len(nrow(pull_grid_df))) {
  yr <- pull_grid_df$year_int[i]
  qt <- pull_grid_df$qtr_int[i]
  message("  industry pull: ", yr, " Q", qt)
  industry_list[[i]] <- tryCatch(
    read_qcew_slice(yr, qt, "industry", "10") |>
      mutate(fips = area_fips) |>
      filter(own_code == 5),
    error = function(e) {
      message("    WARNING: industry pull failed for ", yr, " Q", qt, ": ", conditionMessage(e))
      NULL
    }
  )
}
qcew_industry_panel <- bind_rows(industry_list)

# 6) National/area slice (sliceCode US000 = U.S. total, by industry and aggregation level)
national_list <- vector("list", nrow(pull_grid_df))
for (i in seq_len(nrow(pull_grid_df))) {
  yr <- pull_grid_df$year_int[i]
  qt <- pull_grid_df$qtr_int[i]
  message("  national pull: ", yr, " Q", qt)
  national_list[[i]] <- tryCatch(
    read_qcew_slice(yr, qt, "area", "US000") |>
      mutate(fips = area_fips) |>
      filter(own_code == 5),
    error = function(e) {
      message("    WARNING: national pull failed for ", yr, " Q", qt, ": ", conditionMessage(e))
      NULL
    }
  )
}
qcew_national_panel <- bind_rows(national_list)

#############################################################
###          QA: External validation of totals            ###
#############################################################
# MR-005 (2026-06-06): lightweight spot-check. Print the national private-sector,
# all-industry totals for the target quarter (agglvl 11 = national by ownership,
# industry 10 = all industries) so a malformed pull is caught early. Compare these
# against the published BLS QCEW news release for the target quarter (see README Evidence).
qa_national_row <- qcew_national_panel |>
  filter(agglvl_code == 11, industry_code == "10",
         year == target_year_int, qtr == target_qtr_int)
if (nrow(qa_national_row) == 1) {
  message("QA national private totals ", target_year_int, " Q", target_qtr_int,
          " | establishments = ", format(qa_national_row$qtrly_estabs, big.mark = ","),
          " | month-3 employment = ", format(qa_national_row$month3_emplvl, big.mark = ","),
          " | OTY emp % = ", qa_national_row$oty_month3_emplvl_pct_chg,
          " | OTY estab % = ", qa_national_row$oty_qtrly_estabs_pct_chg)
} else {
  message("QA WARNING: expected exactly one national private all-industry row for ",
          target_year_int, " Q", target_qtr_int, " but found ", nrow(qa_national_row))
}

#############################################################
###                   Save Checkpoints                    ###
#############################################################
# 7) Tabular checkpoints in both .rds and .parquet (snappy)
saveRDS(qcew_industry_panel, file.path(path_processed, "qcew_industry_panel.rds"))
arrow::write_parquet(qcew_industry_panel,
                     file.path(path_processed, "qcew_industry_panel.parquet"),
                     compression = "snappy")

saveRDS(qcew_national_panel, file.path(path_processed, "qcew_national_panel.rds"))
arrow::write_parquet(qcew_national_panel,
                     file.path(path_processed, "qcew_national_panel.parquet"),
                     compression = "snappy")

saveRDS(county_population, file.path(path_processed, "county_population.rds"))
arrow::write_parquet(county_population,
                     file.path(path_processed, "county_population.parquet"),
                     compression = "snappy")

# 8) Geometry checkpoint as .rds only (sf geometry is not written to plain parquet here)
saveRDS(counties_geo, file.path(path_processed, "counties_geo.rds"))

message("01_build_dataset.R complete | industry rows = ", nrow(qcew_industry_panel),
        " | national rows = ", nrow(qcew_national_panel))
