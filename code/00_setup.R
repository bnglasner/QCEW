# 00_setup -- shared packages, paths, and analysis parameters for the QCEW pipeline
# Author - Ben Glasner
# research title - QCEW Employment & Establishment Dynamics
# research question - How are employment, establishments, and average weekly wages
#   changing across U.S. counties and industries over the year in the latest QCEW release?

# This script is sourced by run_all.R and by each numbered script. It does NOT call
# rm(list = ls()); it only defines shared objects. Set the working directory to the
# repository root before sourcing (run_all.R and an RStudio project both do this).

#############################################################
###                     Load Packages                     ###
#############################################################
# 1) Declare and load required packages (fail loudly if any are missing)
required_packages <- c(
  "dplyr",        # data manipulation
  "tidyr",        # reshaping
  "forcats",      # factor ordering for ranking charts
  "ggplot2",      # plotting
  "ggthemes",     # theme_map() for the county maps
  "scales",       # comma() / percent() axis labels
  "gganimate",    # animated over-the-year maps
  "gifski",       # gif renderer for gganimate
  "sf",           # county geometry
  # NOTE (CE-001 migration, 2026-06-06): blscrapeR was dropped. qcew_api() was removed
  #   from the package, so QCEW pulls now read the BLS open-data slice CSVs directly via
  #   read_qcew_slice() (defined below). No blscrapeR dependency remains.
  "tigris",       # county cartographic boundaries
  "tidycensus",   # county population estimates
  "arrow"         # parquet checkpoints
)
# NOTE (refactor 2026-06-05): the legacy script also loaded ggforce, ggmap, ggrepel,
# and plm. None were used by the analysis and they are dropped here. See review-reports/.

for (pkg in required_packages) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    stop(paste0("Missing package: ", pkg, ". Please install it before running."))
  }
  suppressPackageStartupMessages(library(pkg, character.only = TRUE))
}

#############################################################
###                       Set Paths                       ###
#############################################################
# 2) Resolve the project root for the current user, then derive sub-paths
project_directories <- list(
  # "user_from_Sys.info" = "C:/path/to/GitHub/QCEW"
  "bglasner"        = "C:/Users/bglasner/Documents/GitHub/QCEW",
  "bngla"           = "C:/Users/bngla/Documents/GitHub/QCEW",
  "Benjamin Glasner" = "C:/Users/Benjamin Glasner/Documents/GitHub/QCEW",
  "benjamin"        = "/Users/benjamin/Documents/GitHub/QCEW"
)

current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop(paste0(
    "Root folder for current user '", current_user,
    "' is not defined in project_directories (code/00_setup.R)."
  ))
}

path_project   <- project_directories[[current_user]]
path_data      <- file.path(path_project, "data")
path_code      <- file.path(path_project, "code")
path_output    <- file.path(path_project, "output")
path_raw       <- file.path(path_data, "raw")
path_processed <- file.path(path_data, "processed")
path_figures   <- file.path(path_output, "figures")
path_tables    <- file.path(path_output, "tables")

for (d in c(path_processed, path_figures, path_tables)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

#############################################################
###                   QCEW Data Reader                    ###
#############################################################
# read_qcew_slice() -- CE-001 replacement for the removed blscrapeR::qcew_api().
#   Reads one BLS QCEW open-data "slice" CSV and returns it as a data.frame with the
#   same columns the pipeline expects (area_fips, own_code, agglvl_code, industry_code,
#   qtrly_estabs, month3_emplvl, avg_wkly_wage, oty_*_chg, oty_*_pct_chg).
#   Endpoint: https://data.bls.gov/cew/data/api/[YEAR]/[QTR]/[SLICE]/[CODE].csv
#   - slice_chr      : "area" or "industry" (matches qcew_api()'s slice argument)
#   - slice_code_chr : the area FIPS (e.g. "US000", "53000") or industry code (e.g. "10", "54")
#   Files are cached under data/raw/qcew_slices/ so re-runs do not re-download. Keep
#   area_fips / industry_code as character to preserve leading zeros (county GEOID join)
#   and hyphenated NAICS codes ("31-33"). Download errors propagate to the caller's
#   tryCatch(), which logs a warning and skips that year-quarter.
read_qcew_slice <- function(year_int, qtr_int, slice_chr, slice_code_chr,
                            cache_dir = file.path(path_raw, "qcew_slices")) {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE)
  cache_file <- file.path(
    cache_dir,
    paste0("qcew_", year_int, "_q", qtr_int, "_", slice_chr, "_", slice_code_chr, ".csv")
  )
  if (!file.exists(cache_file)) {
    url_chr <- paste0("https://data.bls.gov/cew/data/api/",
                      year_int, "/", qtr_int, "/", slice_chr, "/", slice_code_chr, ".csv")
    tmp_chr <- paste0(cache_file, ".tmp")
    utils::download.file(url_chr, tmp_chr, mode = "wb", quiet = TRUE)
    file.rename(tmp_chr, cache_file)
  }
  read.csv(cache_file,
           colClasses = c(area_fips = "character", industry_code = "character"),
           stringsAsFactors = FALSE)
}

#############################################################
###                  Analysis Parameters                  ###
#############################################################
# 3) Single source of truth for the target release and display choices.
#    To refresh the analysis to a new QCEW release, change only this block.
target_year_int    <- 2025L   # most recent year with QCEW data
target_qtr_int     <- 4L      # most recent quarter available (1:4)
lookback_years_int <- 4L      # years of history to pull (target year and the prior N-1)

pct_cap_num     <- 25         # +/- cap on the map color scale (percent), for readability
pop_threshold_int <- 100000L  # minimum county population for ranking charts
counties_geo_year_int <- 2024L # cartographic-boundary vintage for tigris::counties()

# Over-the-year industry slope charts compare the target year against the prior year.
slope_years_int <- c(target_year_int - 1L, target_year_int)

# file_date string used in every output filename (e.g. "2025_qrt4")
file_date_chr <- paste0(target_year_int, "_qrt", target_qtr_int)

#############################################################
###                    EIG Style System                   ###
#############################################################
# 3b) Source the canonical EIG theme + tokens (provided in Infrastructure/style/themes/r/).
#     Provides eig_theme_ggplot(), eig_scale_color(), eig_scale_fill(), and the token
#     palette. Figures use these instead of ad hoc colors/themes (EIG figure rules).
eig_theme_dir_chr <- file.path(path_project, "Infrastructure", "style", "themes", "r")
source(file.path(eig_theme_dir_chr, "eig_theme.R"))
eig_tokens <- eig_load_tokens(file.path(eig_theme_dir_chr, "eig_tokens.R"))
eig_assert_fonts(eig_tokens, allow_fallback = TRUE)

# Named token colors used across figures (diverging maps + ranking direction).
# Per eig-figure-style.md: diverging = Forest Green (positive) -> neutral -> Terra Cotta
# (negative); avoid pure red/green. Growth = forest green, reduction = terra cotta.
eig_pos_chr <- unname(eig_tokens$EIG_COLORS[["eig_green_700"]])  # #19644D positive / growth
eig_neg_chr <- unname(eig_tokens$EIG_COLORS[["eig_tan_500"]])    # #D6936F negative / reduction
eig_mid_chr <- unname(eig_tokens$EIG_COLORS[["eig_cream_100"]])  # #FEECD6 neutral midpoint
eig_line_chr <- unname(eig_tokens$EIG_COLORS[["eig_green_700"]]) # default single-series line
eig_grid_chr <- "#D9D9D9"                                        # horizontal gridline gray
eig_source_chr <- paste0("Source: BLS QCEW, ", target_year_int, " Q", target_qtr_int, ".")  # dataset version

# Geographic spotlights (slice = "area" sliceCode). Add rows to extend.
# NOTE (2026-06-06): the legacy scale_div/scale_unit columns are dropped -- the small-
# multiples slope charts label each facet's endpoint directly, so no level rescaling is
# needed for readable axis breaks.
spotlight_areas_df <- data.frame(
  area_code   = c("53000", "23000"),
  area_label  = c("Washington", "Maine"),
  stringsAsFactors = FALSE
)

# Industry crosswalk: two-digit NAICS sector code -> abbreviated name
industry_crosswalk <- data.frame(
  industry_code = c("10", "11", "21", "22", "23",
                    "31-33", "42", "44-45", "48-49", "51",
                    "52", "53", "54", "55", "56",
                    "61", "62", "71", "72", "81",
                    "92", "99"),
  industry_name = c("All Industries", "Agriculture", "Mining", "Utilities", "Construction",
                    "Manufacturing", "Wholesale Trade", "Retail Trade", "Transp. and Ware.", "Information",
                    "Fin. and Ins.", "Real Estate", "Prof. Services", "Management", "Admin.",
                    "Education", "Health Care", "Arts", "Accommodation and Food", "Other",
                    "Public Admin.", "Nonclassifiable"),
  stringsAsFactors = FALSE
)

message("00_setup.R loaded | user = ", current_user,
        " | target = ", target_year_int, " Q", target_qtr_int,
        " | project = ", path_project)
