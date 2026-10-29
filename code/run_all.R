# run_all -- orchestrate the QCEW pipeline end to end
# Author - Ben Glasner
# research title - QCEW Employment & Establishment Dynamics
# research question - How are employment, establishments, and average weekly wages
#   changing across U.S. counties and industries over the year in the latest QCEW release?
# outline - set paths/params (00_setup), then run selected scripts in fresh environments

rm(list = ls())
options(scipen = 999)
set.seed(42)

#############################################################
###                  Paths and Parameters                ###
#############################################################
# 1) Resolve the project root for the current user, then move there so that each
#    script's relative source("code/00_setup.R") and read/write paths resolve.
project_directories <- list(
  "bglasner"         = "C:/Users/bglasner/Documents/GitHub/QCEW",
  "bngla"            = "C:/Users/bngla/Documents/GitHub/QCEW",
  "Benjamin Glasner" = "C:/Users/Benjamin Glasner/Documents/GitHub/QCEW",
  "benjamin"         = "/Users/benjamin/Documents/GitHub/QCEW"
)
current_user <- Sys.info()[["user"]]
if (!current_user %in% names(project_directories)) {
  stop(paste0("Root folder for current user '", current_user,
              "' is not defined in project_directories (code/run_all.R and code/00_setup.R)."))
}
setwd(project_directories[[current_user]])

#############################################################
###                     Script Flags                      ###
#############################################################
# 2) Toggle which steps run. 01 and 02 build the data the figure scripts read.
run_01_build_dataset   <- TRUE   # pull QCEW, population, geometry; write checkpoints
run_02_build_map_data  <- TRUE   # assemble the county map dataset
run_03_figures_maps    <- TRUE   # animated OTY county maps
run_04_figures_timeseries <- TRUE # national + industry slope charts
run_05_figures_rankings   <- TRUE # top/bottom-five county charts
run_06_figures_spotlight  <- TRUE # NAICS 54 map + per-state spotlights

#############################################################
###                      Run Scripts                      ###
#############################################################
# 3) Build the ordered list of scripts to source, honoring the flags above
scripts_to_run <- character(0)
if (run_01_build_dataset)     scripts_to_run <- c(scripts_to_run, "01_build_dataset.R")
if (run_02_build_map_data)    scripts_to_run <- c(scripts_to_run, "02_build_map_data.R")
if (run_03_figures_maps)      scripts_to_run <- c(scripts_to_run, "03_figures_maps.R")
if (run_04_figures_timeseries) scripts_to_run <- c(scripts_to_run, "04_figures_timeseries.R")
if (run_05_figures_rankings)  scripts_to_run <- c(scripts_to_run, "05_figures_rankings.R")
if (run_06_figures_spotlight) scripts_to_run <- c(scripts_to_run, "06_figures_spotlight.R")

# 4) Source each script in a fresh environment so objects do not leak between steps.
#    Scripts communicate only through the checkpoints written to data/processed.
if (length(scripts_to_run) == 0) {
  message("run_all.R: no scripts selected. Nothing to do.")
} else {
  for (script_name in scripts_to_run) {
    script_path <- file.path("code", script_name)
    if (!file.exists(script_path)) {
      stop(paste0("Script not found: ", script_path))
    }
    message("=== Running: ", script_name, " ===")
    script_start <- Sys.time()
    tryCatch(
      sys.source(script_path, envir = new.env(parent = globalenv())),
      error = function(e) {
        stop(paste0("ERROR in ", script_name, ": ", conditionMessage(e)))
      }
    )
    elapsed_num <- round(as.numeric(difftime(Sys.time(), script_start, units = "secs")), 2)
    message("=== Completed: ", script_name, " (", elapsed_num, " seconds) ===")
  }
  message("run_all.R finished.")
}
