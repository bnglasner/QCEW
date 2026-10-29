# 06_figures_spotlight -- NAICS 54 county map and per-state industry spotlights
# Author - Ben Glasner
# research title - QCEW Employment & Establishment Dynamics
# research question - How are professional services (NAICS 54) and selected states'
#   industries changing over the year in the latest release?

rm(list = ls())
options(scipen = 999)
set.seed(42)

# 0) Load shared setup (packages, paths, parameters, EIG theme). Working dir = repo root.
source(file.path("code", "00_setup.R"))
suppressPackageStartupMessages(library(ragg))

# 1) Load the static county basemap from 01_build_dataset.R and derive centroids for the
#    point layer (same approach as 02_build_map_data.R; the static lat/long file is retired).
counties_geo <- readRDS(file.path(path_processed, "counties_geo.rds"))
suppressWarnings(centroid_sf <- st_point_on_surface(counties_geo))
county_centroids <- data.frame(
  GEOID = as.character(centroid_sf$GEOID),
  lng   = st_coordinates(centroid_sf)[, 1],
  lat   = st_coordinates(centroid_sf)[, 2],
  stringsAsFactors = FALSE
)

#############################################################
###   Spotlight A - NAICS 54 county establishment map     ###
#############################################################
# 2) Pull professional services (NAICS 54), county-by-industry (agglvl 74), target quarter
#    CE-001 migrated (2026-06-06): reads the BLS open-data slice via read_qcew_slice()
#    (defined in 00_setup.R), same as 01_build_dataset.R.
prof_df <- read_qcew_slice(target_year_int, target_qtr_int, "industry", "54") |>
  filter(own_code == 5, agglvl_code == 74)

map_prof_df <- prof_df |>
  rename("GEOID" = "area_fips") |>
  select(GEOID,
         oty_month3_emplvl_chg, oty_month3_emplvl_pct_chg,
         oty_qtrly_estabs_chg, oty_qtrly_estabs_pct_chg) |>
  na.omit() |>
  distinct() |>
  inner_join(county_centroids, by = "GEOID")

# Cap percent-change measures for color-scale readability
map_prof_df$oty_month3_emplvl_pct_chg_edit <- map_prof_df$oty_month3_emplvl_pct_chg
map_prof_df$oty_month3_emplvl_pct_chg_edit[map_prof_df$oty_month3_emplvl_pct_chg_edit > pct_cap_num] <- pct_cap_num
map_prof_df$oty_month3_emplvl_pct_chg_edit[map_prof_df$oty_month3_emplvl_pct_chg_edit < (-pct_cap_num)] <- (-pct_cap_num)

# refactor fix (2026-06-05): use the percent change (not the count change) for the cap.
map_prof_df$oty_qtrly_estabs_pct_chg_edit <- map_prof_df$oty_qtrly_estabs_pct_chg
map_prof_df$oty_qtrly_estabs_pct_chg_edit[map_prof_df$oty_qtrly_estabs_pct_chg_edit > pct_cap_num] <- pct_cap_num
map_prof_df$oty_qtrly_estabs_pct_chg_edit[map_prof_df$oty_qtrly_estabs_pct_chg_edit < (-pct_cap_num)] <- (-pct_cap_num)

map_caption_chr <- paste0("Note: color capped at +/-", pct_cap_num, "%. Dot area is proportional ",
                          "to the number of establishments changed.\n", eig_source_chr)

# EIG + Tufte restyle (2026-06-06): token diverging scale + honest area (scale_size_area).
plot_prof <- ggplot() +
  labs(title = "Figure 9. Over-the-year change in professional services (NAICS 54) establishments",
       subtitle = paste0(target_year_int, " Q", target_qtr_int, ", private workers"),
       caption = map_caption_chr) +
  geom_sf(data = counties_geo, color = "grey90", fill = "white") +
  geom_point(data = map_prof_df,
             mapping = aes(x = lng, y = lat,
                           size = abs(oty_qtrly_estabs_chg),
                           color = oty_qtrly_estabs_pct_chg_edit),
             alpha = 0.55) +
  scale_color_gradient2(name = "Change (%)",
                        low = eig_neg_chr, mid = eig_mid_chr, high = eig_pos_chr,
                        midpoint = 0) +
  scale_size_area(name = "Establishments changed (#)", max_size = 9, labels = comma) +
  theme_map() +
  theme(text = element_text(family = eig_tokens$EIG_FONT_BODY_PRIMARY),
        legend.position = "bottom",
        plot.title = element_text(family = eig_tokens$EIG_FONT_HEADLINE_PRIMARY, face = "bold",
                                  size = 12, color = eig_tokens$EIG_COLORS[["eig_black"]]),
        plot.subtitle = element_text(color = eig_tokens$EIG_COLORS[["eig_teal_900"]]),
        plot.caption = element_text(color = "#444444", hjust = 0)) +
  guides(color = guide_colorbar(title.position = "top", order = 1),
         size = guide_legend(title.position = "top", order = 2))

agg_png(filename = file.path(path_figures, paste0("qrt_chng_estab_NAICS54_", file_date_chr, ".png")),
        width = 1000, height = 780, res = 130)
print(plot_prof)
dev.off()
message("  saved NAICS 54 establishment map")

#############################################################
###   Spotlight B - Per-state industry small multiples    ###
#############################################################
# 3) For each spotlight area, pull state-by-industry (agglvl 54) over the slope years and
#    draw establishment and employment small-multiples charts (one mini-line per industry,
#    ordered by latest value, endpoint labeled). Areas are parameter-driven (00_setup.R).
#    Tufte redesign (2026-06-06): replaces the legacy white-dot slope charts.

# Inline helper expression style kept consistent with 04_figures_timeseries.R (no function).
for (s in seq_len(nrow(spotlight_areas_df))) {
  area_code_chr  <- spotlight_areas_df$area_code[s]
  area_label_chr <- spotlight_areas_df$area_label[s]
  message("  spotlight: ", area_label_chr, " (", area_code_chr, ")")

  # Pull this area across the slope years (target year truncated at target_qtr_int)
  area_grid_df <- do.call(rbind, lapply(slope_years_int, function(yr) {
    max_qtr <- if (yr == target_year_int) target_qtr_int else 4L
    data.frame(year_int = yr, qtr_int = seq_len(max_qtr))
  }))
  area_list <- vector("list", nrow(area_grid_df))
  for (i in seq_len(nrow(area_grid_df))) {
    yr <- area_grid_df$year_int[i]
    qt <- area_grid_df$qtr_int[i]
    area_list[[i]] <- tryCatch(
      read_qcew_slice(yr, qt, "area", area_code_chr) |>
        mutate(fips = area_fips) |>
        filter(own_code == 5),
      error = function(e) {
        message("    WARNING: pull failed for ", area_label_chr, " ", yr, " Q", qt,
                ": ", conditionMessage(e))
        NULL
      }
    )
  }

  area_industry <- bind_rows(area_list) |>
    filter(agglvl_code == 54, !industry_code %in% c("10", "99")) |>
    mutate(period_order_num = year * 10L + qtr,
           period_chr = paste0(year, "-", qtr)) |>
    left_join(industry_crosswalk, by = "industry_code")

  period_levels_chr <- area_industry |>
    distinct(period_order_num, period_chr) |>
    arrange(period_order_num) |>
    pull(period_chr)
  last_period_num <- max(area_industry$period_order_num)
  slope_xbreaks_chr <- period_levels_chr[c(1, length(period_levels_chr))]

  # Establishment small multiples
  df_estab <- area_industry |>
    transmute(industry_name, period = factor(period_chr, levels = period_levels_chr),
              period_order_num, y = qtrly_estabs)
  estab_order_chr <- df_estab |> filter(period_order_num == last_period_num) |>
    arrange(desc(y)) |> pull(industry_name)
  df_estab  <- df_estab |> mutate(industry_name = factor(industry_name, levels = estab_order_chr))
  estab_end <- df_estab |> filter(period_order_num == last_period_num)

  plot_estab <- ggplot(df_estab, aes(period, y, group = industry_name)) +
    geom_line(color = eig_line_chr, linewidth = 0.7) +
    geom_point(data = estab_end, color = eig_line_chr, size = 1.6) +
    geom_text(data = estab_end, aes(label = scales::comma(y)),
              family = eig_tokens$EIG_FONT_BODY_PRIMARY, size = 2.7, hjust = 1, vjust = -0.8) +
    facet_wrap(~ industry_name, scales = "free_y", ncol = 5) +
    scale_x_discrete(breaks = slope_xbreaks_chr) +
    scale_y_continuous(expand = expansion(mult = c(0.10, 0.20))) +
    labs(title = paste0("Over-the-year change in private establishments by industry, ", area_label_chr),
         subtitle = paste0("Quarterly establishments, ", min(slope_years_int), "–", max(slope_years_int)),
         x = NULL, y = NULL, caption = eig_source_chr) +
    eig_theme_ggplot(eig_tokens, base_size = 9) +
    theme(plot.title = element_text(size = 13),
          axis.text.y = element_blank(), panel.grid = element_blank(),
          axis.text.x = element_text(size = 6.5), panel.spacing.x = unit(1, "lines"),
          strip.text = element_text(face = "bold", size = 8))

  agg_png(filename = file.path(path_figures,
                               paste0("Industry_change_estab_", tolower(area_label_chr), "_", file_date_chr, ".png")),
          width = 1100, height = 850, res = 130)
  print(plot_estab)
  dev.off()

  # Employment small multiples
  df_emp <- area_industry |>
    transmute(industry_name, period = factor(period_chr, levels = period_levels_chr),
              period_order_num, y = month3_emplvl)
  emp_order_chr <- df_emp |> filter(period_order_num == last_period_num) |>
    arrange(desc(y)) |> pull(industry_name)
  df_emp  <- df_emp |> mutate(industry_name = factor(industry_name, levels = emp_order_chr))
  emp_end <- df_emp |> filter(period_order_num == last_period_num)

  plot_emp <- ggplot(df_emp, aes(period, y, group = industry_name)) +
    geom_line(color = eig_line_chr, linewidth = 0.7) +
    geom_point(data = emp_end, color = eig_line_chr, size = 1.6) +
    geom_text(data = emp_end, aes(label = scales::comma(y)),
              family = eig_tokens$EIG_FONT_BODY_PRIMARY, size = 2.7, hjust = 1, vjust = -0.8) +
    facet_wrap(~ industry_name, scales = "free_y", ncol = 5) +
    scale_x_discrete(breaks = slope_xbreaks_chr) +
    scale_y_continuous(expand = expansion(mult = c(0.10, 0.20))) +
    labs(title = paste0("Over-the-year change in private employment by industry, ", area_label_chr),
         subtitle = paste0("Month-three employment, ", min(slope_years_int), "–", max(slope_years_int)),
         x = NULL, y = NULL, caption = eig_source_chr) +
    eig_theme_ggplot(eig_tokens, base_size = 9) +
    theme(plot.title = element_text(size = 13),
          axis.text.y = element_blank(), panel.grid = element_blank(),
          axis.text.x = element_text(size = 6.5), panel.spacing.x = unit(1, "lines"),
          strip.text = element_text(face = "bold", size = 8))

  agg_png(filename = file.path(path_figures,
                               paste0("Industry_change_emp_", tolower(area_label_chr), "_", file_date_chr, ".png")),
          width = 1100, height = 850, res = 130)
  print(plot_emp)
  dev.off()
}

message("06_figures_spotlight.R complete")
