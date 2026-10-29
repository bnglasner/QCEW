# 03_figures_maps -- animated over-the-year county maps (employment, establishments)
# Author - Ben Glasner
# research title - QCEW Employment & Establishment Dynamics
# research question - How are employment and establishments changing across U.S.
#   counties over the year in the latest QCEW release?

rm(list = ls())
options(scipen = 999)
set.seed(42)

# 0) Load shared setup (packages, paths, parameters, EIG theme). Working dir = repo root.
source(file.path("code", "00_setup.R"))

# 1) Load the static county basemap (01) and the animated point data (02). The basemap is a
#    SEPARATE constant layer so the map outline does not change frame to frame (the legacy
#    design fed geom_sf from the per-quarter data, so Connecticut appeared/disappeared at the
#    2024 FIPS change). Only the points animate.
counties_geo     <- readRDS(file.path(path_processed, "counties_geo.rds"))
map_data_dynamic <- readRDS(file.path(path_processed, "map_data_dynamic.rds"))

first_year_int <- target_year_int - (lookback_years_int - 1L)
# Caption carries the methodological cap note and the EIG source line (kept out of the plot
# body per Tufte's collision test).
map_caption_chr <- paste0("Note: color capped at +/-", pct_cap_num, "%. Dot area is proportional ",
                          "to the number of jobs/establishments changed.\n", eig_source_chr)

#############################################################
###      Figure 1 - OTY Employment Change (animated)      ###
#############################################################
# EIG + Tufte restyle (2026-06-06): diverging scale is now token-based terra cotta
# (decline) -> cream (no change) -> forest green (growth), replacing the legacy red/white/
# green. Dot size uses scale_size_area() so AREA is proportional to magnitude (lie factor
# ~ 1), replacing scale_size_continuous() which mapped magnitude to radius.
plot_emp <- ggplot() +
  labs(title = "Figure 1. Over-the-year change in employment, private workers",
       subtitle = "Year: {round(frame_time, 1)}",
       caption = map_caption_chr) +
  geom_sf(data = counties_geo, color = "grey90", fill = "white") +
  geom_point(data = map_data_dynamic,
             mapping = aes(x = lng, y = lat,
                           size = abs(oty_month3_emplvl_chg),
                           color = oty_month3_emplvl_pct_chg_edit),
             alpha = 0.55) +
  scale_color_gradient2(name = "Change (%)",
                        low = eig_neg_chr, mid = eig_mid_chr, high = eig_pos_chr,
                        midpoint = 0) +
  scale_size_area(name = "Jobs changed (#)", max_size = 9, labels = comma) +
  theme_map() +
  theme(text = element_text(family = eig_tokens$EIG_FONT_BODY_PRIMARY),
        legend.position = "bottom",
        plot.title = element_text(family = eig_tokens$EIG_FONT_HEADLINE_PRIMARY,
                                  face = "bold", size = 15, color = eig_tokens$EIG_COLORS[["eig_black"]]),
        plot.subtitle = element_text(color = eig_tokens$EIG_COLORS[["eig_teal_900"]]),
        plot.caption = element_text(color = "#444444", hjust = 0)) +
  guides(color = guide_colorbar(title.position = "top", order = 1),
         size = guide_legend(title.position = "top", order = 2)) +
  transition_time(year_quarter) +
  ease_aes("linear")

animate(plot_emp, height = 5, width = 8.5, units = "in", res = 150,
        end_pause = 50, renderer = gifski_renderer())
anim_save(filename = file.path(path_figures,
                               paste0("qrt_chng_emp_", first_year_int, "_qtr1_", file_date_chr, ".gif")))
message("  saved employment animation")

#############################################################
###    Figure 2 - OTY Establishment Change (animated)     ###
#############################################################
plot_estab <- ggplot() +
  labs(title = "Figure 2. Over-the-year change in establishments, private workers",
       subtitle = "Year: {round(frame_time, 1)}",
       caption = map_caption_chr) +
  geom_sf(data = counties_geo, color = "grey90", fill = "white") +
  geom_point(data = map_data_dynamic,
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
        plot.title = element_text(family = eig_tokens$EIG_FONT_HEADLINE_PRIMARY,
                                  face = "bold", size = 15, color = eig_tokens$EIG_COLORS[["eig_black"]]),
        plot.subtitle = element_text(color = eig_tokens$EIG_COLORS[["eig_teal_900"]]),
        plot.caption = element_text(color = "#444444", hjust = 0)) +
  guides(color = guide_colorbar(title.position = "top", order = 1),
         size = guide_legend(title.position = "top", order = 2)) +
  transition_time(year_quarter) +
  ease_aes("linear")

animate(plot_estab, height = 5, width = 8.5, units = "in", res = 150,
        end_pause = 50, renderer = gifski_renderer())
anim_save(filename = file.path(path_figures,
                               paste0("qrt_chng_estab_", first_year_int, "_qtr1_", file_date_chr, ".gif")))
message("03_figures_maps.R complete")
