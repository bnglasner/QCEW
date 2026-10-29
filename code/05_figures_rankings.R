# 05_figures_rankings -- top/bottom five counties (employment, establishments, wages)
# Author - Ben Glasner
# research title - QCEW Employment & Establishment Dynamics
# research question - Which large counties saw the largest over-the-year gains and
#   losses in employment, establishments, and average weekly wages in the latest release?

rm(list = ls())
options(scipen = 999)
set.seed(42)

# 0) Load shared setup (packages, paths, parameters, EIG theme). Working dir = repo root.
source(file.path("code", "00_setup.R"))
suppressPackageStartupMessages(library(ragg))

# 1) Load checkpoints from 01_build_dataset.R
qcew_industry_panel <- readRDS(file.path(path_processed, "qcew_industry_panel.rds"))
county_population    <- readRDS(file.path(path_processed, "county_population.rds"))

# 2) Target-year county base, restricted to the most recent quarter and large counties
#    NOTE (refactor 2026-06-05): the legacy establishment chart hard-coded a `state != "53"`
#    (exclude Washington) filter with no explanation. It was specific to the 2022 Q3 story
#    and is NOT carried forward in this generalized pipeline. See review-reports/.
ranking_base <- qcew_industry_panel |>
  filter(year == target_year_int) |>
  mutate(state = substr(area_fips, 1, 2),
         year_quarter = year + qtr / 4) |>
  left_join(county_population, by = "area_fips") |>
  filter(population >= pop_threshold_int, year_quarter == max(year_quarter))

# EIG token fills: forest green = growth, terra cotta = reduction (avoid pure red/green).
ranking_fill_chr <- c("Over-the-Year Growth" = eig_pos_chr,
                      "Over-the-Year Reduction" = eig_neg_chr)

# Helper expression reused per chart to place value labels just outside each bar end.
# (Inline, not a function: two geom_text layers split by sign, as in the legacy script.)

#############################################################
###    Figure 6 - County Employment Growth (high/low)     ###
#############################################################
emp_lowest_df <- ranking_base |>
  arrange(oty_month3_emplvl_pct_chg) |>
  select(NAME, population, month3_emplvl, oty_month3_emplvl_chg, oty_month3_emplvl_pct_chg) |>
  na.omit() |>
  head(5) |>
  mutate(Change = "Over-the-Year Reduction")

emp_highest_df <- ranking_base |>
  arrange(-oty_month3_emplvl_pct_chg) |>
  select(NAME, population, month3_emplvl, oty_month3_emplvl_chg, oty_month3_emplvl_pct_chg) |>
  na.omit() |>
  head(5) |>
  mutate(Change = "Over-the-Year Growth")

emp_change_df <- bind_rows(emp_lowest_df, emp_highest_df) |>
  mutate(NAME = forcats::fct_reorder(NAME, oty_month3_emplvl_pct_chg),
         bar_label_chr = paste0(ifelse(oty_month3_emplvl_pct_chg >= 0, "+", ""),
                                oty_month3_emplvl_pct_chg, "% (",
                                scales::comma(round(oty_month3_emplvl_chg)), ")"))
emp_max_range_num <- max(abs(emp_change_df$oty_month3_emplvl_pct_chg))
# AS-004 guard (2026-06-06): chart expects 10 bars (top 5 + bottom 5). Warn if a sparse
# release leaves fewer non-missing large counties than expected.
if (nrow(emp_change_df) < 10) message("  AS-004: employment ranking has only ", nrow(emp_change_df), " bars (expected 10)")

plot_emp_rank <- emp_change_df |>
  ggplot(aes(x = NAME, y = oty_month3_emplvl_pct_chg, fill = Change)) +
  geom_col(width = 0.6) +
  geom_text(data = ~ subset(.x, oty_month3_emplvl_pct_chg >= 0),
            aes(label = bar_label_chr), hjust = -0.1, size = 3,
            family = eig_tokens$EIG_FONT_BODY_PRIMARY) +
  geom_text(data = ~ subset(.x, oty_month3_emplvl_pct_chg < 0),
            aes(label = bar_label_chr), hjust = 1.1, size = 3,
            family = eig_tokens$EIG_FONT_BODY_PRIMARY) +
  scale_fill_manual(values = ranking_fill_chr, guide = "none") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(-emp_max_range_num * 1.4, emp_max_range_num * 1.4)) +
  labs(title = "Figure 6. Over-the-year change in employment, largest highs and lows",
       subtitle = "Counties of at least 100,000 people, private workers",
       x = NULL, y = "Over-the-year change (%)", caption = eig_source_chr) +
  coord_flip() +
  eig_theme_ggplot(eig_tokens, base_size = 11) +
  theme(plot.title = element_text(size = 12), panel.grid.major.y = element_blank())

agg_png(filename = file.path(path_figures, paste0("high_low_emp_", file_date_chr, ".png")),
        width = 900, height = 520, res = 120)
print(plot_emp_rank)
dev.off()
message("  saved employment ranking chart")

#############################################################
###  Figure 7 - County Establishment Growth (high/low)    ###
#############################################################
estab_lowest_df <- ranking_base |>
  arrange(oty_qtrly_estabs_pct_chg) |>
  select(NAME, population, month3_emplvl, oty_qtrly_estabs_chg, oty_qtrly_estabs_pct_chg) |>
  na.omit() |>
  head(5) |>
  mutate(Change = "Over-the-Year Reduction")

estab_highest_df <- ranking_base |>
  arrange(-oty_qtrly_estabs_pct_chg) |>
  select(NAME, population, month3_emplvl, oty_qtrly_estabs_chg, oty_qtrly_estabs_pct_chg) |>
  na.omit() |>
  head(5) |>
  mutate(Change = "Over-the-Year Growth")

estab_change_df <- bind_rows(estab_lowest_df, estab_highest_df) |>
  mutate(NAME = forcats::fct_reorder(NAME, oty_qtrly_estabs_pct_chg),
         bar_label_chr = paste0(ifelse(oty_qtrly_estabs_pct_chg >= 0, "+", ""),
                                oty_qtrly_estabs_pct_chg, "% (",
                                scales::comma(round(oty_qtrly_estabs_chg)), ")"))
estab_max_range_num <- max(abs(estab_change_df$oty_qtrly_estabs_pct_chg))
if (nrow(estab_change_df) < 10) message("  AS-004: establishment ranking has only ", nrow(estab_change_df), " bars (expected 10)")

plot_estab_rank <- estab_change_df |>
  ggplot(aes(x = NAME, y = oty_qtrly_estabs_pct_chg, fill = Change)) +
  geom_col(width = 0.6) +
  geom_text(data = ~ subset(.x, oty_qtrly_estabs_pct_chg >= 0),
            aes(label = bar_label_chr), hjust = -0.1, size = 3,
            family = eig_tokens$EIG_FONT_BODY_PRIMARY) +
  geom_text(data = ~ subset(.x, oty_qtrly_estabs_pct_chg < 0),
            aes(label = bar_label_chr), hjust = 1.1, size = 3,
            family = eig_tokens$EIG_FONT_BODY_PRIMARY) +
  scale_fill_manual(values = ranking_fill_chr, guide = "none") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(-estab_max_range_num * 1.4, estab_max_range_num * 1.4)) +
  labs(title = "Figure 7. Over-the-year change in establishments, largest highs and lows",
       subtitle = "Counties of at least 100,000 people, private workers",
       x = NULL, y = "Over-the-year change (%)", caption = eig_source_chr) +
  coord_flip() +
  eig_theme_ggplot(eig_tokens, base_size = 11) +
  theme(plot.title = element_text(size = 12), panel.grid.major.y = element_blank())

agg_png(filename = file.path(path_figures, paste0("high_low_estab_", file_date_chr, ".png")),
        width = 900, height = 520, res = 120)
print(plot_estab_rank)
dev.off()
message("  saved establishment ranking chart")

#############################################################
###   Figure 8 - County Weekly Wage Growth (high/low)     ###
#############################################################
wage_lowest_df <- ranking_base |>
  arrange(oty_avg_wkly_wage_pct_chg) |>
  select(NAME, population, month3_emplvl, oty_avg_wkly_wage_chg, oty_avg_wkly_wage_pct_chg) |>
  na.omit() |>
  head(5) |>
  mutate(Change = "Over-the-Year Reduction")

wage_highest_df <- ranking_base |>
  arrange(-oty_avg_wkly_wage_pct_chg) |>
  select(NAME, population, month3_emplvl, oty_avg_wkly_wage_chg, oty_avg_wkly_wage_pct_chg) |>
  na.omit() |>
  head(5) |>
  mutate(Change = "Over-the-Year Growth")

wage_change_df <- bind_rows(wage_lowest_df, wage_highest_df) |>
  mutate(NAME = forcats::fct_reorder(NAME, oty_avg_wkly_wage_pct_chg),
         bar_label_chr = paste0(ifelse(oty_avg_wkly_wage_pct_chg >= 0, "+", ""),
                                oty_avg_wkly_wage_pct_chg, "% ($",
                                scales::comma(round(oty_avg_wkly_wage_chg)), ")"))
wage_max_range_num <- max(abs(wage_change_df$oty_avg_wkly_wage_pct_chg))
if (nrow(wage_change_df) < 10) message("  AS-004: wage ranking has only ", nrow(wage_change_df), " bars (expected 10)")

plot_wage_rank <- wage_change_df |>
  ggplot(aes(x = NAME, y = oty_avg_wkly_wage_pct_chg, fill = Change)) +
  geom_col(width = 0.6) +
  geom_text(data = ~ subset(.x, oty_avg_wkly_wage_pct_chg >= 0),
            aes(label = bar_label_chr), hjust = -0.1, size = 3,
            family = eig_tokens$EIG_FONT_BODY_PRIMARY) +
  geom_text(data = ~ subset(.x, oty_avg_wkly_wage_pct_chg < 0),
            aes(label = bar_label_chr), hjust = 1.1, size = 3,
            family = eig_tokens$EIG_FONT_BODY_PRIMARY) +
  scale_fill_manual(values = ranking_fill_chr, guide = "none") +
  scale_y_continuous(labels = function(x) paste0(x, "%"),
                     limits = c(-wage_max_range_num * 1.4, wage_max_range_num * 1.4)) +
  labs(title = "Figure 8. Over-the-year change in average weekly wage, largest highs and lows",
       subtitle = "Counties of at least 100,000 people, private workers",
       x = NULL, y = "Over-the-year change (%)", caption = eig_source_chr) +
  coord_flip() +
  eig_theme_ggplot(eig_tokens, base_size = 11) +
  theme(plot.title = element_text(size = 12), panel.grid.major.y = element_blank())

agg_png(filename = file.path(path_figures, paste0("high_low_weeklywage_", file_date_chr, ".png")),
        width = 900, height = 520, res = 120)
print(plot_wage_rank)
dev.off()
message("05_figures_rankings.R complete")
