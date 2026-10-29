# 04_figures_timeseries -- national OTY series and industry-level slope charts
# Author - Ben Glasner
# research title - QCEW Employment & Establishment Dynamics
# research question - How are employment and establishments changing nationally and
#   by industry over the year in the latest QCEW release?

rm(list = ls())
options(scipen = 999)
set.seed(42)

# 0) Load shared setup (packages, paths, parameters, EIG theme). Working dir = repo root.
source(file.path("code", "00_setup.R"))
suppressPackageStartupMessages(library(ragg))

# 1) Load the national panel from 01_build_dataset.R
qcew_national_panel <- readRDS(file.path(path_processed, "qcew_national_panel.rds"))

#############################################################
###   Figure 3 - National OTY Employment & Establishments ###
#############################################################
# 2) All-industry national aggregate (agglvl 11, industry 10), long format for faceting
emp_chg_df <- qcew_national_panel |>
  filter(agglvl_code == 11, industry_code == "10") |>
  mutate(year_quarter = paste0(year, "-", qtr)) |>
  select(year_quarter, industry_code, oty_month3_emplvl_pct_chg) |>
  rename("value" = "oty_month3_emplvl_pct_chg") |>
  mutate(variable = "Employment")

estab_chg_df <- qcew_national_panel |>
  filter(agglvl_code == 11, industry_code == "10") |>
  mutate(year_quarter = paste0(year, "-", qtr)) |>
  select(year_quarter, industry_code, oty_qtrly_estabs_pct_chg) |>
  rename("value" = "oty_qtrly_estabs_pct_chg") |>
  mutate(variable = "Establishments")

national_long_df <- bind_rows(emp_chg_df, estab_chg_df) |>
  arrange(year_quarter)

# Year-only x ticks (first quarter of each year) to keep the axis legible
xq_levels_chr <- sort(unique(national_long_df$year_quarter))
xq_breaks_chr <- xq_levels_chr[grepl("-1$", xq_levels_chr)]

plot_national <- national_long_df |>
  ggplot(aes(x = year_quarter, y = value / 100, group = variable)) +
  geom_hline(yintercept = 0, color = eig_tokens$EIG_COLORS[["eig_black"]], linewidth = 0.3) +
  geom_line(color = eig_line_chr, linewidth = 0.8) +
  geom_point(color = eig_line_chr, size = 1.6) +
  facet_wrap(~ variable) +
  scale_y_continuous(labels = percent) +
  scale_x_discrete(breaks = xq_breaks_chr, labels = function(x) sub("-1$", "", x)) +
  labs(title = "Figure 3. Over-the-year change in employment and establishments, all private industries",
       x = NULL, y = "Over-the-year change (%)", caption = eig_source_chr) +
  eig_theme_ggplot(eig_tokens, base_size = 11) +
  theme(plot.title = element_text(size = 13),
        strip.text = element_text(face = "bold"),
        strip.background = element_blank(),
        panel.spacing = unit(1.5, "lines"))

agg_png(filename = file.path(path_figures, paste0("National_change_num_and_perc_", file_date_chr, ".png")),
        width = 1100, height = 500, res = 110)
print(plot_national)
dev.off()
message("  saved national time series")

#############################################################
###   Industry-level slope data (target vs prior year)    ###
#############################################################
# 3) National-by-industry rows (agglvl 14), excluding totals (10) and nonclassifiable (99)
national_industry <- qcew_national_panel |>
  filter(agglvl_code == 14, !industry_code %in% c("10", "99"), year %in% slope_years_int) |>
  mutate(period_order_num = year * 10L + qtr,
         period_chr = paste0(year, "-", qtr)) |>
  left_join(industry_crosswalk, by = "industry_code") |>
  mutate(qtrly_estabs_k = round(qtrly_estabs / 1000, 0),
         month3_emplvl_k = round(month3_emplvl / 1000, 0))

# Ordered period labels generated from the data (no hardcoded factor levels)
period_levels_chr <- national_industry |>
  distinct(period_order_num, period_chr) |>
  arrange(period_order_num) |>
  pull(period_chr)
last_period_num <- max(national_industry$period_order_num)
# Year-only x ticks for the small multiples (first and last period)
slope_xbreaks_chr <- period_levels_chr[c(1, length(period_levels_chr))]

#############################################################
###    Figure 4 - Industry Establishments small multiples ###
#############################################################
# Tufte redesign (2026-06-06): replaces the legacy single-panel "white-dot" slope chart
# (industry names crammed onto the y-axis -> label collisions) with faceted small multiples
# -- one mini-line per industry, ordered by latest value, endpoint labeled. No white dots.
df_estab <- national_industry |>
  transmute(industry_name, period = factor(period_chr, levels = period_levels_chr),
            period_order_num, y = qtrly_estabs_k)
estab_order_chr <- df_estab |> filter(period_order_num == last_period_num) |>
  arrange(desc(y)) |> pull(industry_name)
df_estab  <- df_estab |> mutate(industry_name = factor(industry_name, levels = estab_order_chr))
estab_end <- df_estab |> filter(period_order_num == last_period_num)

plot_estab_slope <- ggplot(df_estab, aes(period, y, group = industry_name)) +
  geom_line(color = eig_line_chr, linewidth = 0.7) +
  geom_point(data = estab_end, color = eig_line_chr, size = 1.6) +
  geom_text(data = estab_end, aes(label = scales::comma(y)),
            family = eig_tokens$EIG_FONT_BODY_PRIMARY, size = 2.7, hjust = 1, vjust = -0.8) +
  facet_wrap(~ industry_name, scales = "free_y", ncol = 5) +
  scale_x_discrete(breaks = slope_xbreaks_chr) +
  scale_y_continuous(expand = expansion(mult = c(0.10, 0.20))) +
  labs(title = "Figure 4. Over-the-year change in private establishments by industry",
       subtitle = paste0("Quarterly establishments (thousands), ", min(slope_years_int), "–", max(slope_years_int)),
       x = NULL, y = NULL, caption = eig_source_chr) +
  eig_theme_ggplot(eig_tokens, base_size = 9) +
  theme(axis.text.y = element_blank(), panel.grid = element_blank(),
        axis.text.x = element_text(size = 6.5), panel.spacing.x = unit(1, "lines"),
        strip.text = element_text(face = "bold", size = 8))

agg_png(filename = file.path(path_figures, paste0("Industry_change_estab_", file_date_chr, ".png")),
        width = 1100, height = 850, res = 130)
print(plot_estab_slope)
dev.off()
message("  saved industry establishment slope chart")

#############################################################
###     Figure 5 - Industry Employment small multiples    ###
#############################################################
df_emp <- national_industry |>
  transmute(industry_name, period = factor(period_chr, levels = period_levels_chr),
            period_order_num, y = month3_emplvl_k)
emp_order_chr <- df_emp |> filter(period_order_num == last_period_num) |>
  arrange(desc(y)) |> pull(industry_name)
df_emp  <- df_emp |> mutate(industry_name = factor(industry_name, levels = emp_order_chr))
emp_end <- df_emp |> filter(period_order_num == last_period_num)

plot_emp_slope <- ggplot(df_emp, aes(period, y, group = industry_name)) +
  geom_line(color = eig_line_chr, linewidth = 0.7) +
  geom_point(data = emp_end, color = eig_line_chr, size = 1.6) +
  geom_text(data = emp_end, aes(label = scales::comma(y)),
            family = eig_tokens$EIG_FONT_BODY_PRIMARY, size = 2.7, hjust = 1, vjust = -0.8) +
  facet_wrap(~ industry_name, scales = "free_y", ncol = 5) +
  scale_x_discrete(breaks = slope_xbreaks_chr) +
  scale_y_continuous(expand = expansion(mult = c(0.10, 0.20))) +
  labs(title = "Figure 5. Over-the-year change in private employment by industry",
       subtitle = paste0("Month-three employment (thousands), ", min(slope_years_int), "–", max(slope_years_int)),
       x = NULL, y = NULL, caption = eig_source_chr) +
  eig_theme_ggplot(eig_tokens, base_size = 9) +
  theme(axis.text.y = element_blank(), panel.grid = element_blank(),
        axis.text.x = element_text(size = 6.5), panel.spacing.x = unit(1, "lines"),
        strip.text = element_text(face = "bold", size = 8))

agg_png(filename = file.path(path_figures, paste0("Industry_change_emp_", file_date_chr, ".png")),
        width = 1100, height = 850, res = 130)
print(plot_emp_slope)
dev.off()
message("04_figures_timeseries.R complete")
