# Ben Glasner
# Quarterly Census of Employment and Wages Analysis Code

##################
###  Library   ###
##################
library(dplyr)
library(stargazer)
library(usmap)
library(ggplot2)
library(ggforce)
library(ggthemes)
library(scales)

# devtools::install_github("keberwein/blscrapeR", force = TRUE)
library(blscrapeR)

#################
### Set paths ###
#################
if(Sys.info()[["user"]]=="bglasner"){
  # Root folder
  path_project <- "C:/Users/bglasner/Dropbox"
}
if(Sys.info()[["user"]]=="bngla"){
  # Root folder
  path_project <- "C:/Users/bngla/Dropbox"
}
if(Sys.info()[["user"]]=="Benjamin Glasner"){
  # Root folder
  path_project <- "C:/Users/Benjamin Glasner/Dropbox"
}
# Path to saved cohort data 
path_qcew <- paste0(path_project,"/GitHub/QCEW")

setwd(path_qcew)

eig_color <- c("#024140","#E1AD28","#D6936F",
               "#5E9C86","#194F8B","#FFFFFF",
               "#19644d","#176F96","#00000",
               "#B3D6DD","#39274F","#FEECD6")

##################################

unemployment_rate <- quick_unemp_rate()
laborforce_rate <- quick_laborForce_rate()
employment_rate <- quick_employed_rate()

unemployment_rate <- unemployment_rate %>% 
  mutate(year_period = paste0(year,"-",periodName),
         group = "Unemployment Rate")

laborforce_rate <- laborforce_rate %>% 
  mutate(year_period = paste0(year,"-",periodName),
         group = "Labor Force Rate")

employment_rate <- employment_rate %>% 
  mutate(year_period = paste0(year,"-",periodName),
         group = "Employment Rate")

National <- rbind(unemployment_rate, employment_rate, laborforce_rate)

unemployment_rate %>% 
  ggplot(aes(x = year_period,
             y = value/100,
             group = group)) +
  geom_point() +
  geom_line() + 
  ylab("Unemployment Rate") +
  xlab("Year - Month") + coord_cartesian(ylim = c(0,.075)) +
  scale_y_continuous(labels = percent) + 
  theme_calc() +
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 11, angle = 90),
        axis.text.y = element_text(size = 11),
        axis.title = element_text(size = 12),
        title = element_text(size = 14))

qcew_2022_2 <- qcew_api(year = 2022,
         qtr="2", 
         slice="industry", # "industry", "area", or "size."
         sliceCode=10
         ) %>% mutate(fips = area_fips)

qcew_2022_2 <- within(qcew_2022_2, quantile <- as.integer(cut(oty_qtrly_contributions_pct_chg, unique(quantile(oty_qtrly_contributions_pct_chg, probs=seq(0,1,.01), na.rm = TRUE), include.lowest=TRUE))))
qcew_2022_2$quantile <- as.numeric(as.character(qcew_2022_2$quantile))

plot_usmap(data = qcew_2022_2,
           values = "quantile",
           include = c("CA"),
           regions = "counties") + 
  labs(title = "US Counties",
       subtitle = "This is a map of the change in Contributions") + 
  scale_fill_continuous(
    low = eig_color[1], high = eig_color[2], name = "quantile of the Change in Quarterly Contributions") + 
  theme(panel.background = element_rect(color = "black", fill = "white"),
        legend.position = "right")

qcew_2022_2 <- qcew_2022_2 %>% select(-quantile)
qcew_2022_2 <- within(qcew_2022_2, quantile <- as.integer(cut(oty_avg_wkly_wage_pct_chg, unique(quantile(oty_avg_wkly_wage_pct_chg, probs=seq(0,1,.01), na.rm = TRUE), include.lowest=TRUE))))
qcew_2022_2$quantile <- as.numeric(as.character(qcew_2022_2$quantile))

plot_usmap(data = qcew_2022_2,
           values = "quantile",
           include = c("CA"),
           regions = "counties") + 
  labs(title = "US Counties",
       subtitle = "This is a map of the change in wage") + 
  scale_fill_continuous(
    low = eig_color[1], high = eig_color[2], name = "quantile of the Change in wage") + 
  theme(panel.background = element_rect(color = "black", fill = "white"),
        legend.position = "right")
