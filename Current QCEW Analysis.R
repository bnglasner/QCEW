# Ben Glasner
# Quarterly Census of Employment and Wages Analysis Code

##################
###  Library   ###
##################
library(dplyr)
library(usmap)
library(ggplot2)
library(ggforce)
library(ggthemes)
library(gganimate)
library(gifski)
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

##################################
## QCEW Data Pull

data_2022 <- list()
data_2021 <- list()
data_2020 <- list()
data_2019 <- list()
data_2018 <- list()

for(i in 1:2){
  data_2022[[i]] <- qcew_api(year = 2022,
                             qtr=paste(i), 
                             slice="industry", # "industry", "area", or "size."
                             sliceCode=10) %>% 
    mutate(fips = area_fips) %>% 
    filter(own_code == 5)
}
for(i in 1:4){
  data_2021[[i]] <- qcew_api(year = 2021,
                             qtr=paste(i), 
                             slice="industry", # "industry", "area", or "size."
                             sliceCode=10) %>% 
    mutate(fips = area_fips) %>% 
    filter(own_code == 5)
}
for(i in 1:4){
  data_2020[[i]] <- qcew_api(year = 2020,
                             qtr=paste(i), 
                             slice="industry", # "industry", "area", or "size."
                             sliceCode=10) %>% 
    mutate(fips = area_fips) %>% 
    filter(own_code == 5)
}
for(i in 1:4){
  data_2019[[i]] <- qcew_api(year = 2019,
                             qtr=paste(i), 
                             slice="industry", # "industry", "area", or "size."
                             sliceCode=10) %>% 
    mutate(fips = area_fips) %>% 
    filter(own_code == 5)
}
for(i in 1:4){
  data_2018[[i]] <- qcew_api(year = 2018,
                             qtr=paste(i), 
                             slice="industry", # "industry", "area", or "size."
                             sliceCode=10) %>% 
    mutate(fips = area_fips) %>% 
    filter(own_code == 5)
}

QCEW <- rbind(do.call(rbind, data_2022),
      do.call(rbind, data_2021),
      do.call(rbind, data_2020),
      do.call(rbind, data_2019),
      do.call(rbind, data_2018))

##########################################################################
# Animated map of establishments
# oty_qtrly_estabs_pct_chg, oty_taxable_qtrly_wages_pct_chg, oty_qtrly_contributions_pct_chg
quant_01 <- as.numeric(quantile(QCEW$oty_qtrly_estabs_pct_chg,probs = .01))
quant_99 <- as.numeric(quantile(QCEW$oty_qtrly_estabs_pct_chg,probs = .99))

QCEW$oty_qtrly_estabs_pct_chg[QCEW$oty_qtrly_estabs_pct_chg<quant_01 | QCEW$oty_qtrly_estabs_pct_chg>quant_99] <- 0

dynamic <- QCEW %>% mutate(year_quarter = year + qtr/4) %>% 
                        select(fips,year_quarter,
                               oty_qtrly_estabs_pct_chg) %>% 
                        na.omit() %>%
                        distinct()  

plot <- plot_usmap(data = dynamic, 
                   values = "oty_qtrly_estabs_pct_chg", 
                   size = .1)  + 
  scale_fill_gradientn(name    = "Percent Change", 
                       colours = c("red", "white", "forestgreen"),
                       breaks  = c(quant_01, 0, quant_99)) +
  theme(legend.position = "right", 
        plot.title = element_text(size=14), 
        legend.title = element_text(size=12)) +
  labs(title = "Over-the-Year Percent Change in Quarterly Establishments, Private Workers",
       subtitle = 'Year - {as.integer(frame_time)}') +
  transition_time(year_quarter) +
  ease_aes('linear')

animate(plot, 
        height = 5, 
        width = 8.5, 
        units = "in", 
        res = 150, 
        end_pause = 10,
        renderer = gifski_renderer())

anim_save(filename = "qrt_chng_estab_2018-1_2022-2.gif")
##########################################################################


# https://www.bls.gov/cew/about-data/downloadable-file-layouts/quarterly/naics-based-quarterly-layout.htm

# unemployment_rate <- quick_unemp_rate()
# laborforce_rate <- quick_laborForce_rate()
# employment_rate <- quick_employed_rate()
# 
# unemployment_rate <- unemployment_rate %>% 
#   mutate(year_period = paste0(year,"-",periodName),
#          group = "Unemployment Rate")
# 
# laborforce_rate <- laborforce_rate %>% 
#   mutate(year_period = paste0(year,"-",periodName),
#          group = "Labor Force Rate")
# 
# employment_rate <- employment_rate %>% 
#   mutate(year_period = paste0(year,"-",periodName),
#          group = "Employment Rate")
# 
# National <- rbind(unemployment_rate, employment_rate, laborforce_rate)
# 
# unemployment_rate %>% 
#   ggplot(aes(x = year_period,
#              y = value/100,
#              group = group)) +
#   geom_point() +
#   geom_line() + 
#   ylab("Unemployment Rate") +
#   xlab("Year - Month") + coord_cartesian(ylim = c(0,.075)) +
#   scale_y_continuous(labels = percent) + 
#   theme_calc() +
#   theme(legend.position = "bottom",
#         axis.text.x = element_text(size = 11, angle = 90),
#         axis.text.y = element_text(size = 11),
#         axis.title = element_text(size = 12),
#         title = element_text(size = 14))

##########################################################################