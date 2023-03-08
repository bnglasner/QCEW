# Ben Glasner
# Quarterly Census of Employment and Wages Analysis Code

# Content:
# 1. Packages
# 2. Set Paths
# 3. Data Load
# 4. Figures 
#     1. Animated map of the US - (OTY) Employment changes (# and %)
#     2. Animated map of the US - (OTY) Establishment changes (# and %)
#     3. National level aggregate of all industries - Time series of employment and establishment growth rate
#     4. Industry level growth rates for establishments - Time Series
#     5. Industry level growth rates for employment - Time Series
#     6. Top and bottom five counties for employment growth
#     7. Top and bottom five counties for establishment growth
#     8. Top and bottom five counties for average weekly wage growth

##################
###  Options   ###
##################
options(scipen=100000)
file_date <- "2022_qrt3"
Most_recent_qrt <- 3 # This can take the values from 1:4
##################
###  Library   ###
##################
library(dplyr)
library(ggplot2)
library(ggforce)
library(ggthemes)
library(gganimate)
library(ggrepel)
library(gifski)
library(ggmap)
library(sf)
library(scales)
library(plm)
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


##################
###  Data Load ###
##################

########
# Build an INdustry cross walk with two-digit NAICS and abreviated industry name

industry_crosswalk <- as.data.frame(cbind(c("10","11","21","22","23",
                                            "31-33","42","44-45","48-49","51",
                                            "52","53","54","55","56",
                                            "61","62","71","72","81",
                                            "92","99"),
                                          c("All Industries","Agriculture","Mining","Utilities","Construction",
                                            "Manufacturing","Wholesale Trade","Retail Trade","Transp. and Ware.","Information",
                                            "Fin. and Ins.","Real Estate","Prof. Services","Management","Admin.",
                                            "Education","Health Care","Arts","Accommodation and Food","Other",
                                            "Public Admin.","Nonclassifiable")))
industry_crosswalk <- industry_crosswalk %>% rename("industry_code" = "V1",
                                                    "industry_name" = "V2")

########
# load County lat/long data for the animated US map
us_county_latlng <- read.csv("us_county_latlng.txt")
us_county_latlng <- us_county_latlng %>% rename("fips" = "fips_code")

########
# load a map of the US counties, but exclude Alaska and Hawaii for ease of presentation - Sorry for the exclusion. You are both great states.
counties <- tigris::counties(cb=TRUE,resolution = "500k", year = 2021)
counties <- counties %>% filter(as.numeric(as.character(STATEFP))<=56 & STATEFP!="02" & STATEFP!="15")

########
# Load county level population data. 
# This was the easiest way I could think of building a populaiton API that would also let me/people load alternive county info easily
population <- tidycensus::get_estimates(geography = "county",
                                        product = "characteristics",
                                        breakdown = c("SEX"),
                                        breakdown_labels = TRUE) %>%
  filter(SEX == "Both sexes") %>%
  rename("area_fips" = "GEOID",
         "population" = "value")

########
# QCEW Data Pull
# https://www.bls.gov/cew/about-data/downloadable-file-layouts/quarterly/naics-based-quarterly-layout.htm

industry_2022 <- list()
industry_2021 <- list()
industry_2020 <- list()
industry_2019 <- list()
industry_2018 <- list()

national_2022 <- list()
national_2021 <- list()
national_2020 <- list()
national_2019 <- list()
national_2018 <- list()

QCEW_industry_list <- list()
QCEW_national_list <- list()

# Industry level pull 
for(i in 1:Most_recent_qrt){
  industry_2022[[i]] <- qcew_api(year = 2022,
                                 qtr=paste(i), 
                                 slice="industry", # "industry", "area", or "size."
                                 sliceCode=10) %>% 
    mutate(fips = area_fips) %>% 
    filter(own_code == 5)
}
for(i in 1:4){
  industry_2021[[i]] <- qcew_api(year = 2021,
                                 qtr=paste(i), 
                                 slice="industry", # "industry", "area", or "size."
                                 sliceCode=10) %>% 
    mutate(fips = area_fips) %>% 
    filter(own_code == 5)
}
for(i in 1:4){
  industry_2020[[i]] <- qcew_api(year = 2020,
                                 qtr=paste(i), 
                                 slice="industry", # "industry", "area", or "size."
                                 sliceCode=10) %>% 
    mutate(fips = area_fips) %>% 
    filter(own_code == 5)
}
for(i in 1:4){
  industry_2019[[i]] <- qcew_api(year = 2019,
                                 qtr=paste(i), 
                                 slice="industry", # "industry", "area", or "size."
                                 sliceCode=10) %>% 
    mutate(fips = area_fips) %>% 
    filter(own_code == 5)
}
for(i in 1:4){
  industry_2018[[i]] <- qcew_api(year = 2018,
                                 qtr=paste(i), 
                                 slice="industry", # "industry", "area", or "size."
                                 sliceCode=10) %>% 
    mutate(fips = area_fips) %>% 
    filter(own_code == 5)
}

# National level pull
for(i in 1:Most_recent_qrt){
  national_2022[[i]] <- qcew_api(year = 2022,
                                 qtr=paste(i), 
                                 slice="area", # "industry", "area", or "size."
                                 sliceCode="US000") %>% # Consider Metro (USMSA) and nonmetro (USNMS)
    mutate(fips = area_fips) %>% 
    filter(own_code == 5)
}
for(i in 1:4){
  national_2021[[i]] <- qcew_api(year = 2021,
                                 qtr=paste(i), 
                                 slice="area", # "industry", "area", or "size."
                                 sliceCode="US000") %>% # Consider Metro (USMSA) and nonmetro (USNMS)
    mutate(fips = area_fips) %>% 
    filter(own_code == 5)
}
for(i in 1:4){
  national_2020[[i]] <- qcew_api(year = 2020,
                                 qtr=paste(i), 
                                 slice="area", # "industry", "area", or "size."
                                 sliceCode="US000") %>% # Consider Metro (USMSA) and nonmetro (USNMS)
    mutate(fips = area_fips) %>% 
    filter(own_code == 5)
}
for(i in 1:4){
  national_2019[[i]] <- qcew_api(year = 2019,
                                 qtr=paste(i), 
                                 slice="area", # "industry", "area", or "size."
                                 sliceCode="US000") %>% # Consider Metro (USMSA) and nonmetro (USNMS)
    mutate(fips = area_fips) %>% 
    filter(own_code == 5)
}
for(i in 1:4){
  national_2018[[i]] <- qcew_api(year = 2018,
                                 qtr=paste(i), 
                                 slice="area", # "industry", "area", or "size."
                                 sliceCode="US000") %>% # Consider Metro (USMSA) and nonmetro (USNMS)
    mutate(fips = area_fips) %>% 
    filter(own_code == 5)
}

# Combine the industry data
QCEW_industry_list[[1]] <- rbind(do.call(rbind, industry_2022),
                                 do.call(rbind, industry_2021),
                                 do.call(rbind, industry_2020),
                                 do.call(rbind, industry_2019))

QCEW_industry_list[[2]] <- rbind(do.call(rbind, industry_2022)) # Create data for the top and bottom five list figures
QCEW_industry_list[[2]]$state <- substr(QCEW_industry_list[[2]]$fips,start = 1, stop = 2)

# Combine the national data
QCEW_national_list[[1]] <- rbind(do.call(rbind, national_2022),
                                 do.call(rbind, national_2021),
                                 do.call(rbind, national_2020),
                                 do.call(rbind, national_2019))

QCEW_national_list[[2]] <- rbind(do.call(rbind, national_2022), # Create data for the industry level time series plots
                                 do.call(rbind, national_2021)) %>% 
  filter(agglvl_code==14 & industry_code!="10" & industry_code!="99") %>% 
  mutate(year_quarter_num = year + qtr/4) 

QCEW_national_list[[2]] <- left_join(QCEW_national_list[[2]],industry_crosswalk)
QCEW_national_list[[2]]$qtrly_estabs <- round(QCEW_national_list[[2]]$qtrly_estabs/1000,0)
QCEW_national_list[[2]]$month3_emplvl <- round(QCEW_national_list[[2]]$month3_emplvl/1000,0)

# Build the data for a map of the US - 
dynamic <- QCEW_industry_list[[1]] %>% 
  mutate(year_quarter = year + qtr/4,
         fips = as.numeric(as.character(fips))) %>% 
  select(area_fips,fips,year_quarter, # This is where you need to select the dependent variables for the map
         oty_month3_emplvl_chg,
         oty_month3_emplvl_pct_chg,
         oty_qtrly_estabs_chg,
         oty_qtrly_estabs_pct_chg
  ) %>% 
  na.omit() %>%
  distinct()  

dynamic <- left_join(dynamic,us_county_latlng)
dynamic <- dynamic %>%  rename("GEOID" = "area_fips")

dynamic <- left_join(counties, dynamic)

# Cap the visual % change for our two map dependent variables at 25% in either direction. This improves the figure's visual appeal
dynamic$oty_month3_emplvl_pct_chg_edit <- dynamic$oty_month3_emplvl_pct_chg
dynamic$oty_month3_emplvl_pct_chg_edit[dynamic$oty_month3_emplvl_pct_chg_edit>25] <- 25
dynamic$oty_month3_emplvl_pct_chg_edit[dynamic$oty_month3_emplvl_pct_chg_edit<(-25)] <- (-25)

dynamic$oty_qtrly_estabs_pct_chg_edit <- dynamic$oty_qtrly_estabs_chg
dynamic$oty_qtrly_estabs_pct_chg_edit[dynamic$oty_qtrly_estabs_pct_chg_edit>25] <- 25
dynamic$oty_qtrly_estabs_pct_chg_edit[dynamic$oty_qtrly_estabs_pct_chg_edit<(-25)] <- (-25)

###################
###  Figures    ###
###################
#####################################################################
#     1. Animated map of the US - (OTY) Employment changes (# and %)
plot <- dynamic %>% 
  ggplot() +
  labs(title = "Over-the-Year Change in Employment, Private Workers",
       subtitle = 'Year - {round(frame_time,1)}',
       caption = "Max % Change at +/- 25% for the Color Range") +
  geom_sf(color = "grey90",
          fill = "white") +
  geom_point(mapping = aes(x = lng,
                           y = lat,
                           size = abs(oty_month3_emplvl_chg),
                           color = oty_month3_emplvl_pct_chg_edit),
             alpha = 0.5) +
  scale_color_gradient2(name = "Percent Change", 
                        low = "red",
                        mid = "white",
                        high = "forestgreen",
                        midpoint = 0) +
  scale_size_continuous(labels = comma) +
  theme_map() +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5,
                                  color = "Gray40",
                                  size = 16,
                                  face = "bold"),
        plot.subtitle = element_text(color = "Gray40"),
        plot.caption = element_text(color = "Gray60")) +
  guides(color = guide_colorbar(title = "Change in Employment (%)",
                                title.position = "top",
                                title.theme = element_text(size = 10,
                                                           face = "bold",
                                                           colour = "gray70",
                                                           angle = 0),
                                order = 1),
         size = guide_legend(title = "Change in Employment (#)",
                             title.position = "top",
                             title.theme = element_text(size = 10,
                                                        face = "bold",
                                                        colour = "gray70",
                                                        angle = 0),
                             order = 2)) + 
  transition_time(year_quarter) +
  ease_aes('linear')

animate(plot, 
        height = 5, 
        width = 8.5, 
        units = "in", 
        res = 250, 
        end_pause = 50,
        renderer = gifski_renderer())

anim_save(filename = paste0("qrt_chng_emp_2019_qtr1_",file_date,".gif"))
#####################################################################
#     2. Animated map of the US - (OTY) Establishment changes (# and %)
plot <- dynamic %>% 
  ggplot() +
  labs(title = "Over-the-Year Change in Establishments, Private Workers",
       subtitle = 'Year - {round(frame_time,1)}',
       caption = "Max % Change at +/- 25% for the Color Range") +
  geom_sf(color = "grey90",
          fill = "white") +
  geom_point(mapping = aes(x = lng,
                           y = lat,
                           size = abs(oty_qtrly_estabs_chg),
                           color = oty_qtrly_estabs_pct_chg_edit),
             alpha = 0.5) +
  scale_color_gradient2(name = "Percent Change", 
                        low = "red",
                        mid = "white",
                        high = "forestgreen",
                        midpoint = 0) +
  scale_size_continuous(labels = comma) +
  theme_map() +
  theme(legend.position="bottom",
        plot.title = element_text(hjust = 0.5,
                                  color = "Gray40",
                                  size = 16,
                                  face = "bold"),
        plot.subtitle = element_text(color = "Gray40"),
        plot.caption = element_text(color = "Gray60")) +
  guides(color = guide_colorbar(title = "Change in Establishments (%)",
                                title.position = "top",
                                title.theme = element_text(size = 10,
                                                           face = "bold",
                                                           colour = "gray70",
                                                           angle = 0),
                                order = 1),
         size = guide_legend(title = "Change in Establishments (#)",
                             title.position = "top",
                             title.theme = element_text(size = 10,
                                                        face = "bold",
                                                        colour = "gray70",
                                                        angle = 0),
                             order = 2)) + 
  transition_time(year_quarter) +
  ease_aes('linear')

animate(plot, 
        height = 5, 
        width = 8.5, 
        units = "in", 
        res = 250, 
        end_pause = 50,
        renderer = gifski_renderer())

anim_save(filename = paste0("qrt_chng_estab_2019_qtr1_",file_date,".gif"))
#####################################################################
#     3. National level aggregate of all industries - Time series of employment and establishment growth rate
theme_set(theme_classic())

emp_chg <- QCEW_national_list[[1]] %>% 
  filter(agglvl_code==11 & industry_code=="10") %>%
  mutate(year_quarter = paste0(year,"-",qtr)) %>% 
  select(year_quarter,industry_code,oty_month3_emplvl_pct_chg) %>% 
  rename("value" = "oty_month3_emplvl_pct_chg") %>% 
  mutate(variable = "Over-the-Year Change in Quarterly Employment (%)")

estab_chg <- QCEW_national_list[[1]] %>% 
  filter(agglvl_code==11 & industry_code=="10") %>%
  mutate(year_quarter = paste0(year,"-",qtr)) %>% 
  select(year_quarter,industry_code,oty_qtrly_estabs_pct_chg) %>% 
  rename("value" = "oty_qtrly_estabs_pct_chg") %>% 
  mutate(variable = "Over-the-Year Change in Quarterly Estab. (%)")

national <- rbind(emp_chg,estab_chg)

P <- national %>% 
  ggplot(aes(x = year_quarter,
             y = value/100,
             group = industry_code)) + 
  geom_hline(yintercept = 0, color = "black") +
  geom_point() + 
  geom_line() + 
  labs(title = "Over-the-Year (%) Change in Employment and Establishments Across All Industries, Private Workers") +
  xlab("Year-Quarter") +
  ylab("% Change") +
  scale_y_continuous(labels = percent) +
  theme(legend.position = "right", 
        plot.title = element_text(size=14), 
        legend.title = element_text(size=12),
        panel.background = element_rect(color = "transparent",
                                        fill = "transparent"),
        strip.background = element_rect(fill = "white"),
        panel.spacing = unit(2, "lines"),
        axis.text.x = element_text(angle = 90)) +
  facet_wrap(variable~.)

png(filename = paste0("National_change_num_and_perc_",file_date,".png"),width = 1000,height = 500)
plot(P)
dev.off()

#####################################################################
#     4. Industry level growth rates for establishments - Time Series
theme_set(theme_classic())

QCEW_national_list[[2]] %>% filter(year_quarter_num==max(year_quarter_num)) %>% filter(oty_qtrly_estabs_chg == max(oty_qtrly_estabs_chg)) %>% select(industry_code,industry_name,oty_qtrly_estabs_chg,oty_month3_emplvl_chg)

yvals <- round(QCEW_national_list[[2]]$qtrly_estabs[QCEW_national_list[[2]]$year_quarter_num == min(QCEW_national_list[[2]]$year_quarter_num)],0)
ylabs <- QCEW_national_list[[2]] %>% filter(year_quarter_num == min(year_quarter_num)) %>% 
  select(industry_name)
ylabs <- ylabs[,1]

df <- QCEW_national_list[[2]] %>% mutate(x=factor(year_quarter_num, levels=c(2021.25,2021.5,2021.75,2022,2022.25,2022.5), 
                                                  labels=c("2021-1","2021-2","2021-3","2021-4","2022-1","2022-2")), 
                                         y=qtrly_estabs,
                                         group = as.factor(industry_name)) %>% select(x,y,group)

P <- df %>% 
  ggplot(aes(x=x,
             y=y)) +
  geom_line(aes(group=group),colour="grey80") +
  geom_point(colour="white",size=8) +
  geom_text(aes(label=y), 
            size=3) +
  scale_y_continuous(name="", breaks=yvals, labels=ylabs) +
  labs(title="Quarterly Establishments (Thousands)") + 
  theme(axis.title=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5,
                                  face="bold"),
        axis.text = element_text(face="bold"))

png(filename = paste0("Industry_change_estab_",file_date,".png"),width = 1000 ,height = 1000)
plot(P)
dev.off()
#####################################################################
#     5. Industry level growth rates for employment - Time Series
QCEW_national_list[[2]] %>% filter(year_quarter_num==max(year_quarter_num)) %>% filter(oty_month3_emplvl_chg == max(oty_month3_emplvl_chg)) %>% select(industry_code,industry_name,oty_qtrly_estabs_chg,oty_month3_emplvl_chg)

yvals <- round(QCEW_national_list[[2]]$month3_emplvl[QCEW_national_list[[2]]$year_quarter_num == min(QCEW_national_list[[2]]$year_quarter_num)])
ylabs <- QCEW_national_list[[2]] %>% filter(year_quarter_num == min(year_quarter_num)) %>% 
  select(industry_name)
ylabs <- ylabs[,1]

df <- QCEW_national_list[[2]]  %>% mutate(x=factor(year_quarter_num, levels=c(2021.25,2021.5,2021.75,2022,2022.25,2022.5), 
                                                   labels=c("2021-1","2021-2","2021-3","2021-4","2022-1","2022-2")), 
                                          y=month3_emplvl,
                                          group = as.factor(industry_name)) %>% select(x,y,group)

P <- df %>% 
  ggplot(aes(x=x,
             y=y)) +
  geom_line(aes(group=group),colour="grey80") +
  geom_point(colour="white",size=8) +
  geom_text(aes(label=y), 
            size=3) +
  scale_y_continuous(name="", breaks=yvals, labels=ylabs) +
  labs(title="Employment in Month Three (Thousands)") + 
  theme(axis.title=element_blank(),
        axis.ticks = element_blank(),
        plot.title = element_text(hjust=0.5,
                                  face="bold"),
        axis.text = element_text(face="bold"))


png(filename = paste0("Industry_change_emp_",file_date,".png"),width = 1000,height = 1000)
plot(P)
dev.off()

#####################################################################
#     6. Top and bottom five counties for employment growth
theme_set(theme_classic())

lowest <- QCEW_industry_list[[2]] %>% 
  mutate(fips = as.numeric(as.character(fips)),
         year_quarter = year + qtr/4) %>%
  left_join(population) %>% 
  filter(population>=100000 & year_quarter==max(year_quarter)) %>%
  arrange(oty_month3_emplvl_pct_chg) %>%
  select(NAME,population,month3_emplvl,oty_month3_emplvl_chg,oty_month3_emplvl_pct_chg) %>% 
  na.omit() %>% 
  head(5) %>%
  mutate(Change = "Over-the-Year Reduction")

highest <- QCEW_industry_list[[2]] %>% 
  mutate(fips = as.numeric(as.character(fips)),
         year_quarter = year + qtr/4) %>%
  left_join(population) %>% 
  filter(population>=100000 & year_quarter==max(year_quarter)) %>%
  arrange(-oty_month3_emplvl_pct_chg) %>%
  select(NAME,population,month3_emplvl,oty_month3_emplvl_chg,oty_month3_emplvl_pct_chg) %>% 
  na.omit() %>% 
  head(5) %>%
  mutate(Change = "Over-the-Year Growth")

change <- rbind(lowest,highest) %>%
  mutate(NAME = forcats::fct_reorder(NAME, oty_month3_emplvl_pct_chg),
         Label_positive = paste0(round(oty_month3_emplvl_chg)," = ",oty_month3_emplvl_pct_chg,"%"),
         Label_negative = paste0(round(oty_month3_emplvl_chg)," = ",oty_month3_emplvl_pct_chg,"%")) 
change$Label_positive[change$oty_month3_emplvl_pct_chg<0] <- NA 
change$Label_negative[change$oty_month3_emplvl_pct_chg>0] <- NA 

max_range <- max(abs(change$oty_month3_emplvl_pct_chg))

P <- change %>%  
  ggplot(aes(x=NAME, y=oty_month3_emplvl_pct_chg, label=oty_month3_emplvl_pct_chg)) + 
  geom_bar(stat='identity', aes(fill=Change), width=.5)  +
  geom_text(aes(label=Label_positive), 
            size=3,
            hjust = "top") +
  geom_text(aes(label=Label_negative), 
            size=3,
            hjust = "bottom") +
  scale_fill_manual(name="Change", 
                    values = c("Over-the-Year Growth"="#00ba38",
                               "Over-the-Year Reduction"="#f8766d")) + 
  labs(title= "Over-the-Year % Change in Employment",
       subtitle="Highs and Lows for counties of at least 100,000 People"
  ) + 
  ylab("Percent Change") +
  xlab("") +
  scale_y_continuous(limits = c(-max_range,max_range)) +
  coord_flip()

png(filename = paste0("high_low_emp_",file_date,".png"),width = 750,height = 500)
plot(P)
dev.off()
#####################################################################
#     7. Top and bottom five counties for establishment growth
theme_set(theme_classic())
lowest <- QCEW_industry_list[[2]] %>% 
  mutate(fips = as.numeric(as.character(fips)),
         year_quarter = year + qtr/4) %>%
  left_join(population) %>% 
  filter(population>=100000 & year_quarter==max(year_quarter) & state!="53") %>%
  arrange(oty_qtrly_estabs_pct_chg) %>%
  select(NAME,population,month3_emplvl,oty_qtrly_estabs_chg,oty_qtrly_estabs_pct_chg) %>% 
  na.omit() %>% 
  head(5) %>%
  mutate(Change = "Over-the-Year Reduction")

highest <- QCEW_industry_list[[2]] %>% 
  mutate(fips = as.numeric(as.character(fips)),
         year_quarter = year + qtr/4) %>%
  left_join(population) %>% 
  filter(population>=100000 & year_quarter==max(year_quarter) & state!="53") %>%
  arrange(-oty_qtrly_estabs_pct_chg) %>%
  select(NAME,population,month3_emplvl,oty_qtrly_estabs_chg,oty_qtrly_estabs_pct_chg) %>% 
  na.omit() %>% 
  head(5) %>%
  mutate(Change = "Over-the-Year Growth")

change <- rbind(lowest,highest) %>%
  mutate(NAME = forcats::fct_reorder(NAME, oty_qtrly_estabs_pct_chg),
         Label_positive = paste0(round(oty_qtrly_estabs_chg)," = ",oty_qtrly_estabs_pct_chg,"%"),
         Label_negative = paste0(round(oty_qtrly_estabs_chg)," = ",oty_qtrly_estabs_pct_chg,"%")) 
change$Label_positive[change$oty_qtrly_estabs_pct_chg<0] <- NA 
change$Label_negative[change$oty_qtrly_estabs_pct_chg>0] <- NA 

max_range <- max(abs(change$oty_qtrly_estabs_pct_chg))

P <- change %>%  
  ggplot(aes(x=NAME, y=oty_qtrly_estabs_pct_chg, label=oty_qtrly_estabs_pct_chg)) + 
  geom_bar(stat='identity', aes(fill=Change), width=.5)  +
  geom_text(aes(label=Label_positive), 
            size=3,
            hjust = "top") +
  geom_text(aes(label=Label_negative), 
            size=3,
            hjust = "bottom") +
  scale_fill_manual(name="Change", 
                    values = c("Over-the-Year Growth"="#00ba38",
                               "Over-the-Year Reduction"="#f8766d")) + 
  labs(title= "Over-the-Year % Change in Establishments",
       subtitle="Highs and Lows for counties of at least 100,000 People"
  ) + 
  ylab("Percent Change") +
  xlab("") +
  scale_y_continuous(limits = c(-max_range,max_range)) +
  coord_flip()

png(filename = paste0("high_low_estab_",file_date,".png"),width = 750,height = 500)
plot(P)
dev.off()
#####################################################################
#     8. Top and bottom five counties for average weekly wage growth
theme_set(theme_classic())

lowest <- QCEW_industry_list[[2]] %>% 
  mutate(fips = as.numeric(as.character(fips)),
         year_quarter = year + qtr/4) %>%
  left_join(population) %>% 
  filter(population>=100000 & year_quarter==max(year_quarter)) %>%
  arrange(oty_avg_wkly_wage_pct_chg) %>%
  select(NAME,population,month3_emplvl,oty_avg_wkly_wage_chg,oty_avg_wkly_wage_pct_chg) %>% 
  na.omit() %>% 
  head(5) %>%
  mutate(Change = "Over-the-Year Reduction")

highest <- QCEW_industry_list[[2]] %>% 
  mutate(fips = as.numeric(as.character(fips)),
         year_quarter = year + qtr/4) %>%
  left_join(population) %>% 
  filter(population>=100000 & year_quarter==max(year_quarter)) %>%
  arrange(-oty_avg_wkly_wage_pct_chg) %>%
  select(NAME,population,month3_emplvl,oty_avg_wkly_wage_chg,oty_avg_wkly_wage_pct_chg) %>% 
  na.omit() %>% 
  head(5) %>%
  mutate(Change = "Over-the-Year Growth")

change <- rbind(lowest,highest) %>%
  mutate(NAME = forcats::fct_reorder(NAME, oty_avg_wkly_wage_pct_chg),
         Label_positive = paste0(round(oty_avg_wkly_wage_chg)," = ",oty_avg_wkly_wage_pct_chg,"%"),
         Label_negative = paste0(round(oty_avg_wkly_wage_chg)," = ",oty_avg_wkly_wage_pct_chg,"%")) 
change$Label_positive[change$oty_avg_wkly_wage_pct_chg<0] <- NA 
change$Label_negative[change$oty_avg_wkly_wage_pct_chg>0] <- NA 

max_range <- max(abs(change$oty_avg_wkly_wage_pct_chg))

P <- change %>%  
  ggplot(aes(x=NAME, y=oty_avg_wkly_wage_pct_chg, label=oty_avg_wkly_wage_pct_chg)) + 
  geom_bar(stat='identity', aes(fill=Change), width=.5)  +
  geom_text(aes(label=Label_positive), 
            size=3,
            hjust = "top") +
  geom_text(aes(label=Label_negative), 
            size=3,
            hjust = "bottom") +
  scale_fill_manual(name="Change", 
                    values = c("Over-the-Year Growth"="#00ba38",
                               "Over-the-Year Reduction"="#f8766d")) + 
  labs(title= "Over-the-Year % Change in Average Weekly Wage",
       subtitle="Highs and Lows for counties of at least 100,000 People"
  ) + 
  ylab("Percent Change") +
  xlab("") +
  scale_y_continuous(limits = c(-max_range,max_range)) +
  coord_flip()

png(filename = paste0("high_low_weeklywage_",file_date,".png"),width = 750,height = 500)
plot(P)
dev.off()