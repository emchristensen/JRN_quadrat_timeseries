#' This script looks at trends of grass on individual quadrats during important time periods
#' Both total grass and most important grass species
#' Time periods: 1945-1955 (to capture drought)
#'               1955-1980 (to capture any recovery)
#'               1995-2016 (to describe current dynamics)
#' This analysis uses Theil-Sen slope with Kendall's tau test for significance to describe trends
#' 
#' EMC 9/3/20
#' Info on sens.slope: https://cran.r-project.org/web/packages/trend/vignettes/trend.pdf

library(dplyr)
library(ggplot2)
library(trend)

source('data_functions.R')

grasstotals = read.csv('data/grass_species_totals.csv', stringsAsFactors = F)
dates = read.csv('data/quadrats_dates_for_analysis.csv', stringsAsFactors = F)
#quadtype = read.csv('data/quad_type_coordinate.csv', stringsAsFactors = F) %>%
#  dplyr::select(quadrat, vegtype, upland_byspecies)

# quadrats to be used in analysis
quads = unique(dates$quadrat)

# only use these quadrats for analysis
grassdata = grasstotals %>% dplyr::filter(quadrat %in% quads)

# ==========================================
# preliminary look at data ----
# which species are on the most quadrats?
grass_species_quadrats = grassdata %>%
  dplyr::select(quadrat, species) %>% 
  unique() %>%
  group_by(species) %>%
  summarize(n_quadrats = n_distinct(quadrat))
# over 2: SPORO, BOER4, BOUTE, ARIST, DAPU7, SCBR2, PLMU3, MUAR, PAOB, PAHA, MUPO2, MUAR2, ENDE, SELE6

# which species have the highest cover?
grass_species_totalcover = grassdata %>%
  dplyr::select(species, totalarea) %>%
  group_by(species) %>%
  summarize(total = sum(totalarea))
# PLMU3, BOER4, SPORO, SCBR2, ARIST, MUAR, DAPU7, PAOB, MUAR2, BOUTE, MUPO2, SELE6, BOCU, PAHA, DICA8

# Looks like I should look at the main 5 (ARIST, BOER4, SPORO, PLMU3, SCBR2) plus maybe
#    DAPU7, BOUTE, MUAR, MUAR2, PAOB, PAHA, MUPO2, SELE6, ENDE

# ==========================================================

# 1995-2016 ----
boer = get_grass_ts_data(grassdata, dates, target_sp='BOER4', min_year=1995, max_year=2016, 
                         save_figures = F)
boer_theil = calculate_theil_sen(boer)
# multivariate mk test (trend package)
mult.mk.test()

plmu = get_grass_ts_data(grassdata, dates, target_sp='PLMU3', min_year=1995, max_year=2016, 
                         save_figures = F)
plmu_theil = calculate_theil_sen(plmu)

sporo = get_grass_ts_data(grassdata, dates, target_sp='SPORO', min_year=1995, max_year=2016, 
                         save_figures = F)
sporo_theil = calculate_theil_sen(sporo)

scbr = get_grass_ts_data(grassdata, dates, target_sp='SCBR2', min_year=1995, max_year=2016, 
                         save_figures = F)
scbr_theil = calculate_theil_sen(scbr)

arist = get_grass_ts_data(grassdata, dates, target_sp='ARIST', min_year=1995, max_year=2016, 
                         save_figures = F)
arist_theil = calculate_theil_sen(arist)

muar = get_grass_ts_data(grassdata, dates, target_sp='MUAR', min_year=1995, max_year=2016, 
                          save_figures = F)
muar_theil = calculate_theil_sen(muar)

dapu = get_grass_ts_data(grassdata, dates, target_sp='DAPU7', min_year=1995, max_year=2016, 
                         save_figures = F)
dapu_theil = calculate_theil_sen(dapu)

muar2 = get_grass_ts_data(grassdata, dates, target_sp='MUAR2', min_year=1995, max_year=2016, 
                          save_figures = F)
muar2_theil = calculate_theil_sen(muar2)

paob = get_grass_ts_data(grassdata, dates, target_sp='PAOB', min_year=1995, max_year=2016, 
                          save_figures = F)
paob_theil = calculate_theil_sen(paob)

paha = get_grass_ts_data(grassdata, dates, target_sp='PAHA', min_year=1995, max_year=2016, 
                         save_figures = F)
paha_theil = calculate_theil_sen(paha)

mupo2 = get_grass_ts_data(grassdata, dates, target_sp='MUPO2', min_year=1995, max_year=2016, 
                         save_figures = F)
mupo2_theil = calculate_theil_sen(mupo2)

sele6 = get_grass_ts_data(grassdata, dates, target_sp='SELE6', min_year=1995, max_year=2016, 
                          save_figures = F)
sele6_theil = calculate_theil_sen(sele6)



# 1945-1955 ----
# there are a selection of quadrats that were sampled poorly 1943-1960, so I will use
#  a different set of quadrats for this analysis

# quadrats that are not sampled at least once every 5 years 1945-1956; 
#   or are not sampled at least once in 1955 or 1956
removequads = c('B5','K2','K4','L1','L5','M5','N5','N6','P2','P3','P5',
                'T1','T2','T3','T4','T5','T6','T7','T8','T9','T10','T11',
                'V6','Y3','Y7')
grassdata2 = grassdata %>% dplyr::filter(!(quadrat %in% removequads))

# perform trend analysis on grass species
boer50 = grass_trend_analysis(grassdata2, dates, target_sp='BOER4', min_year=1945, max_year=1956, 
                         save_figures=T, aggregate_5_year = F)
length(unique(boer50$quadrat))
dplyr::filter(boer50, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(boer50, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

plmu50 = grass_trend_analysis(grassdata2, dates, target_sp='PLMU3', min_year=1945, max_year=1956, 
                         save_figures = T, aggregate_5_year = F)
length(unique(plmu50$quadrat))
dplyr::filter(plmu50, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(plmu50, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()


sporo50 = grass_trend_analysis(grassdata2, dates, target_sp='SPORO', min_year=1945, max_year=1956, 
                          save_figures = T, aggregate_5_year=F)
length(unique(sporo50$quadrat))
dplyr::filter(sporo50, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(sporo50, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

scbr50 = grass_trend_analysis(grassdata2, dates, target_sp='SCBR2', min_year=1945, max_year=1956, 
                         save_figures = T, aggregate_5_year = F)
length(unique(scbr50$quadrat))
dplyr::filter(scbr50, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(scbr50, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

arist50 = grass_trend_analysis(grassdata2, dates, target_sp='ARIST', min_year=1945, max_year=1956, 
                          save_figures = T, aggregate_5_year = F)
length(unique(arist50$quadrat))
dplyr::filter(arist50, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(arist50, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

muar50 = grass_trend_analysis(grassdata, dates, target_sp='MUAR', min_year=1945, max_year=1956, 
                         save_figures = T, aggregate_5_year = F)
length(unique(muar50$quadrat))
dplyr::filter(muar50, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(muar50, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

dapu50 = grass_trend_analysis(grassdata, dates, target_sp='DAPU7', min_year=1945, max_year=1956, 
                         save_figures = T, aggregate_5_year = F)
length(unique(dapu50$quadrat))
dplyr::filter(dapu50, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(dapu50, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

boute50 = grass_trend_analysis(grassdata, dates, target_sp='BOUTE', min_year=1945, max_year=1956, 
                          save_figures = T)
length(unique(boute50$quadrat))
dplyr::filter(boute50, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(boute50, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

muar250 = grass_trend_analysis(grassdata, dates, target_sp='MUAR2', min_year=1945, max_year=1956, 
                          save_figures = T)
length(unique(muar250$quadrat))
dplyr::filter(muar250, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(muar250, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

paob50 = grass_trend_analysis(grassdata, dates, target_sp='PAOB', min_year=1945, max_year=1956, 
                         save_figures = F)
length(unique(paob50$quadrat))
dplyr::filter(paob50, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(paob50, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()
