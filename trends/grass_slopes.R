#' This script looks at trends of grass on individual quadrats during important time periods
#' Both total grass and most important grass species
#' Time periods: 1945-1955 (to capture drought)
#'               1955-1980 (to capture any recovery)
#'               1995-2016 (to describe current dynamics)
#' This analysis uses Theil-Sen slope with Kendall's tau test for significance to describe trends
#' 
#' EMC 9/3/20
#' Info on sens.slope: https://cran.r-project.org/web/packages/trend/vignettes/trend.pdf
#' last run: 2/17/21

library(dplyr)
library(ggplot2)
library(trend)

source('data_functions.R')

grasstotals = read.csv('data/grass_species_totals.csv', stringsAsFactors = F)
dates = read.csv('data/quadrats_dates_for_analysis.csv', stringsAsFactors = F)

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
#    DAPU7, BOUTE, MUAR, MUAR2, PAOB

# ==========================================================

# 1995-2016 ----
# perform trend analysis on total grass per quadrat
# there's something weird with quadrat A2: remove
grassdata1 = dplyr::filter(grassdata, quadrat != 'A2')
grass = grass_trend_lm_analysis(grassdata1, dates, target_sp='All', min_year=1995, max_year=2016,
                               save_figures=T, aggregate_5_year = F)
length(unique(grass$quadrat))
length(unique(grass$quadrat[is.na(grass$pvalue)]))
dplyr::filter(grass, significant_05==1) %>% dplyr::select(quadrat) %>% unique() %>% nrow()
dplyr::filter(grass, significant_05==2) %>% dplyr::select(quadrat) %>% unique() %>% nrow()


# perform trend analysis on grass species
boer = grass_trend_lm_analysis(grassdata1, dates, target_sp='BOER4', min_year=1995, max_year=2016, 
                              save_figures=T, aggregate_5_year = F)
length(unique(boer$quadrat[!is.na(boer$pvalue)]))
dplyr::filter(boer, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(boer, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

plmu = grass_trend_lm_analysis(grassdata1, dates, target_sp='PLMU3', min_year=1995, max_year=2016, 
                              save_figures = T, aggregate_5_year = F)
length(unique(plmu$quadrat[!is.na(plmu$pvalue)]))
dplyr::filter(plmu, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(plmu, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()


sporo = grass_trend_lm_analysis(grassdata1, dates, target_sp='SPORO', min_year=1995, max_year=2016, 
                               save_figures = T, aggregate_5_year=F)
length(unique(sporo$quadrat[!is.na(sporo$pvalue)]))
dplyr::filter(sporo, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(sporo, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

scbr = grass_trend_lm_analysis(grassdata1, dates, target_sp='SCBR2', min_year=1995, max_year=2016, 
                              save_figures = T, aggregate_5_year = F)
length(unique(scbr$quadrat[!is.na(scbr$pvalue)]))
dplyr::filter(scbr, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(scbr, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

arist = grass_trend_lm_analysis(grassdata1, dates, target_sp='ARIST', min_year=1995, max_year=2016, 
                               save_figures = T, aggregate_5_year = F)
length(unique(arist$quadrat[!is.na(arist$pvalue)]))
dplyr::filter(arist, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(arist, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

muar = grass_trend_lm_analysis(grassdata1, dates, target_sp='MUAR', min_year=1995, max_year=2016, 
                              save_figures = T, aggregate_5_year = F)
length(unique(muar$quadrat[!is.na(muar$pvalue)]))
dplyr::filter(muar, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(muar, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

dapu = grass_trend_lm_analysis(grassdata1, dates, target_sp='DAPU7', min_year=1995, max_year=2016, 
                              save_figures = T, aggregate_5_year = F)
length(unique(dapu$quadrat[!is.na(dapu$pvalue)]))
dplyr::filter(dapu, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(dapu, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

#boute = grass_trend_analysis(grassdata1, dates, target_sp='BOUTE', min_year=1995, max_year=2016, 
#                                save_figures = T)
# length(unique(boute$quadrat))
# dplyr::filter(boute, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
# dplyr::filter(boute, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

muar2 = grass_trend_lm_analysis(grassdata1, dates, target_sp='MUAR2', min_year=1995, max_year=2016, 
                               save_figures = T)
length(unique(muar2$quadrat[!is.na(muar2$pvalue)]))
dplyr::filter(muar2, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(muar2, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

paob = grass_trend_lm_analysis(grassdata1, dates, target_sp='PAOB', min_year=1995, max_year=2016, 
                              save_figures = T)
length(unique(paob$quadrat[!is.na(paob$pvalue)]))
dplyr::filter(paob, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(paob, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()



# 1945-1956 ----
# there are a selection of quadrats that were sampled poorly 1943-1960, so I will use
#  a different set of quadrats for this analysis

# quadrats that are not sampled at least 5 times 1945-1956; 
#   or are not sampled at least once in 1955 or 1956 -- removed: K4, M5, N5, N6, P2, P5, V6
removequads = c('B5','K2','L1','L5','P3',
                'T1','T2','T3','T4','T5','T6','T7','T8','T9','T10','T11',
                'Y1','Y2','Y3','Y7')
grassdata2 = grassdata %>% dplyr::filter(!(quadrat %in% removequads))

# perform trend analysis on total grass per quadrat
grass50 = grass_trend_analysis(grassdata2, dates, target_sp='All', min_year=1945, max_year=1956,
                               save_figures=T, aggregate_5_year = F)
length(unique(grass50$quadrat))
length(unique(grass50$quadrat[is.na(grass50$pvalue)]))
dplyr::filter(grass50, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(grass50, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

# perform trend analysis on grass species
boer50 = grass_trend_analysis(grassdata2, dates, target_sp='BOER4', min_year=1945, max_year=1956, 
                         save_figures=T, aggregate_5_year = F)
length(unique(boer50$quadrat[!is.na(boer50$pvalue)]))
dplyr::filter(boer50, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(boer50, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

plmu50 = grass_trend_analysis(grassdata2, dates, target_sp='PLMU3', min_year=1945, max_year=1956, 
                         save_figures = T, aggregate_5_year = F)
length(unique(plmu50$quadrat[!is.na(plmu50$pvalue)]))
dplyr::filter(plmu50, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(plmu50, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()


sporo50 = grass_trend_analysis(grassdata2, dates, target_sp='SPORO', min_year=1945, max_year=1956, 
                          save_figures = T, aggregate_5_year=F)
length(unique(sporo50$quadrat[!is.na(sporo50$pvalue)]))
dplyr::filter(sporo50, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(sporo50, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

scbr50 = grass_trend_analysis(grassdata2, dates, target_sp='SCBR2', min_year=1945, max_year=1956, 
                         save_figures = T, aggregate_5_year = F)
length(unique(scbr50$quadrat[!is.na(scbr50$pvalue)]))
dplyr::filter(scbr50, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(scbr50, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

arist50 = grass_trend_analysis(grassdata2, dates, target_sp='ARIST', min_year=1945, max_year=1956, 
                          save_figures = T, aggregate_5_year = F)
length(unique(arist50$quadrat[!is.na(arist50$pvalue)]))
dplyr::filter(arist50, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(arist50, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

muar50 = grass_trend_analysis(grassdata2, dates, target_sp='MUAR', min_year=1945, max_year=1956, 
                         save_figures = T, aggregate_5_year = F)
length(unique(muar50$quadrat[!is.na(muar50$pvalue)]))
dplyr::filter(muar50, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(muar50, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

dapu50 = grass_trend_analysis(grassdata2, dates, target_sp='DAPU7', min_year=1945, max_year=1956, 
                         save_figures = T, aggregate_5_year = F)
length(unique(dapu50$quadrat[!is.na(dapu50$pvalue)]))
dplyr::filter(dapu50, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(dapu50, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

boute50 = grass_trend_analysis(grassdata2, dates, target_sp='BOUTE', min_year=1945, max_year=1956, 
                          save_figures = T)
length(unique(boute50$quadrat[!is.na(boute50$pvalue)]))
dplyr::filter(boute50, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(boute50, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

muar250 = grass_trend_analysis(grassdata2, dates, target_sp='MUAR2', min_year=1945, max_year=1956, 
                          save_figures = T)
length(unique(muar250$quadrat[!is.na(muar250$pvalue)]))
dplyr::filter(muar250, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(muar250, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

paob50 = grass_trend_analysis(grassdata2, dates, target_sp='PAOB', min_year=1945, max_year=1956, 
                         save_figures = F)
length(unique(paob50$quadrat[!is.na(paob50$pvalue)]))
dplyr::filter(paob50, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(paob50, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

# 1955-1980 ----
# there are a selection of quadrats that were sampled poorly, so I will use
#  a different set of quadrats for this analysis

# only use quadrats that don't have a 5+ year gap in data 1955-1980 and are sampled once 1976-79 
# removequads2 = c('K1','K2','K4','L1','L2','L3A','L5','M5','M6',
#                 'P1','P2','P3','P4','P5',
#                 'T1','T2','T3','T4','T5','T6','T7','T8','T9','T10','T11',
#                 'V1','V2','V3','V4','V5','V6',
#                 'Y1','Y2','Y3','Y7')
# only use quads that have 6+ samples 1955-1980 and are sampled once 1977-1979 and once 1955-56
removequads2 = c('K1','K2','K4','L1','L5',
                 'N6','P3','P4','P5',
                 'U4','V1','V4','Y2','Y3','Y7')

grassdata3 = grassdata %>% dplyr::filter(!(quadrat %in% removequads2))

# perform trend analysis on total grass per quadrat
grass60 = grass_trend_analysis(grassdata3, dates, target_sp='All', min_year=1955, max_year=1980,
                               save_figures=T, aggregate_5_year = F)
length(unique(grass60$quadrat))
length(unique(grass60$quadrat[is.na(grass60$pvalue)]))
dplyr::filter(grass60, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(grass60, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()


# perform trend analysis on grass species
boer60 = grass_trend_analysis(grassdata3, dates, target_sp='BOER4', min_year=1955, max_year=1980, 
                              save_figures=T, aggregate_5_year = F)
length(unique(boer60$quadrat[!is.na(boer60$pvalue)]))
dplyr::filter(boer60, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(boer60, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

plmu60 = grass_trend_analysis(grassdata3, dates, target_sp='PLMU3', min_year=1955, max_year=1980, 
                              save_figures = T, aggregate_5_year = F)
length(unique(plmu60$quadrat[!is.na(plmu60$pvalue)]))
dplyr::filter(plmu60, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(plmu60, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()


sporo60 = grass_trend_analysis(grassdata3, dates, target_sp='SPORO', min_year=1955, max_year=1980, 
                               save_figures = T, aggregate_5_year=F)
length(unique(sporo60$quadrat[!is.na(sporo60$pvalue)]))
dplyr::filter(sporo60, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(sporo60, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

scbr60 = grass_trend_analysis(grassdata3, dates, target_sp='SCBR2', min_year=1955, max_year=1980, 
                              save_figures = T, aggregate_5_year = F)
length(unique(scbr60$quadrat[!is.na(scbr60$pvalue)]))
dplyr::filter(scbr60, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(scbr60, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

arist60 = grass_trend_analysis(grassdata3, dates, target_sp='ARIST', min_year=1955, max_year=1980, 
                               save_figures = T, aggregate_5_year = F)
length(unique(arist60$quadrat[!is.na(arist60$pvalue)]))
dplyr::filter(arist60, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(arist60, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

muar60 = grass_trend_analysis(grassdata3, dates, target_sp='MUAR', min_year=1955, max_year=1980, 
                              save_figures = T, aggregate_5_year = F)
length(unique(muar60$quadrat[!is.na(muar60$pvalue)]))
dplyr::filter(muar60, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(muar60, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

dapu60 = grass_trend_analysis(grassdata3, dates, target_sp='DAPU7', min_year=1955, max_year=1980, 
                              save_figures = T, aggregate_5_year = F)
length(unique(dapu60$quadrat[!is.na(dapu60$pvalue)]))
dplyr::filter(dapu60, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(dapu60, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

boute60 = grass_trend_analysis(grassdata3, dates, target_sp='BOUTE', min_year=1955, max_year=1980, 
                               save_figures = T)
length(unique(boute60$quadrat[!is.na(boute60$pvalue)]))
dplyr::filter(boute60, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(boute60, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

muar260 = grass_trend_analysis(grassdata3, dates, target_sp='MUAR2', min_year=1955, max_year=1980, 
                               save_figures = T)
length(unique(muar260$quadrat[!is.na(muar260$pvalue)]))
dplyr::filter(muar260, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(muar260, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

paob60 = grass_trend_analysis(grassdata3, dates, target_sp='PAOB', min_year=1955, max_year=1980, 
                              save_figures = F)
length(unique(paob60$quadrat[!is.na(paob60$pvalue)]))
dplyr::filter(paob60, significant_05==1) %>% select(quadrat) %>% unique() %>% nrow()
dplyr::filter(paob60, significant_05==2) %>% select(quadrat) %>% unique() %>% nrow()

# ==========================================================================
# slopes ----
# extract slope data from above results for use in logistic models
slope_data = unique(dplyr::select(grass, quadrat, slope_95=slope, pvalue_95=pvalue, significant_95=significant_05)) %>%
  merge(unique(dplyr::select(grass60, quadrat, slope_60=slope, pvalue_60=pvalue, significant_60=significant_05)), all=T) %>%
  merge(unique(dplyr::select(grass50, quadrat, slope_50=slope, pvalue_50=pvalue, significant_50=significant_05)), all=T)

# important species: BOER, SPORO, ARIST for 1950s; SPORO and DAPU7 in 1960s
slope_boer = unique(dplyr::select(boer50, quadrat, slope_50=slope, pvalue_50=pvalue, significant_50=significant_05)) %>%
  merge(unique(dplyr::select(boer60, quadrat, slope_60=slope, pvalue_60=pvalue, significant_60=significant_05)), all=T) %>%
  merge(unique(dplyr::select(boer, quadrat, slope_95=slope, pvalue_95=pvalue, significant_95=significant_05)), all=T)

slope_plmu = unique(dplyr::select(plmu50, quadrat, slope_50=slope, pvalue_50=pvalue, significant_50=significant_05)) %>%
  merge(unique(dplyr::select(plmu60, quadrat, slope_60=slope, pvalue_60=pvalue, significant_60=significant_05)), all=T) %>%
  merge(unique(dplyr::select(plmu, quadrat, slope_95=slope, pvalue_95=pvalue, significant_95=significant_05)), all=T)

slope_sporo = unique(dplyr::select(sporo50, quadrat, slope_50=slope, pvalue_50=pvalue, significant_50=significant_05)) %>%
  merge(unique(dplyr::select(sporo60, quadrat, slope_60=slope, pvalue_60=pvalue, significant_60=significant_05)), all=T) %>%
  merge(unique(dplyr::select(sporo, quadrat, slope_95=slope, pvalue_95=pvalue, significant_95=significant_05)), all=T)

slope_scbr = unique(dplyr::select(scbr50, quadrat, slope_50=slope, pvalue_50=pvalue, significant_50=significant_05)) %>%
  merge(unique(dplyr::select(scbr60, quadrat, slope_60=slope, pvalue_60=pvalue, significant_60=significant_05)), all=T) %>%
  merge(unique(dplyr::select(scbr, quadrat, slope_95=slope, pvalue_95=pvalue, significant_95=significant_05)), all=T)

slope_arist = unique(dplyr::select(arist50, quadrat, slope_50=slope, pvalue_50=pvalue, significant_50=significant_05)) %>%
  merge(unique(dplyr::select(arist60, quadrat, slope_60=slope, pvalue_60=pvalue, significant_60=significant_05)), all=T) %>%
  merge(unique(dplyr::select(arist, quadrat, slope_95=slope, pvalue_95=pvalue, significant_95=significant_05)), all=T)

slope_dapu = unique(dplyr::select(dapu50, quadrat, slope_50=slope, pvalue_50=pvalue, significant_50=significant_05)) %>%
  merge(unique(dplyr::select(dapu60, quadrat, slope_60=slope, pvalue_60=pvalue, significant_60=significant_05)), all=T) %>%
  merge(unique(dplyr::select(dapu, quadrat, slope_95=slope, pvalue_95=pvalue, significant_95=significant_05)), all=T)


write.csv(slope_data, 'data/slopes_50_60_95.csv', row.names=F)
write.csv(slope_boer, 'data/slopes_boer_50_60_95.csv', row.names=F)
write.csv(slope_plmu, 'data/slopes_plmu_50_60_95.csv', row.names=F)
write.csv(slope_sporo, 'data/slopes_sporo_50_60_95.csv', row.names=F)
write.csv(slope_scbr, 'data/slopes_scbr_50_60_95.csv', row.names=F)
write.csv(slope_arist, 'data/slopes_arist_50_60_95.csv', row.names=F)
write.csv(slope_dapu, 'data/slopes_dapu_50_60_95.csv', row.names=F)

