#' script to create time series of total grass on quadrats
#' selected quadrats and years so that timeseries line up
#' All quadrats selected are separated  by >=50m
#' 
#' There are 3 time periods of partiucular interest: 1930-1940; 1945-1960; 1995-2016
#' 
#' EMC 7/22/20 - no new digitized quads yet, need to resolve species issues first

library(dplyr)
library(ggplot2)

plantcover <- read.csv('data/quadrat_veg.csv', stringsAsFactors = F)
plantcover$date = as.Date(paste(plantcover$year, plantcover$month, '01', sep='-'))
sampledates <- read.csv('../JRN_quadrat_datapaper/Plants/dates/quadrat_sample_dates_20200803.csv', stringsAsFactors = F)

# make crosstab with all quadrats and all years
quadyear = sampledates %>%
  group_by(quadrat, project_year) %>% tally() %>%
  tidyr::spread(project_year, value=n)
write.csv(quadyear,'data/quadyearcrosstab.csv', row.names = F)

# time period 1: 1927-1940 ----
# quadrats to be used in analysis:
#   not using N, P, R series yet (7/22/20)
#   removed AR6 -- too close to AR5 (11m)
#   removed J12 -- too close to J1 (35m)
#   removed K2 -- no samples between 1925 and 1965
#   removed K4 -- missing samples
#   removed L3A -- too physically close to L3 (0m)
#   removed M5 -- too close to M6 (44m) and was missing more than M6
#   removed Y2 -- too close to Y1 (14m)
# missing 1 or 2 sample from time period, may remove: J22, K1, M6, N3, N4, P1, T1, T4, T8
# NOTE: P1 1938 exists as a scan but not digitized
tsquads1 = c('A1','A2','A3','A4','A5','AR1','AR2','AR3','AR4','AR5',
            'B1','B2','B2A','B3','B4','B5',
            'G1','G2','G3','G4','G5','G6',
            'H1','H2','H3',
            'I1','I2','I3','I4','I5','I6','I7',
            'J1','J8','J9','J22',
            'K1','K3',
            'L1','L2','L3','L4','L5',
            'M6',
            'N1','N3','N3A','N4','N5','N6',
            'P1','P2','P3','P4','P5',
            'R1','R2','R3','R4',
            'T1','T2','T3','T4',
            'T5','T6','T7','T8','T9','T10','T11',
            'U1','U2','U3','U4','U5',
            'V1','V2','V3','V4','V5','V6',
            'Y1','Y3','Y7')

# years to be used in analysis
#    most quads were initialized by 1927; several missing 1927-1928 however
#    many missing 1941
tsyears1 = c(1929:1940, 1942)

# create crosstab of quadrat/year samples to check overlap
quadyear1 = dplyr::filter(sampledates, project_year %in% tsyears1,
                         quadrat %in% tsquads1) %>%
  group_by(quadrat, project_year) %>% tally() %>%
  tidyr::spread(project_year, value=n)
write.csv(quadyear1,'data/quadyearcrosstab1.csv', row.names = F)

# write data to csv ready for modeling
plantcover1 = dplyr::filter(plantcover, 
                            quadrat %in% tsquads1, 
                            project_year %in% tsyears1)
# get rid of second sample in 1935
plantcover1 = plantcover1[plantcover1$date< as.Date('1935-01-01') | plantcover1$date>as.Date('1935-09-30'),] %>%
  unique()
# write to csv and get rid of second sample in a year when present (R4 1934)
write.csv(plantcover1,'data/grasscover1_raw.csv', row.names = F)
plantcover1 = read.csv('data/grasscover1_raw.csv', stringsAsFactors = F)

pc_wide = tidyr::pivot_wider(plantcover1, id_cols=quadrat, names_from=project_year, values_from = total_grass)
write.csv(pc_wide, 'data/grasscover1_wide.csv', row.names = F)

# time period 2: 1945-1960 ----
# second drought hit somewhere between 1950 and 1955
# quadrats to be used for analysis:
#   not using N, P, R series yet
#   removed AR6 -- too close to AR5 (11m)
#   removed L3A -- too physically close to L3 (0m)  
#   removed J12 -- too close to J1 (35m)
#   removed K2 -- no samples between 1925 and 1965
#   removed L5 -- missing samples
#   removed M5, M6 -- missing many samples, none after 1957
#   removed T1-T11 -- no samples between 1944 and 1968
#   removed P1-P5 -- missing many samples between 1947-1975
#   removed Y1, Y2, Y3, Y7 -- missing many samples 1942-1960

tsquads2 = c('A1','A2','A3','A4','A5','AR1','AR2','AR3','AR4','AR5',
             'B1','B2','B2A','B3','B4','B5',
             'G1','G2','G3','G4','G5','G6',
             'H1','H2','H3',
             'I1','I2','I3','I4','I5','I6','I7',
             'J1','J8','J9','J22',
             'K1','K3','K4',
             'L1','L2','L3','L4',
             #'L5',
             #'M5','M6',
             'N1','N3','N3A','N4','N5','N6',
             #'P1','P2','P3','P4','P5',
             'R1','R2','R3','R4',
             #'T1','T2','T3','T4',
             #'T5','T6','T7','T8','T9','T10','T11',
             'U1','U2','U3','U4','U5',
             'V1','V2','V3','V4','V5','V6')
             #'Y1','Y2','Y3','Y7')

# years to be used in analysis
#    lots missing in 1948, 1953-1955
#    samples really fall off after 1960
tsyears2 = c(1942:1947, 1949:1952, 1956:1960)

# create crosstab of quadrat/year samples to check overlap
quadyear2 = dplyr::filter(sampledates, project_year %in% tsyears2,
                          quadrat %in% tsquads2) %>%
  group_by(quadrat, project_year) %>% tally() %>%
  tidyr::spread(project_year, value=n)
write.csv(quadyear2,'data/quadyearcrosstab2.csv', row.names = F)

plantcover2 = dplyr::filter(plantcover, quadrat %in% tsquads2, project_year %in% tsyears2) %>%
  group_by(quadrat)
# =============================================================

# plots to explore data -- time period 1
plotdat = dplyr::filter(plantcover1, quadrat %in% c('A1','A2','A3','A4','A5','AR1','AR2','AR3'))
ggplot(plotdat, aes(x=project_year, y=total_grass)) + geom_point() +
  geom_line(aes(color=quadrat)) +
  #geom_line() +
  #theme(legend.position = 'none') +
  theme_bw()





# time period 2 ----
ggplot(plantcover2, aes(x=date, y=total_grass)) + geom_point() +
  geom_line(aes(color=quadrat)) +
  #geom_line() +
  theme(legend.position = 'none')
