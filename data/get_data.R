#' Get data and prepare for clustering
#' EMC 3/9/20
#' last run: 6/16/20

library(dplyr)

# read in data
datafolder = '../JRN_quadrat_datapaper/Plants/'
cover = read.csv(paste0(datafolder, 'Jornada_quadrat_cover.csv'), stringsAsFactors = F)
counts = read.csv(paste0(datafolder, 'Jornada_quadrat_forb_counts.csv'), stringsAsFactors = F)
dates = read.csv(paste0(datafolder, 'quadrat_sample_dates.csv'), stringsAsFactors = F)
splist = read.csv(paste0(datafolder, 'Jornada_quadrat_species_list_WIP.csv'), stringsAsFactors = F)


# calculate total shrub, grass, and forb
shrub = merge(cover, splist) %>%
  dplyr::filter(form=='SHRUB') %>%
  group_by(quadrat, project_year, year, month) %>%
  summarize(total_shrub = sum(area)) %>%
  merge(dates, all=T)
shrub$total_shrub[is.na(shrub$total_shrub)] <- 0

grass = merge(cover, splist) %>%
  dplyr::filter(form=='GRASS') %>%
  group_by(quadrat, project_year, year, month) %>%
  summarize(total_grass = sum(area)) %>%
  merge(dates, all=T)
grass$total_grass[is.na(grass$total_grass)] <- 0

forb = merge(counts, splist) %>%
  group_by(quadrat, project_year, year, month) %>%
  summarize(n_forbs=sum(count)) %>%
  merge(dates, all=T)
forb$n_forbs[is.na(forb$n_forbs)] <- 0

# put data frame together and save to csv
quadrat_veg = merge(shrub, grass, all=T) %>%
  merge(forb, all=T)
write.csv(quadrat_veg, 'data/quadrat_veg.csv', row.names=F)

