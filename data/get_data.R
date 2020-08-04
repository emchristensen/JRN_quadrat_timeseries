#' Get data and prepare for clustering
#' EMC 3/9/20
#' 
#' Shrub species: ATCA2, EPHED, EPTO, EPTR, FLCE, KRLA2, LATR2, LYBE, PRGL2, PSSC6, VACO9
#' Grass species: (many)
#' Note: yucca species removed from shrub cover total because they are short-lived
#' last run: 8/4/20

library(dplyr)

# read in data
datafolder = '../JRN_quadrat_datapaper/Plants/'
cover1 = read.csv(paste0(datafolder, 'Jornada_quadrat_cover.csv'), stringsAsFactors = F)
cover2 = read.csv(paste0(datafolder,'Jornada_quadrat_cover_new.csv'), stringsAsFactors = F)
counts1 = read.csv(paste0(datafolder, 'Jornada_quadrat_forb_counts.csv'), stringsAsFactors = F)
counts2 = read.csv(paste0(datafolder,'Jornada_quadrat_forb_counts_new.csv'), stringsAsFactors = F)
dates = read.csv(paste0(datafolder, 'dates/quadrat_sample_dates_20200803.csv'), stringsAsFactors = F)
splist = read.csv(paste0(datafolder, 'Jornada_quadrat_species_list_WIP.csv'), stringsAsFactors = F)


# calculate total shrub, grass, and forb
shrub = rbind(cover1, cover2) %>%
  merge(splist) %>%
  dplyr::filter(form=='SHRUB', !(species_code %in% c('YUBA','YUEL'))) %>%
  group_by(quadrat, project_year, year, month) %>%
  summarize(total_shrub = sum(area)) %>%
  merge(dates, all=T)
shrub$total_shrub[is.na(shrub$total_shrub)] <- 0

grass = rbind(cover1, cover2) %>%
  merge(splist) %>%
  dplyr::filter(form=='GRASS') %>%
  group_by(quadrat, project_year, year, month) %>%
  summarize(total_grass = sum(area)) %>%
  merge(dates, all=T)
grass$total_grass[is.na(grass$total_grass)] <- 0

forb = rbind(counts1, counts2) %>%
  merge(splist) %>%
  group_by(quadrat, project_year, year, month) %>%
  summarize(n_forbs=sum(count)) %>%
  merge(dates, all=T)
forb$n_forbs[is.na(forb$n_forbs)] <- 0

# put data frame together and save to csv
quadrat_veg = merge(shrub, grass, all=T) %>%
  merge(forb, all=T)
write.csv(quadrat_veg, 'data/quadrat_veg.csv', row.names=F)

