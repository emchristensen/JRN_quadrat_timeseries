#' Get data and prepare for analysis
#' EMC 3/9/20
#' 
#' Shrub species: ATCA2, EPHED, FLCE, KRLA2, LATR2, LYBE, PRGL2, PSSC6, VACO9
#' Grass species: (many)
#' Notes: yucca species removed from shrub cover total because they are short-lived
#'     grouped EPTO, EPTR, and EPHED as all EPHED. Adler's group called EPTO=EPHED, and functionally they're not different
#'     grouped CYRE14 and CYPER (Adler did)
#'     changed SPNE -> MUAR
#'     changed MUTO2 -> MUAR2
#'     changed SPCO4 -> SPFL2
#'     changed SPCR -> SPFL2
#'     changed ARPA9, ARPU9, ARPUL -> ARIST
#' last run: 8/12/20

library(dplyr)
library(lubridate)

# read in data
datafolder = '../JRN_quadrat_datapaper/Plants/'
cover1 = read.csv(paste0(datafolder, 'Jornada_quadrat_cover.csv'), stringsAsFactors = F)
cover2 = read.csv(paste0(datafolder,'Jornada_quadrat_cover_new.csv'), stringsAsFactors = F)
counts1 = read.csv(paste0(datafolder, 'Jornada_quadrat_forb_counts.csv'), stringsAsFactors = F)
counts2 = read.csv(paste0(datafolder,'Jornada_quadrat_forb_counts_new.csv'), stringsAsFactors = F)
dates = read.csv(paste0(datafolder, 'dates/quadrat_sample_dates_20200803.csv'), stringsAsFactors = F)
splist = read.csv(paste0(datafolder, 'Jornada_quadrat_species_list_WIP.csv'), stringsAsFactors = F)
spchanges = read.csv('data/species_name_changes.csv', stringsAsFactors = F)
quadtype = read.csv('data/quad_type_coordinate.csv', stringsAsFactors = F) %>% dplyr::select(quadrat, vegtype, upland)


#  setup file ----
# quadrats ready to use for analysis (skipped L3A --too close to L3)
#    NEW: K4, L5, N2, N3, N3A, P3, P4, P5, T1, T2, T3
#    SOON: K4, N1, N4, N5, N6, P1, P2, R1, R2, R3, R4
quads = c('A1','A2','A3','A4','A5',
          'AR1','AR2','AR3','AR4','AR5','AR6',
          'B1','B2','B2A','B3','B4','B5',
          'G1','G2','G3','G4','G5','G6',
          'H1','H2','H3',
          'I1','I2','I3','I4','I5','I6','I7',
          'J1','J8','J9','J12','J22',
          'K1','K2','K3',
          'K4',
          'L1','L2','L3','L4',
          'L5',
          'M5','M6',
          'N2','N3','N3A',
          'N1','N4','N5','N6',
          'P1','P2',
          'P3','P4','P5',
          'R1','R2','R3','R4',
          'T1','T2','T3',
          'T5','T6','T7','T8','T9','T10','T11',
          'U1','U2','U3','U4','U5',
          'V1','V2','V3','V4','V5','V6',
          'Y1','Y2','Y3','Y7')

# create file indicating which dates/quadrats will be used for these analyses
quaddates = dates %>% dplyr::filter(quadrat %in% quads) %>%
  mutate(date=as.Date(paste(year, month, day, sep='-'))) %>% 
  group_by(quadrat, project_year) %>%
  summarize(lastdate = max(date)) %>%
  ungroup() %>%
  mutate(month = month(lastdate),
         day = day(lastdate),
         year = year(lastdate)) %>%
  dplyr::select(quadrat, project_year, year, month, day) %>%
  arrange(quadrat, project_year)

# write to file
write.csv(quaddates, 'data/quadrats_dates_for_analysis.csv', row.names = F)

# calculate total cover by species ----

shrub = rbind(cover1, cover2) %>%
  merge(splist) %>%
  dplyr::filter(form=='SHRUB', !(species_code %in% c('YUBA','YUEL'))) %>%
  group_by(quadrat, project_year, year, month, species_code) %>%
  summarize(totalarea = sum(area)) %>%
  merge(spchanges, by.x='species_code', by.y='oldspeciescode', all.x=T)
# make species name changes
shrub$species = shrub$newspeciescode
shrub$species[is.na(shrub$species)] <- shrub$species_code[is.na(shrub$species)]
shrubfinal = dplyr::select(shrub, quadrat, project_year, year, month, species, totalarea)
write.csv(shrubfinal, 'data/shrub_species_totals.csv', row.names = F)


grass = rbind(cover1, cover2) %>%
  merge(splist) %>%
  dplyr::filter(form=='GRASS', category=='Cover') %>%
  group_by(quadrat, project_year, year, month, species_code) %>%
  summarize(totalarea = sum(area)) %>%
  merge(spchanges, by.x='species_code', by.y='oldspeciescode', all.x=T)
# make species name changes
grass$species = grass$newspeciescode
grass$species[is.na(grass$species)] <- grass$species_code[is.na(grass$species)]
grassfinal = dplyr::select(grass, quadrat, project_year, year, month, species, totalarea)
write.csv(grassfinal, 'data/grass_species_totals.csv', row.names = F)


# ====================================
# get total grass and shrub ----
shrubfinal = read.csv('data/shrub_species_totals.csv', stringsAsFactors = F)
totalshrub = shrubfinal %>%
  group_by(quadrat, project_year, year, month) %>%
  summarize(total_shrub = sum(totalarea)) %>%
  merge(dates, all=T)
totalshrub$total_shrub[is.na(totalshrub$total_shrub)] <- 0

grassfinal = read.csv('data/grass_species_totals.csv', stringsAsFactors = F)
totalgrass = grassfinal %>%
  group_by(quadrat, project_year, year, month) %>%
  summarize(total_grass = sum(totalarea)) %>%
  merge(dates, all=T)
totalgrass$total_grass[is.na(totalgrass$total_grass)] <- 0

forb = rbind(counts1, counts2) %>%
  merge(splist) %>%
  group_by(quadrat, project_year, year, month) %>%
  summarize(n_forbs=sum(count)) %>%
  merge(dates, all=T)
forb$n_forbs[is.na(forb$n_forbs)] <- 0

# put data frame together and save to csv
quadrat_veg = merge(totalshrub, totalgrass, all=T) %>%
  merge(forb, all=T)
write.csv(quadrat_veg, 'data/quadrat_veg.csv', row.names=F)

