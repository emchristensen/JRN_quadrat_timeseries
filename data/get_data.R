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
#' last run: 12/10/20

library(dplyr)
library(lubridate)

# read in data
datafolder = '../JRN_quadrat_datapaper/Plants/'
perennials = read.csv(paste0(datafolder, 'Jornada_quadrat_perennials.csv'), stringsAsFactors = F)
#cover1 = read.csv(paste0(datafolder, 'Jornada_quadrat_cover.csv'), stringsAsFactors = F)
#counts1 = read.csv(paste0(datafolder, 'Jornada_quadrat_forb_counts.csv'), stringsAsFactors = F)
dates = read.csv(paste0(datafolder, 'dates/quadrat_sample_dates.csv'), stringsAsFactors = F)
splist = read.csv(paste0(datafolder, 'Jornada_quadrat_species_list_WIP.csv'), stringsAsFactors = F)
spchanges = read.csv('data/species_name_changes.csv', stringsAsFactors = F)
quadtype = read.csv('data/quad_type_coordinate.csv', stringsAsFactors = F) %>% dplyr::select(quadrat, vegtype, upland_byspecies)


#  setup file ----
# quadrats ready to use for analysis 
#    NEW: K4, L5, N1, N2, N3, N3A, N4, N5, N6, P1, P2, P3, P4, P5, R1, R2, R3, R4, T1, T2, T3, T4
#    Still maybe species errors: N1, N3, N3A, N4, N5, N6, P1, P2, P3, P5, R1, R2, R3, R4, T2, T3
quads = c('A1','A2','A3','A4','A5',
          'AR1','AR2','AR3','AR4','AR5','AR6',
          'B1','B2','B2A','B3','B4','B5',
          'G1','G2','G3','G4','G5','G6',
          'H1','H2','H3',
          'I1','I2','I3','I4','I5','I6','I7',
          'J1','J8','J9','J12','J22',
          'K1','K2','K3','K4',
          'L1','L2','L3','L3A','L4','L5',
          'M5','M6',
          'N1','N2','N3','N3A','N4','N5','N6',
          'P1','P2','P3','P4','P5',
          'R1','R2','R3','R4',
          'T1','T2','T3','T4',
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

# get crosstab of years and quadrats
quaddates_wide = quaddates %>% 
  mutate(sampled = rep(1)) %>%
  group_by(quadrat, project_year) %>%
  summarize(samples=sum(sampled)) %>%
  #ungroup() %>%
  arrange(project_year) %>%
  tidyr::pivot_wider(id_cols=quadrat, names_from=project_year, values_from=samples)

write.csv(quaddates_wide, 'data/quadyearcrosstab.csv', row.names=F)

# calculate total cover by species ----

# make species name changes; restrict to just the quaddates from above
perenn_spchanged = perennials %>%
  merge(spchanges, by.x='species_code', by.y='oldspeciescode', all.x=T) %>%
  merge(quaddates)
perenn_spchanged$species = perenn_spchanged$newspeciescode
perenn_spchanged$species[is.na(perenn_spchanged$species)] <- perenn_spchanged$species_code[is.na(perenn_spchanged$species)]

# total shrub cover by species
shrub = perenn_spchanged %>%
  merge(splist, by.x='species', by.y='species_code', all.x=T) %>%
  dplyr::filter(form=='SHRUB', !(species %in% c('YUBA','YUEL'))) %>%
  group_by(quadrat, project_year, year, month, species) %>%
  summarize(totalarea = sum(area)) 

shrubfinal = dplyr::select(shrub, quadrat, project_year, year, month, species, totalarea)
write.csv(shrubfinal, 'data/shrub_species_totals.csv', row.names = F)

# total grass cover by species. remove Cyperus -- not technically a grass
grass = perenn_spchanged %>%
  merge(splist, by.x='species', by.y='species_code', all.x=T) %>%
  dplyr::filter(form=='GRASS') %>%
  dplyr::filter(species!='CYPER') %>%
  group_by(quadrat, project_year, year, month, species) %>%
  summarize(totalarea = sum(area)) 

grassfinal = dplyr::select(grass, quadrat, project_year, year, month, species, totalarea)
write.csv(grassfinal, 'data/grass_species_totals.csv', row.names = F)


# ====================================
# get total grass and shrub ----
shrubfinal = read.csv('data/shrub_species_totals.csv', stringsAsFactors = F)
totalshrub = shrubfinal %>%
  group_by(quadrat, project_year, year, month) %>%
  summarize(total_shrub = sum(totalarea)) %>%
  merge(quaddates, all=T)
totalshrub$total_shrub[is.na(totalshrub$total_shrub)] <- 0

grassfinal = read.csv('data/grass_species_totals.csv', stringsAsFactors = F)
totalgrass = grassfinal %>%
  group_by(quadrat, project_year, year, month) %>%
  summarize(total_grass = sum(totalarea)) %>%
  merge(quaddates, all=T)
totalgrass$total_grass[is.na(totalgrass$total_grass)] <- 0

forb = perenn_spchanged %>%
  dplyr::filter(is.na(area)) %>%
  merge(splist, by.x='species', by.y='species_code') %>%
  count(quadrat, project_year, year, month) %>%
  merge(quaddates, all=T)
forb$n[is.na(forb$n)] <- 0

# put data frame together and save to csv
quadrat_veg = merge(totalshrub, totalgrass, all=T) %>%
  merge(forb, all=T)
write.csv(quadrat_veg, 'data/quadrat_veg.csv', row.names=F)


# ========================================
# get total counts and cover of all perennial species

total_count_cover = perenn_spchanged %>%
  mutate(count = rep(1))
total_count_cover$area[is.na(total_count_cover$area)] <- .000025

total_counts = total_count_cover %>%
  group_by(quadrat, project_year, year, month, species) %>%
  summarize(count = sum(count),
            cover = sum(area)) %>%
  merge(splist[,c('species_code','form','category')], by.x='species',by.y='species_code',all.x=T) %>%
  arrange(quadrat, year, month, species)


write.csv(total_counts, 'data/all_species_counts_cover.csv', row.names = F)

# ========================================
# get crosstab of species

allveg = read.csv('data/all_species_counts_cover.csv', stringsAsFactors = F)

veg_crosstab = allveg %>%
  arrange(species) %>%
  tidyr::pivot_wider(id_cols=c(quadrat, project_year, year, month),
                     names_from=species, values_from=count, values_fill=list(count=0)) %>%
  arrange(quadrat, year, month)

write.csv(veg_crosstab, 'data/all_veg_count_crosstab.csv', row.names=F)
