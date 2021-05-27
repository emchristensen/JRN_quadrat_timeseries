#' Get data and prepare for analysis
#' EMC 3/9/20
#' 
#' Shrub species: ATCA2, EPHED, FLCE, KRLA2, LATR2, LYBE, PRGL2, PSSC6, VACO9
#' Grass species: (many)
#' Forb species: includes everything not on shrub or grass list. Does not include yucca or cyperus. Does include cacti and subshrubs.
#' Notes: 
#'     yucca species removed from shrub cover total because they are short-lived compared to the other species
#'     cyperus species removed from grass cover as it is not a true grass
#'     grouped EPTO, EPTR into EPHED
#'     grouped CYRE14 and CYPER 
#'     grouped ARPA9, ARPU9, ARPUL into ARIST
#'     grouped SPCO4, SPCR, SPFL2, SPAI into SPORO
#'
#' last run: 5/26/21

library(dplyr)
library(lubridate)

# read in data
datafolder = '../JRN_quadrat_datapaper/Plants/'
perennials = read.csv(paste0(datafolder, 'Jornada_quadrat_perennials.csv'), stringsAsFactors = F)
dates = read.csv(paste0(datafolder, 'dates/quadrat_sample_dates.csv'), stringsAsFactors = F)
splist = read.csv(paste0(datafolder, 'Jornada_quadrat_species_list.csv'), stringsAsFactors = F)
spchanges = read.csv('data/species_name_changes.csv', stringsAsFactors = F)

# get list of perennial grass species and shrub species
grasslist = splist %>%
  dplyr::filter(form=='GRASS', category !='Annual', !species_code %in% c('CYPER','CYRE14'))
shrublist = splist %>%
  dplyr::filter(form=='SHRUB', !species_code %in% c('YUBA','YUEL'))
forblist = splist %>%
  dplyr::filter(!species_code %in% grasslist$species_code, 
                !species_code %in% shrublist$species_code, 
                !species_code %in% c('CYPER','CYRE14','YUBA','YUEL'),
                category != 'Annual')

#  setup file ----
# quadrats ready to use for analysis 
#    NEW: K4, L5, N1, N2, N3, N3A, N4, N5, N6, P1, P2, P3, P4, P5, R1, R2, R3, R4, T1, T2, T3, T4
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
#   takes only the quadrats listed above
#   takes the latest date per project_year if there are multiple samples in a project_year
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
  arrange(project_year) %>%
  tidyr::pivot_wider(id_cols=quadrat, names_from=project_year, values_from=samples)

write.csv(quaddates_wide, 'data/quadyearcrosstab.csv', row.names=F)


# make species name changes; restrict to just the quaddates from above
perenn_spchanged = perennials %>%
  merge(spchanges, by.x='species_code', by.y='oldspeciescode', all.x=T) %>%
  merge(quaddates)
perenn_spchanged$species = perenn_spchanged$newspeciescode
perenn_spchanged$species[is.na(perenn_spchanged$species)] <- perenn_spchanged$species_code[is.na(perenn_spchanged$species)]


# ========================================
# get total counts and cover of all perennial species ----

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
# create crosstab of species counts (for community ordination) ----

total_counts = read.csv('data/all_species_counts_cover.csv', stringsAsFactors = F)

veg_crosstab = total_counts %>%
  arrange(species) %>%
  tidyr::pivot_wider(id_cols=c(quadrat, project_year, year, month),
                     names_from=species, values_from=count, values_fill=list(count=0)) %>%
  arrange(quadrat, year, month)

write.csv(veg_crosstab, 'data/all_veg_count_crosstab.csv', row.names=F)


# ====================================
# get total cover of grass, shrub, all plants, and bare ground ----
# from shapefiles

# total forb/subshrub count
forb = perenn_spchanged %>%
  dplyr::filter(species_code %in% forblist$species_code) %>%
  count(quadrat, project_year, year, month) %>%
  merge(quaddates, all=T)
forb$n[is.na(forb$n)] <- 0


# get paths to all shapefiles
shapefile_folder = '../JRN_quadrat_datapaper/shapefiles'
quadrats = list.dirs(shapefile_folder)
quadrats = quadrats[-1]

# I don't know why but the union doesn't work without this
require('maptools')
gpclibPermit()

# loop through folders
final_areas = c()
#quad = quadrats[1]
for (quad in quadrats) {
  # get list of polygon layers in the quadrat folder
  polygons = list.files(quad, pattern='\\_pol.shp$', recursive=T)
  
  #loop through charts
  #layer = polygons[6]
  for(layer in polygons) {
    # read in layer
    datalayer = rgdal::readOGR(dsn = paste0(quad,'/',layer))
    
    # get date info from layer name
    quadratname = strsplit(layer, split='_')[[1]][1]
    year = strsplit(layer, split='_')[[1]][2]
    month = strsplit(layer, split='_')[[1]][3]
    
    # create data column for plant/ not plant (boundary) and shrub/grass
    datalayer@data$plant = rep('nonpernn')
    datalayer@data$plant[datalayer@data$species=='Plot boundary'] <- 'boundary'
    datalayer@data$plant[datalayer@data$species %in% c(grasslist$species_code, shrublist$species_code)] <- 'perennial'
    datalayer@data$planttype = rep('other')
    datalayer@data$planttype[datalayer@data$species %in% grasslist$species_code] <- 'grass'
    datalayer@data$planttype[datalayer@data$species %in% shrublist$species_code] <- 'shrub'
    
    # get union of all plant cover, all grass cover, and all shrub cover
    datalayer_union = maptools::unionSpatialPolygons(datalayer, datalayer@data$plant)
    datalayer_planttype = maptools::unionSpatialPolygons(datalayer, datalayer@data$planttype)
    
    # get areas from union calculations
    layer_areas = c()
    for (n in 1:length(datalayer_union@polygons)) {
      layer_areas = rbind(layer_areas, data.frame(ID=datalayer_union@polygons[[n]]@ID,
                                                  area=datalayer_union@polygons[[n]]@area))
    }
    for (n in 1:length(datalayer_planttype@polygons)) {
      layer_areas = rbind(layer_areas, data.frame(ID=datalayer_planttype@polygons[[n]]@ID,
                                                  area=datalayer_planttype@polygons[[n]]@area))
    }
    
    # put area values into data frame
    df_final = merge(layer_areas, data.frame(ID=c('nonpernn','boundary','perennial','other','grass','shrub')),
                     by='ID', all=T) %>%
      tidyr::pivot_wider(names_from=ID, values_from=area) %>%
      mutate(quadrat=quadratname,
             year=year,
             month=month) %>%
      dplyr::select(quadrat, year, month, boundary, perennial, nonpernn, grass, shrub, other)
    
    final_areas = rbind(final_areas, df_final)
  }
}
write.csv(final_areas, 'data/quadrat_grass_veg_areas_raw.csv', row.names=F)

# if there was no boundary polygon, default to 1
grassareas = final_areas
grassareas$boundary[is.na(grassareas$boundary)] <- 1

# fill in zeros
grassareas$grass[is.na(grassareas$grass)] <- 0
grassareas$shrub[is.na(grassareas$shrub)] <- 0
grassareas$other[is.na(grassareas$other)] <- 0
grassareas$perennial[is.na(grassareas$perennial)] <- 0
grassareas$nonpernn[is.na(grassareas$nonpernn)] <- 0

# areas should be normalized by the boundary area, if there is one
grassareas$grass_cover = grassareas$grass/grassareas$boundary
grassareas$shrub_cover = grassareas$shrub/grassareas$boundary
grassareas$other_cover = grassareas$other/grassareas$boundary
grassareas$perennial_cover = grassareas$perennial/grassareas$boundary
grassareas$nonpernn_cover = grassareas$nonpernn/grassareas$boundary

# calculate bare ground
grassareas$bareground = 1-grassareas$perennial_cover

# merge with forb counts and save to csv
grassareas$year = as.integer(grassareas$year)
grassareas$month = as.integer(grassareas$month)
quadrat_veg = grassareas %>%
  merge(forb, by=c('quadrat','year','month'), all.y=T) %>%
  dplyr::select(quadrat, project_year, year, month, day, perennial_cover, 
                           grass_cover, shrub_cover, bareground, n_forbs=n)

# fill in zeros for charts with zero cover
quadrat_veg$perennial_cover[is.na(quadrat_veg$perennial_cover)] <- 0
quadrat_veg$grass_cover[is.na(quadrat_veg$grass_cover)] <- 0
quadrat_veg$shrub_cover[is.na(quadrat_veg$shrub_cover)] <- 0
quadrat_veg$bareground[is.na(quadrat_veg$bareground)] <- 1

write.csv(quadrat_veg, 'data/quadrat_veg.csv', row.names=F)


# ================================================================================================
# oldcode
# 
# # total shrub cover by species
# shrub = perenn_spchanged %>%
#   merge(splist, by.x='species', by.y='species_code', all.x=T) %>%
#   dplyr::filter(form=='SHRUB', !(species %in% c('YUBA','YUEL'))) %>%
#   group_by(quadrat, project_year, year, month, species) %>%
#   summarize(totalarea = sum(area)) 
# 
# shrubfinal = dplyr::select(shrub, quadrat, project_year, year, month, species, totalarea)
# #write.csv(shrubfinal, 'data/shrub_species_totals.csv', row.names = F)
# 
# # total grass cover by species. remove Cyperus -- not technically a grass
# grass = perenn_spchanged %>%
#   merge(splist, by.x='species', by.y='species_code', all.x=T) %>%
#   dplyr::filter(form=='GRASS') %>%
#   dplyr::filter(species!='CYPER') %>%
#   group_by(quadrat, project_year, year, month, species) %>%
#   summarize(totalarea = sum(area)) 
# 
# grassfinal = dplyr::select(grass, quadrat, project_year, year, month, species, totalarea)
# #write.csv(grassfinal, 'data/grass_species_totals.csv', row.names = F)
# 
# # get total grass and shrub covers
# shrubfinal = read.csv('data/shrub_species_totals.csv', stringsAsFactors = F)
# totalshrub = shrubfinal %>%
#   group_by(quadrat, project_year, year, month) %>%
#   summarize(total_shrub = sum(totalarea)) %>%
#   merge(quaddates, all=T)
# totalshrub$total_shrub[is.na(totalshrub$total_shrub)] <- 0
# 
# grassfinal = read.csv('data/grass_species_totals.csv', stringsAsFactors = F)
# totalgrass = grassfinal %>%
#   group_by(quadrat, project_year, year, month) %>%
#   summarize(total_grass = sum(totalarea)) %>%
#   merge(quaddates, all=T)
# totalgrass$total_grass[is.na(totalgrass$total_grass)] <- 0