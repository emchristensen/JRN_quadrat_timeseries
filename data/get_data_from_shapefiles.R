#' merge polygons to get total grass, total shrub, and bare ground
#' EMC 5/24/21

library(dplyr)
library(maptools)
library(ggplot2)

# get species list and dates
splist = read.csv('../JRN_quadrat_datapaper/Plants/Jornada_quadrat_species_list.csv', stringsAsFactors = F)
quaddates = read.csv('data/quadrats_dates_for_analysis.csv')

# get list of perennial grass species and shrub species
grasslist = splist %>%
  dplyr::filter(form=='GRASS', category !='Annual', !species_code %in% c('CYPER','CYRE14'))
shrublist = splist %>%
  dplyr::filter(form=='SHRUB', !species_code %in% c('YUBA','YUEL'))




# get paths to all shapefiles
shapefile_folder = '../JRN_quadrat_datapaper/shapefiles'
quadrats = list.dirs(shapefile_folder)
quadrats = quadrats[-1]

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
    
    # get total area (plot boundary if present)
    if ('Plot boundary' %in% datalayer@data$species) {
      chartarea = datalayer@data$area[datalayer@data$species=='Plot boundary']
    } else {
      chartarea = 1
    }
    
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
    
    #datalayer_union_tidy = broom::tidy(datalayer_planttype)
    

    
    
    
    
    
    # # create plot
    # plt <- ggplot() + 
    #   geom_polygon(data=datalayer_union_tidy, aes(x=long, y=lat, group=group, fill=id)) +
    #   #geom_polygon(data=datalayer_tidy, aes(x=long, y=lat, group=group, fill=species)) +
    #   #scale_fill_manual(name='species', values=myColors) +
    #   xlim(0,1) +
    #   ylim(0,1) +
    #   #ggtitle('A1 1915') +
    #   theme_bw()
    # plt
  }
}

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

# select final columns and save
grassareas = dplyr::select(grassareas, quadrat, year, month, boundary, perennial_cover, nonpernn_cover, 
                           grass_cover, shrub_cover, other_cover, bareground)
write.csv(grassareas, 'data/quadrat_veg_from_spatial.csv', row.names=F)


# =======================================================
# compare to veg data compiled from cover
veg = read.csv('data/quadrat_veg.csv')

test = merge(grassareas, veg, by=c('quadrat','year','month'))



plot(test$grass_cover, test$total_grass)

test$grassdiff = test$grass_cover-test$total_grass

plot(test$grassratio, test$total_grass)

ggplot(test, aes(x=shrub_cover, y=total_shrub)) +
  geom_point() +
  geom_abline(slope=1)
test$shrubdiff =test$shrub_cover-test$total_shrub

# grass + shrub should be > perennial_cover calculated by unioning 
ggplot(test, aes(x=perennial_cover, y=total_shrub+total_grass)) +
  geom_point() +
  geom_abline(slope=1)
