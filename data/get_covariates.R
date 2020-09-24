# get covariates from data repository
# EMC 8/20/20

library(dplyr)

# get covariates from Datapaper repo
datarepo = '../JRN_quadrat_datapaper/'

# landforms
landforms <- read.csv(paste0(datarepo,'SiteAndMethods/Jornada_quadrat_landforms.csv'), stringsAsFactors = T)

# soil
soil_raw <- read.csv(paste0(datarepo,'Soil/Jornada_quadrat_soil_PSA.csv'), stringsAsFactors = F)
soil_deep = dplyr::filter(soil_raw, depth_layer=='deep') %>% 
  dplyr::select(quadrat, sand_deep=pct_sand, silt_deep=pct_silt, clay_deep=pct_clay, vfs_deep=pct_vfs,
                fs_deep=pct_vfs, ms_deep=pct_ms, cos_deep=pct_cos, vcs_deep=pct_vcs)
soil_shallow = dplyr::filter(soil_raw, depth_layer=='shallow') %>% 
  dplyr::select(quadrat, sand_s=pct_sand, silt_s=pct_silt, clay_s=pct_clay, vfs_s=pct_vfs,
                fs_s=pct_fs, ms_s=pct_ms, cos_s=pct_cos, vcs_s=pct_vcs)
soil = merge(soil_deep, soil_shallow, all=T)
# coarse sand = vcs + cos; fine sand = vfs + fs
soil$coarse_deep = (soil$vcs_deep + soil$cos_deep)/100 * soil$sand_deep/100 * 100
soil$coarse_s = (soil$vcs_s + soil$cos_s)/100 * soil$sand_s/100 * 100
soil$fine_deep = (soil$vfs_deep + soil$fs_deep)/100 * soil$sand_deep/100 * 100
soil$fine_s = (soil$vfs_s + soil$fs_s)/100 * soil$sand_s/100 * 100

# weighted avg of 2 sand layers
soil$clay = soil$clay_deep*15/20 + soil$clay_s*5/20
soil$sand = soil$sand_deep*15/20 + soil$sand_s*5/20
soil$silt = soil$silt_deep*15/20 + soil$silt_s*5/20
soil$coarse = soil$coarse_deep*15/20 + soil$coarse_s*5/20
soil$fine = soil$fine_deep*15/20 + soil$fine_s*5/20

# topography
topo <- read.csv(paste0(datarepo,'SiteandMethods/Jornada_quadrat_topography.csv'), stringsAsFactors = F)
# topo categories are ordered factors
topo$topo_local = as.factor(topo$topo_local)
topo$topo_patch = as.factor(topo$topo_patch)

# pasture
pasture <- read.csv(paste0(datarepo,'SiteandMethods/Jornada_quadrat_pastures.csv'), stringsAsFactors = F)

# Wenji's shrub layer
shrub <- read.csv('shrub/quadrat_cover.csv', stringsAsFactors = F)


# ======================================================
# look at covariates ----

# look at correlation between shallow sand and deep clay
plot(soil$sand_s, soil$clay_deep)
claysand = lm(soil$sand_s ~ soil$clay_deep)
summary(claysand)
# r-sq is .67, meaning these variables are highly correlated

# correlation between soil components
complete_soil = na.omit(soil)
cor(complete_soil[,-1])

# least correlated: clay_deep, coarse_s, fine_s, fine_deep
# I'm including others for the sake of options. some don't have sand fractionation
soil_selected = dplyr::select(complete_soil, quadrat, clay_deep, clay_s, coarse_s, coarse_deep, 
                              fine_deep, fine_s, sand_deep, sand_s)
cor(soil_selected[,-1])

cor(complete_soil[,c('clay','sand','silt','coarse','fine')])

# =======================================================
# put together covariate table
covariates = dplyr::select(soil, quadrat, clay, coarse, fine, clay_deep, clay_s, sand_deep, sand_s, coarse_deep, coarse_s, fine_deep, fine_s) %>%
  merge(landforms, by='quadrat', all=T) %>%
  merge(topo, by='quadrat', all=T) %>%
  merge(dplyr::select(shrub, quadrat, mean_shrub=mean), by='quadrat', all=T) %>%
  merge(dplyr::select(pasture, quadrat, Pasture_1995to2016), by='quadrat', all=T)

write.csv(covariates, 'data/covariates.csv', row.names=F)
