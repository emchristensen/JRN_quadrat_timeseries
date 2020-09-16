#' Logistic models: grass loss/gain related to covariates
#' EMC 9/9/20

library(dplyr)
library(ggplot2)

# theil-sen slopes from grass_slopes_1950s_2000s.R
slopes = read.csv('data/slopes_50_60_95.csv', stringsAsFactors = F)
slopes_boer = read.csv('data/slopes_boer_50_60_95.csv', stringsAsFactors = F)
slopes_sporo = read.csv('data/slopes_sporo_50_60_95.csv', stringsAsFactors = F)
slopes_arist = read.csv('data/slopes_arist_50_60_95.csv', stringsAsFactors = F)
slopes_dapu = read.csv('data/slopes_dapu_50_60_95.csv', stringsAsFactors = F)

# other covariates
datarepo = '../JRN_quadrat_datapaper/'
landforms <- read.csv(paste0(datarepo,'SiteAndMethods/Jornada_quadrat_landforms.csv'), stringsAsFactors = T)
soil_raw <- read.csv(paste0(datarepo,'Soil/Jornada_quadrat_soil_PSA.csv'), stringsAsFactors = F)
soil_deep = dplyr::filter(soil_raw, depth_layer=='deep') %>% 
  dplyr::select(quadrat, sand_deep=pct_sand, silt_deep=pct_silt, clay_deep=pct_clay, vfs_deep=pct_vfs,
                fs_deep=pct_vfs, ms_deep=pct_ms, cos_deep=pct_cos, vcs_deep=pct_vcs)
soil_shallow = dplyr::filter(soil_raw, depth_layer=='shallow') %>% 
  dplyr::select(quadrat, sand_s=pct_sand, silt_s=pct_silt, clay_s=pct_clay, vfs_s=pct_vfs,
                fs_s=pct_fs, ms_s=pct_ms, cos_s=pct_cos, vcs_s=pct_vcs)
soil = merge(soil_deep, soil_shallow, all=T)
soil$coarse_deep = (soil$vcs_deep + soil$cos_deep)/100 * soil$sand_deep/100 * 100
soil$coarse_s = (soil$vcs_s + soil$cos_s)/100 * soil$sand_s/100 * 100
soil$fine_deep = (soil$vfs_deep + soil$fs_deep)/100 * soil$sand_deep/100 * 100
soil$fine_s = (soil$vfs_s + soil$fs_s)/100 * soil$sand_s/100 * 100
#soil_depth <- read.csv(paste0(datarepo,'Soil/Jornada_quadrat_soil_caliche_depth.csv'), stringsAsFactors = F)
topo <- read.csv(paste0(datarepo,'SiteandMethods/Jornada_quadrat_topography.csv'), stringsAsFactors = F)
# topo categories are ordered factors
topo$topo_local = as.factor(topo$topo_local)
topo$topo_patch = as.factor(topo$topo_patch)


#pasture <- read.csv(paste0(datarepo,'SiteandMethods/Jornada_quadrat_pastures.csv'), stringsAsFactors = F)

# which quadrats have all the soil data
complete_soil = na.omit(soil)

# merge slopes with covariates
slope_covars = merge(slopes, soil, all.x=T) %>%
  merge(topo, all.x=T) %>%
  merge(landforms, all.x=T)

boer_covars = merge(slopes_boer, soil, all.x=T) %>%
  merge(topo, all.x=T) %>%
  merge(landforms, all.x=T)

sporo_covars = merge(slopes_sporo, soil, all.x=T) %>%
  merge(topo, all.x=T) %>%
  merge(landforms, all.x=T)

arist_covars = merge(slopes_arist, soil, all.x=T) %>%
  merge(topo, all.x=T) %>%
  merge(landforms, all.x=T)

dapu_covars = merge(slopes_dapu, soil, all.x=T) %>%
  merge(topo, all.x=T) %>%
  merge(landforms, all.x=T)

# ======================================================
# look at covariates ----

# look at correlation between shallow sand and deep clay
plot(soil$sand_s, soil$clay_deep)
claysand = lm(soil$sand_s ~ soil$clay_deep)
summary(claysand)
# r-sq is .67, meaning these variables are highly correlated
cor(complete_soil[,-1])

soil_selected = dplyr::select(complete_soil, quadrat, clay_deep, coarse_s, fine_deep, fine_s)
cor(soil_selected[,-1])

# =====================================================
# model 1: grass loss 1945-1956 ----
# https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
# remove rows with missing values and select parameters
model1data = dplyr::select(slope_covars, significant_50, 
                           clay_deep, coarse_s, 
                           topo_local, topo_patch,
                           landform_code) %>%
  dplyr::filter(!is.na(significant_50), !is.na(clay_deep), !is.na(coarse_s))
# outcome: either slope is significantly negative, or positive/not significant
model1data$outcome = NA
model1data$outcome[model1data$significant_50==1] <- 1
model1data$outcome[model1data$significant_50 %in% c(0,2)] <- 0

# check for class bias
table(model1data$outcome)

# fit model
model1 = glm(outcome ~ clay_deep -1, family=binomial(link='logit'), data=model1data)
summary(model1)
anova(model1, test='Chisq')

# ==========================================
# model 2: grass gain 1955-1980 ----
# remove rows with missing values and select parameters
model2data = dplyr::select(slope_covars, significant_60, 
                           clay_deep, coarse_s, 
                           topo_local, topo_patch,
                           landform_code) %>%
  dplyr::filter(!is.na(significant_60), !is.na(clay_deep), !is.na(coarse_s))
# outcome: either slope is significantly positive, or negative/not significant
model2data$outcome = NA
model2data$outcome[model2data$significant_60==2] <- 1
model2data$outcome[model2data$significant_60 %in% c(0,1)] <- 0

# check for class bias
table(model2data$outcome)
# about even

# fit model
model2 = glm(outcome ~ clay_deep + coarse_s + topo_local + landform_code, 
             family=binomial(link='logit'), data=model2data)
summary(model2)
anova(model2, test='Chisq')
# looks like local topography and landform are important


# =========================================
# model 3: boer loss 1945-1956 ----
model3data = dplyr::select(boer_covars, significant_50,
                           clay_deep, coarse_s,
                           topo_local, topo_patch, landform_code) %>%
  dplyr::filter(!is.na(significant_50))
# outcome: slope negative vs. slope postivie or 0
model3data$outcome = NA
model3data$outcome[model3data$significant_50==1] <- 1
model3data$outcome[model3data$significant_50 %in% c(0,2)] <- 0
# check for class bias
table(model3data$outcome)

# fit model
model3 = glm(outcome ~ clay_deep + coarse_s + topo_local + landform_code,
             family=binomial(link='logit'), data=model3data)
summary(model3)
# none of the covariates are significant
anova(model3, test='Chisq')
# local topography?

# ==========================================
# model 4: SPORO loss 1945-1956
model4data = dplyr::select(sporo_covars, significant_50,
                           clay_deep, coarse_s,
                           topo_local, topo_patch, landform_code) %>%
  dplyr::filter(!is.na(significant_50))
# outcome: slope negative vs. slope postivie or 0
model4data$outcome = NA
model4data$outcome[model4data$significant_50==1] <- 1
model4data$outcome[model4data$significant_50 %in% c(0,2)] <- 0
# check for class bias
table(model4data$outcome)

# fit model
model4 = glm(outcome ~ clay_deep + coarse_s + topo_local + landform_code,
             family=binomial(link='logit'), data=model4data)
summary(model4)
# none of the covariates are significant
anova(model4, test='Chisq')
# none significant

# ===========================================
# model 5: ARIST loss 1945-1956
model5data = dplyr::select(arist_covars, significant_50,
                           clay_deep, coarse_s,
                           topo_local, topo_patch, landform_code) %>%
  dplyr::filter(!is.na(significant_50))
# outcome: slope negative vs. slope postivie or 0
model5data$outcome = NA
model5data$outcome[model5data$significant_50==1] <- 1
model5data$outcome[model5data$significant_50 %in% c(0,2)] <- 0
# check for class bias
table(model5data$outcome)

# fit model
model5 = glm(outcome ~ clay_deep + coarse_s + topo_local + landform_code,
             family=binomial(link='logit'), data=model5data)
summary(model5)
# deep clay is almost significant
anova(model5, test='Chisq')
# landform and deep clay?

# ===========================================
# model 6: SPORO gain 1955-1980
model6data = dplyr::select(sporo_covars, significant_60,
                           clay_deep, coarse_s,
                           topo_local, topo_patch, landform_code) %>%
  dplyr::filter(!is.na(significant_60))
# outcome: slope positive vs. slope negative or 0
model6data$outcome = NA
model6data$outcome[model6data$significant_60==2] <- 1
model6data$outcome[model6data$significant_60 %in% c(0,1)] <- 0
# check for class bias
table(model6data$outcome)

# fit model
model6 = glm(outcome ~ clay_deep + coarse_s + topo_local + landform_code,
             family=binomial(link='logit'), data=model6data)
summary(model6)
# none of the covariates are significant
anova(model6, test='Chisq')
# landform

# ========================================
# model 7: DAPU gain 1955-1980
model7data = dplyr::select(dapu_covars, significant_60,
                           clay_deep, coarse_s,
                           topo_local, topo_patch, landform_code) %>%
  dplyr::filter(!is.na(significant_60))
# outcome: slope positive vs. slope negative or 0
model7data$outcome = NA
model7data$outcome[model7data$significant_60==2] <- 1
model7data$outcome[model7data$significant_60 %in% c(0,1)] <- 0
# check for class bias
table(model7data$outcome)

# fit model
model7 = glm(outcome ~ clay_deep + coarse_s + topo_local + landform_code,
             family=binomial(link='logit'), data=model7data)
summary(model7)
# coarse sand
anova(model7, test='Chisq')
# coarse sand

