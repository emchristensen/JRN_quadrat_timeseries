#' Logistic models: grass loss/gain related to covariates during different time periods
#' EMC 9/9/20

library(dplyr)
#library(MASS)
library(ggplot2)
library(AICcmodavg)
#library(car)
library(MuMIn)

# theil-sen slopes from grass_slopes_1950s_2000s.R
slopes = read.csv('data/slopes_50_60_95.csv', stringsAsFactors = F)
slopes_boer = read.csv('data/slopes_boer_50_60_95.csv', stringsAsFactors = F)
slopes_sporo = read.csv('data/slopes_sporo_50_60_95.csv', stringsAsFactors = F)
slopes_arist = read.csv('data/slopes_arist_50_60_95.csv', stringsAsFactors = F)
slopes_dapu = read.csv('data/slopes_dapu_50_60_95.csv', stringsAsFactors = F)

# covariate data
covariates = read.csv('data/covariates.csv', stringsAsFactors = F)
# some should be factors
covariates$landform_group = as.factor(covariates$landform_group)
#covariates$topo_local = as.factor(covariates$topo_local)
#covariates$topo_patch = as.factor(covariates$topo_patch)
covariates$Pasture_1995to2016 = as.factor(covariates$Pasture_1995to2016)

soil = dplyr::select(covariates, quadrat, clay_deep, coarse_s, fine_s, fine_deep)

# order to topo_local
covariates$topo_local[covariates$topo_local=='slope'] <- 0
covariates$topo_local[covariates$topo_local=='off'] <- -1
covariates$topo_local[covariates$topo_local=='on'] <- 1
covariates$topo_local = as.factor(covariates$topo_local)

# merge slopes with covariates
slope_covars = merge(slopes, covariates, all.x=T)

boer_covars = merge(slopes_boer, covariates, all.x=T)

sporo_covars = merge(slopes_sporo, covariates, all.x=T)

arist_covars = merge(slopes_arist, covariates, all.x=T)

dapu_covars = merge(slopes_dapu, covariates, all.x=T)


# =====================================================
# model 1: grass loss 1945-1956 ----
# https://www.r-bloggers.com/how-to-perform-a-logistic-regression-in-r/
# remove rows with missing values and select parameters
model1data = dplyr::select(slope_covars, significant_50, 
                           #clay_deep, coarse_s, 
                           #clay_s, coarse_deep, fine_s, fine_deep,
                           clay, coarse, 
                           #topo_local, #topo_patch,
                           landform_group) %>%
  dplyr::filter(!is.na(significant_50), !is.na(clay), !is.na(coarse))
# outcome: either slope is significantly negative, or positive/not significant
model1data$outcome = NA
model1data$outcome[model1data$significant_50==1] <- 1
model1data$outcome[model1data$significant_50 %in% c(0,2)] <- 0

# check for class bias
table(model1data$outcome)

# look at range of soil values per landform type
range(model1data$clay[model1data$landform_group=='alluvial_plain_windworked'])
range(model1data$clay[model1data$landform_group=='alluvial_plains'])
range(model1data$clay[model1data$landform_group=='fan_piedmot'])
range(model1data$clay[model1data$landform_group=='gypsiferous'])
range(model1data$clay[model1data$landform_group=='sand_sheets'])

range(model1data$coarse[model1data$landform_group=='alluvial_plain_windworked'])
range(model1data$coarse[model1data$landform_group=='alluvial_plains'])
range(model1data$coarse[model1data$landform_group=='fan_piedmot'])
range(model1data$coarse[model1data$landform_group=='gypsiferous'])
range(model1data$coarse[model1data$landform_group=='sand_sheets'])

model1data = dplyr::select(model1data, -significant_50)
# fit models
model1a = glm(outcome ~ clay + coarse + landform_group,
              family=binomial(link='logit'), data=model1data)
model1b = glm(outcome ~ clay + coarse,
              family=binomial(link='logit'), data=model1data)
model1c = glm(outcome ~ clay + landform_group,
              family=binomial(link='logit'), data=model1data)
model1d = glm(outcome ~ coarse + landform_group,
              family=binomial(link='logit'), data=model1data)
model1e = glm(outcome ~ clay,
              family=binomial(link='logit'), data=model1data)
model1f = glm(outcome ~ coarse,
              family=binomial(link='logit'), data=model1data)
model1g = glm(outcome ~ landform_group,
              family=binomial(link='logit'), data=model1data)
model1h = glm(outcome ~ 1, 
              family=binomial(link='logit'), data=model1data)


cands1 = list(model1a, model1b, model1c, model1d, model1e, model1f, model1g, model1h)
modelnames = c('clay + coarse + topo + landform', 'clay + coarse + topo', 'clay + coarse + landform',
               'clay + topo + landform', 'coarse + topo + landform', 'clay + coarse', 'clay + topo',
               'clay + landform','coarse + topo','coarse + landform','topo + landform','clay','coarse',
               'topo','landform','intercept')
modelnames = c('model1a','model1b','model1c','model1d','model1e','model1f','model1g','model1h')

table1 = aictab(cands1, modelnames)
table1
summary(model1e)

# get model-averaged standardized estimates of parameters ----
# following code from Cade 2015
# get model weights
model1a.wt = table1$AICcWt[table1$Modnames=='model1a']
model1b.wt = table1$AICcWt[table1$Modnames=='model1b']
model1c.wt = table1$AICcWt[table1$Modnames=='model1c']
model1d.wt = table1$AICcWt[table1$Modnames=='model1d']
model1e.wt = table1$AICcWt[table1$Modnames=='model1e']
model1f.wt = table1$AICcWt[table1$Modnames=='model1f']
model1g.wt = table1$AICcWt[table1$Modnames=='model1g']
model1h.wt = table1$AICcWt[table1$Modnames=='model1h']

# use MuMIn package https://rdrr.io/cran/MuMIn/man/std.coef.html
# get standardized model coefficients
std.model1a = std.coef(model1a, partial.sd=T)
std.model1b = std.coef(model1b, partial.sd=T)
std.model1c = std.coef(model1c, partial.sd=T)
std.model1d = std.coef(model1d, partial.sd=T)
std.model1e = std.coef(model1e, partial.sd=T)
std.model1f = std.coef(model1f, partial.sd=T)
std.model1g = std.coef(model1g, partial.sd=T)
std.model1h = std.coef(model1h, partial.sd=T)

# models with clay
#   model1a, model1b, model1c, model1e
# renormalize weights for models with clay
sumclay.wt = model1a.wt +model1b.wt + model1c.wt + model1e.wt

# model-averaged standardized estimates
stdclay.wt = ((std.model1a['clay',1] * model1a.wt) +
  (std.model1b['clay',1] * model1b.wt) +
  (std.model1c['clay',1] * model1c.wt) +
  (std.model1e['clay',1] * model1e.wt))/sumclay.wt

# standard errors for model-averaged standardized estimates
stdclay.se = ((std.model1a['clay',2] * model1a.wt) +
                (std.model1b['clay',2] * model1b.wt) +
                (std.model1c['clay',2] * model1c.wt) +
                (std.model1e['clay',2] * model1e.wt))/sumclay.wt

# models with coarse
#   model1a, model1b, model1d, model1f
# renormalize weights for models with coarse
sumcoarse.wt = model1a.wt +model1b.wt + model1d.wt + model1f.wt

# model-averaged standardized estimates
stdcoarse.wt = ((std.model1a['coarse',1] * model1a.wt) +
                (std.model1b['coarse',1] * model1b.wt) +
                (std.model1d['coarse',1] * model1d.wt) +
                (std.model1f['coarse',1] * model1f.wt))/sumcoarse.wt

# standard errors for model-averaged standardized estimates
stdcoarse.se = ((std.model1a['coarse',2] * model1a.wt) +
                (std.model1b['coarse',2] * model1b.wt) +
                (std.model1d['coarse',2] * model1d.wt) +
                (std.model1f['coarse',2] * model1f.wt))/sumcoarse.wt

# models with landform
#   model1a, model1c, model1d, model1g
# renormalize weights for models with landform
sumlandform.wt = model1a.wt +model1c.wt + model1d.wt + model1g.wt 

# model-averaged standardized estimates - alluvial plains
stdlandformAP.wt = ((std.model1a['landform_groupalluvial_plains',1] * model1a.wt) +
                  (std.model1c['landform_groupalluvial_plains',1] * model1c.wt) +
                  (std.model1d['landform_groupalluvial_plains',1] * model1d.wt) +
                  (std.model1g['landform_groupalluvial_plains',1] * model1g.wt))/sumlandform.wt

# standard errors for model-averaged standardized estimates
stdlandformAP.se = ((std.model1a['landform_groupalluvial_plains',2] * model1a.wt) +
                  (std.model1c['landform_groupalluvial_plains',2] * model1c.wt) +
                  (std.model1d['landform_groupalluvial_plains',2] * model1d.wt) +
                  (std.model1g['landform_groupalluvial_plains',2] * model1g.wt))/sumlandform.wt

# model-averaged standardized estimates - fan piedmont
stdlandformFP.wt = ((std.model1a['landform_groupfan_piedmot',1] * model1a.wt) +
                      (std.model1c['landform_groupfan_piedmot',1] * model1c.wt) +
                      (std.model1d['landform_groupfan_piedmot',1] * model1d.wt) +
                      (std.model1g['landform_groupfan_piedmot',1] * model1g.wt))/sumlandform.wt

# standard errors for model-averaged standardized estimates
stdlandformFP.se = ((std.model1a['landform_groupfan_piedmot',2] * model1a.wt) +
                      (std.model1c['landform_groupfan_piedmot',2] * model1c.wt) +
                      (std.model1d['landform_groupfan_piedmot',2] * model1d.wt) +
                      (std.model1g['landform_groupfan_piedmot',2] * model1g.wt))/sumlandform.wt

# model-averaged standardized estimates - gypsiferous
stdlandformG.wt = ((std.model1a['landform_groupgypsiferous',1] * model1a.wt) +
                      (std.model1c['landform_groupgypsiferous',1] * model1c.wt) +
                      (std.model1d['landform_groupgypsiferous',1] * model1d.wt) +
                      (std.model1g['landform_groupgypsiferous',1] * model1g.wt))/sumlandform.wt

# standard errors for model-averaged standardized estimates
stdlandformG.se = ((std.model1a['landform_groupgypsiferous',2] * model1a.wt) +
                      (std.model1c['landform_groupgypsiferous',2] * model1c.wt) +
                      (std.model1d['landform_groupgypsiferous',2] * model1d.wt) +
                      (std.model1g['landform_groupgypsiferous',2] * model1g.wt))/sumlandform.wt

# model-averaged standardized estimates - sand sheets
stdlandformSS.wt = ((std.model1a['landform_groupsand_sheets',1] * model1a.wt) +
                     (std.model1c['landform_groupsand_sheets',1] * model1c.wt) +
                     (std.model1d['landform_groupsand_sheets',1] * model1d.wt) +
                     (std.model1g['landform_groupsand_sheets',1] * model1g.wt))/sumlandform.wt

# standard errors for model-averaged standardized estimates
stdlandformSS.se = ((std.model1a['landform_groupsand_sheets',2] * model1a.wt) +
                     (std.model1c['landform_groupsand_sheets',2] * model1c.wt) +
                     (std.model1d['landform_groupsand_sheets',2] * model1d.wt) +
                     (std.model1g['landform_groupsand_sheets',2] * model1g.wt))/sumlandform.wt

# write results to csv
# gather together estimates and standard errors, write to csv
model1_standardized_estimates = data.frame(model = c('1945_1956_grass_loss',NA,NA,NA,NA,NA),
                                           variable = c('clay','coarse','Alluvial_Plains','Fan_Piedmont','Gypsiferous',
                                                        'Sand_Sheets'),
                                           standardized_estimate = c(stdclay.wt, stdcoarse.wt, stdlandformAP.wt, 
                                                                     stdlandformFP.wt, stdlandformG.wt, stdlandformSS.wt),
                                           standardized_se = c(stdclay.se, stdcoarse.se, stdlandformAP.se, stdlandformFP.se,
                                                               stdlandformG.se, stdlandformSS.se))
write.csv(model1_standardized_estimates, 'trends/parameter_estimates_1945_1956_grass_loss.csv', row.names=F)



ggplot(model1_standardized_estimates, aes(y=std_estimates, x=variable)) +
  geom_bar(stat='identity')

# ==========================================
# model 2: grass gain 1955-1980 ----
# remove rows with missing values and select parameters
model2data = dplyr::select(slope_covars, significant_60, 
                           clay, coarse,
                           #topo_local, 
                           landform_group) %>%
  dplyr::filter(!is.na(significant_60), !is.na(clay), !is.na(coarse))
# outcome: either slope is significantly positive, or negative/not significant
model2data$outcome = NA
model2data$outcome[model2data$significant_60==2] <- 1
model2data$outcome[model2data$significant_60 %in% c(0,1)] <- 0

# check for class bias
table(model2data$outcome)
# about even

model2data = dplyr::select(model2data, -significant_60)
# fit models
model2a = glm(outcome ~ clay + coarse + landform_group,
              family=binomial(link='logit'), data=model2data)
model2b = glm(outcome ~ clay + coarse,
              family=binomial(link='logit'), data=model2data)
model2c = glm(outcome ~ clay + landform_group,
              family=binomial(link='logit'), data=model2data)
model2d = glm(outcome ~ coarse + landform_group,
              family=binomial(link='logit'), data=model2data)
model2e = glm(outcome ~ clay,
              family=binomial(link='logit'), data=model2data)
model2f = glm(outcome ~ coarse,
              family=binomial(link='logit'), data=model2data)
model2g = glm(outcome ~ landform_group,
              family=binomial(link='logit'), data=model2data)
model2h = glm(outcome ~ 1, 
              family=binomial(link='logit'), data=model2data)

cands2 = list(model2a, model2b, model2c, model2d, model2e, model2f, model2g, model2h)
modelnames2 = c('model2a','model2b','model2c','model2d','model2e','model2f','model2g','model2h')

table2 = aictab(cands2, modelnames2)
table2
summary(model2h)
# get model-averaged standardized estimates of parameters ----
# following code from Cade 2015
# get model weights
model2a.wt = table2$AICcWt[table2$Modnames=='model2a']
model2b.wt = table2$AICcWt[table2$Modnames=='model2b']
model2c.wt = table2$AICcWt[table2$Modnames=='model2c']
model2d.wt = table2$AICcWt[table2$Modnames=='model2d']
model2e.wt = table2$AICcWt[table2$Modnames=='model2e']
model2f.wt = table2$AICcWt[table2$Modnames=='model2f']
model2g.wt = table2$AICcWt[table2$Modnames=='model2g']
model2h.wt = table2$AICcWt[table2$Modnames=='model2h']

# use MuMIn package https://rdrr.io/cran/MuMIn/man/std.coef.html
# get standardized model coefficients
std.model2a = std.coef(model2a, partial.sd=T)
std.model2b = std.coef(model2b, partial.sd=T)
std.model2c = std.coef(model2c, partial.sd=T)
std.model2d = std.coef(model2d, partial.sd=T)
std.model2e = std.coef(model2e, partial.sd=T)
std.model2f = std.coef(model2f, partial.sd=T)
std.model2g = std.coef(model2g, partial.sd=T)
std.model2h = std.coef(model2h, partial.sd=T)

# models with clay
# renormalize weights for models with clay
sumclay2.wt = model2a.wt +model2b.wt + model2c.wt + model2e.wt

# model-averaged standardized estimates
stdclay2.wt = ((std.model2a['clay',1] * model2a.wt) +
                (std.model2b['clay',1] * model2b.wt) +
                (std.model2c['clay',1] * model2c.wt) +
                (std.model2e['clay',1] * model2e.wt))/sumclay2.wt

# standard errors for model-averaged standardized estimates
stdclay2.se = ((std.model2a['clay',2] * model2a.wt) +
                (std.model2b['clay',2] * model2b.wt) +
                (std.model2c['clay',2] * model2c.wt) +
                (std.model2e['clay',2] * model2e.wt))/sumclay2.wt

# models with coarse
#   model1a, model1b, model1d, model1f
# renormalize weights for models with coarse
sumcoarse2.wt = model2a.wt +model2b.wt + model2d.wt + model2f.wt

# model-averaged standardized estimates
stdcoarse2.wt = ((std.model2a['coarse',1] * model2a.wt) +
                  (std.model2b['coarse',1] * model2b.wt) +
                  (std.model2d['coarse',1] * model2d.wt) +
                  (std.model2f['coarse',1] * model2f.wt))/sumcoarse2.wt

# standard errors for model-averaged standardized estimates
stdcoarse2.se = ((std.model2a['coarse',2] * model2a.wt) +
                  (std.model2b['coarse',2] * model2b.wt) +
                  (std.model2d['coarse',2] * model2d.wt) +
                  (std.model2f['coarse',2] * model2f.wt))/sumcoarse2.wt

# models with landform
#   model1a, model1c, model1d, model1g
# renormalize weights for models with landform
sumlandform2.wt = model2a.wt +model2c.wt + model2d.wt + model2g.wt

# model-averaged standardized estimates - alluvial plain
stdlandformAP2.wt = ((std.model2a['landform_groupalluvial_plains',1] * model2a.wt) +
                      (std.model2c['landform_groupalluvial_plains',1] * model2c.wt) +
                      (std.model2d['landform_groupalluvial_plains',1] * model2d.wt) +
                      (std.model2g['landform_groupalluvial_plains',1] * model2g.wt))/sumlandform2.wt

# standard errors for model-averaged standardized estimates
stdlandformAP2.se = ((std.model2a['landform_groupalluvial_plains',2] * model2a.wt) +
                      (std.model2c['landform_groupalluvial_plains',2] * model2c.wt) +
                      (std.model2d['landform_groupalluvial_plains',2] * model2d.wt) +
                      (std.model2g['landform_groupalluvial_plains',2] * model2g.wt))/sumlandform2.wt

# model-averaged standardized estimates - gypsiferous
stdlandformG2.wt = ((std.model2a['landform_groupgypsiferous',1] * model2a.wt) +
                     (std.model2c['landform_groupgypsiferous',1] * model2c.wt) +
                     (std.model2d['landform_groupgypsiferous',1] * model2d.wt) +
                     (std.model2g['landform_groupgypsiferous',1] * model2g.wt))/sumlandform2.wt

# standard errors for model-averaged standardized estimates
stdlandformG2.se = ((std.model2a['landform_groupgypsiferous',2] * model2a.wt) +
                     (std.model2c['landform_groupgypsiferous',2] * model2c.wt) +
                     (std.model2d['landform_groupgypsiferous',2] * model2d.wt) +
                     (std.model2g['landform_groupgypsiferous',2] * model2g.wt))/sumlandform2.wt

# model-averaged standardized estimates - sand sheets
stdlandformSS2.wt = ((std.model2a['landform_groupsand_sheets',1] * model2a.wt) +
                      (std.model2c['landform_groupsand_sheets',1] * model2c.wt) +
                      (std.model2d['landform_groupsand_sheets',1] * model2d.wt) +
                      (std.model2g['landform_groupsand_sheets',1] * model2g.wt))/sumlandform2.wt

# standard errors for model-averaged standardized estimates
stdlandformSS2.se = ((std.model2a['landform_groupsand_sheets',2] * model2a.wt) +
                      (std.model2c['landform_groupsand_sheets',2] * model2c.wt) +
                      (std.model2d['landform_groupsand_sheets',2] * model2d.wt) +
                      (std.model2g['landform_groupsand_sheets',2] * model2g.wt))/sumlandform2.wt

# gather together estimates and standard errors, write to csv
model2_standardized_estimates = data.frame(model = c('1955_1980_grass_gain',NA,NA,NA,NA),
                                           variable = c('clay','coarse','Alluvial_Plains','Gypsiferous',
                                                        'Sand_Sheets'),
                                           standardized_estimate = c(stdclay2.wt, stdcoarse2.wt, stdlandformAP2.wt, 
                                                                     stdlandformG2.wt, stdlandformSS2.wt),
                                           standardized_se = c(stdclay2.se, stdcoarse2.se, stdlandformAP2.se,
                                                               stdlandformG2.se, stdlandformSS2.se))
write.csv(model2_standardized_estimates, 'trends/parameter_estimates_1955_1980_grass_gain.csv', row.names=F)


# =========================================
# model 3: boer loss 1945-1956 ----
model3data = dplyr::select(boer_covars, significant_50,
                           clay_deep, coarse_s, 
                           clay_s, coarse_deep, fine_s, fine_deep,
                           topo_local, topo_patch,
                           landform_code) %>%
  dplyr::filter(!is.na(significant_50), !is.na(clay_deep), !is.na(fine_s), !is.na(fine_deep))
# outcome: slope negative vs. slope postivie or 0
model3data$outcome = NA
model3data$outcome[model3data$significant_50==1] <- 1
model3data$outcome[model3data$significant_50 %in% c(0,2)] <- 0
# check for class bias
table(model3data$outcome)

model3data = dplyr::select(model3data, -significant_50)
# fit model
model3 = glm(outcome ~ .,
             family=binomial(link='logit'), data=model3data) %>%
  stepAIC()
summary(model3)

anova(model3, test='Chisq')
# kept variables: fine_s, topo_local

# ==========================================
# model 4: SPORO loss 1945-1956
model4data = dplyr::select(sporo_covars, significant_50,
                           clay_deep, coarse_s, 
                           clay_s, coarse_deep, fine_s, fine_deep,
                           topo_local, topo_patch,
                           landform_code) %>%
  dplyr::filter(!is.na(significant_50), !is.na(clay_deep), !is.na(fine_s), !is.na(fine_deep))
# outcome: slope negative vs. slope postivie or 0
model4data$outcome = NA
model4data$outcome[model4data$significant_50==1] <- 1
model4data$outcome[model4data$significant_50 %in% c(0,2)] <- 0
# check for class bias
table(model4data$outcome)

model4data = dplyr::select(model4data, -significant_50)
# fit model
model4 = glm(outcome ~ .,
             family=binomial(link='logit'), data=model4data) %>%
  stepAIC()
summary(model4)
# none of the covariates are significant
anova(model4, test='Chisq')
# kept variable: topo_patch

# ===========================================
# model 5: ARIST loss 1945-1956
model5data = dplyr::select(arist_covars, significant_50,
                           clay_deep, coarse_s, 
                           clay_s, coarse_deep, fine_s, fine_deep,
                           topo_local, topo_patch,
                           landform_code) %>%
  dplyr::filter(!is.na(significant_50), !is.na(clay_deep), !is.na(fine_s), !is.na(fine_deep))
# outcome: slope negative vs. slope postivie or 0
model5data$outcome = NA
model5data$outcome[model5data$significant_50==1] <- 1
model5data$outcome[model5data$significant_50 %in% c(0,2)] <- 0
# check for class bias
table(model5data$outcome)

model5data = dplyr::select(model5data, -significant_50)
# fit model
model5 = glm(outcome ~ .,
             family=binomial(link='logit'), data=model5data) %>%
  stepAIC()
summary(model5)
# deep clay is almost significant
anova(model5, test='Chisq')
# kept variables: clay_deep, fine_s

# ===========================================
# model 6: SPORO gain 1955-1980
model6data = dplyr::select(sporo_covars, significant_60,
                           clay_deep, coarse_s, 
                           clay_s, coarse_deep, fine_s, fine_deep,
                           topo_local, topo_patch,
                           landform_code) %>%
  dplyr::filter(!is.na(significant_60), !is.na(clay_deep), !is.na(fine_s), !is.na(fine_deep))
# outcome: slope positive vs. slope negative or 0
model6data$outcome = NA
model6data$outcome[model6data$significant_60==2] <- 1
model6data$outcome[model6data$significant_60 %in% c(0,1)] <- 0
# check for class bias
table(model6data$outcome)

model6data = dplyr::select(model6data, -significant_60)
# fit model
model6 = glm(outcome ~ .,
             family=binomial(link='logit'), data=model6data) %>%
  stepAIC()
summary(model6)
# none of the covariates are significant
anova(model6, test='Chisq')
# kept variables: fine_s, landform

# ========================================
# model 7: DAPU gain 1955-1980
model7data = dplyr::select(dapu_covars, significant_60,
                           clay_deep, coarse_s, 
                           clay_s, coarse_deep, fine_s, fine_deep,
                           topo_local, topo_patch,
                           landform_code) %>%
  dplyr::filter(!is.na(significant_60),  !is.na(clay_deep), !is.na(fine_s), !is.na(fine_deep))
# outcome: slope positive vs. slope negative or 0
model7data$outcome = NA
model7data$outcome[model7data$significant_60==2] <- 1
model7data$outcome[model7data$significant_60 %in% c(0,1)] <- 0
# check for class bias
table(model7data$outcome)

model7data = dplyr::select(model7data, -significant_60)
# fit model
model7 = glm(outcome ~ .,
             family=binomial(link='logit'), data=model7data) %>%
  stepAIC()
summary(model7)
# coarse sand
anova(model7, test='Chisq')
# kept variables: coarse_deep, fine_deep, topo_local, landform

# ======================================
# model 8: grass loss 1995-2016 ----
model8data = dplyr::select(slope_covars, significant_95, 
                           clay, coarse, 
                           #clay_s, coarse_deep, fine_s, fine_deep,
                           #topo_local, topo_patch,
                           landform_group, mean_shrub) %>%
  dplyr::filter(!is.na(significant_95), !is.na(clay), !is.na(coarse))
# outcome: slope negative vs. slope positive or 0
model8data$outcome = NA
model8data$outcome[model8data$significant_95==1] <- 1
model8data$outcome[model8data$significant_95 %in% c(0,2)] <- 0

table(model8data$outcome)

model8data = dplyr::select(model8data, -significant_95)
# fit model
model8a = glm(outcome ~ clay + coarse + landform_group + mean_shrub, family=binomial(link='logit'),
              data=model8data)
model8b = glm(outcome ~ clay + coarse + landform_group, family=binomial(link='logit'),
              data=model8data)
model8c = glm(outcome ~ clay + coarse + mean_shrub, family=binomial(link='logit'),
              data=model8data)
model8d = glm(outcome ~ clay + landform_group + mean_shrub, family=binomial(link='logit'),
              data=model8data)
model8e = glm(outcome ~ coarse + landform_group + mean_shrub, family=binomial(link='logit'),
              data=model8data)
model8f = glm(outcome ~ clay + coarse, family=binomial(link='logit'),
              data=model8data)
model8g = glm(outcome ~ clay + landform_group, family=binomial(link='logit'),
              data=model8data)
model8h = glm(outcome ~ clay + mean_shrub, family=binomial(link='logit'),
              data=model8data)
model8i = glm(outcome ~ coarse + landform_group, family=binomial(link='logit'),
              data=model8data)
model8j = glm(outcome ~ coarse + mean_shrub, family=binomial(link='logit'),
              data=model8data)
model8k = glm(outcome ~ landform_group + mean_shrub, family=binomial(link='logit'),
              data=model8data)
model8l = glm(outcome ~ clay, family=binomial(link='logit'),
              data=model8data)
model8m = glm(outcome ~ coarse, family=binomial(link='logit'),
              data=model8data)
model8n = glm(outcome ~ landform_group, family=binomial(link='logit'),
              data=model8data)
model8o = glm(outcome ~ mean_shrub, family=binomial(link='logit'),
              data=model8data)
model8p = glm(outcome ~ 1, family=binomial(link='logit'),
              data=model8data)


cands8 = list(model8a, model8b, model8c, model8d, model8e, model8f, model8g, model8h, model8i, model8j,
              model8k, model8l, model8m, model8n, model8o, model8p)
modelnames = c('model8a','model8b','model8c','model8d','model8e','model8f','model8g','model8h','model8i',
               'model8j','model8k','model8l','model8m','model8n','model8o','model8p')

table8 = aictab(cands8, modelnames)
table8
summary(model8o)
# kept variables: mean_shrub

# get model-averaged standardized estimates of parameters ----
# following code from Cade 2015
# get model weights
model8a.wt = table8$AICcWt[table8$Modnames=='model8a']
model8b.wt = table8$AICcWt[table8$Modnames=='model8b']
model8c.wt = table8$AICcWt[table8$Modnames=='model8c']
model8d.wt = table8$AICcWt[table8$Modnames=='model8d']
model8e.wt = table8$AICcWt[table8$Modnames=='model8e']
model8f.wt = table8$AICcWt[table8$Modnames=='model8f']
model8g.wt = table8$AICcWt[table8$Modnames=='model8g']
model8h.wt = table8$AICcWt[table8$Modnames=='model8h']
model8i.wt = table8$AICcWt[table8$Modnames=='model8i']
model8j.wt = table8$AICcWt[table8$Modnames=='model8j']
model8k.wt = table8$AICcWt[table8$Modnames=='model8k']
model8l.wt = table8$AICcWt[table8$Modnames=='model8l']
model8m.wt = table8$AICcWt[table8$Modnames=='model8m']
model8n.wt = table8$AICcWt[table8$Modnames=='model8n']
model8o.wt = table8$AICcWt[table8$Modnames=='model8o']
model8p.wt = table8$AICcWt[table8$Modnames=='model8p']

# use MuMIn package https://rdrr.io/cran/MuMIn/man/std.coef.html
# get standardized model coefficients
std.model8a = std.coef(model8a, partial.sd=T)
std.model8b = std.coef(model8b, partial.sd=T)
std.model8c = std.coef(model8c, partial.sd=T)
std.model8d = std.coef(model8d, partial.sd=T)
std.model8e = std.coef(model8e, partial.sd=T)
std.model8f = std.coef(model8f, partial.sd=T)
std.model8g = std.coef(model8g, partial.sd=T)
std.model8h = std.coef(model8h, partial.sd=T)
std.model8i = std.coef(model8i, partial.sd=T)
std.model8j = std.coef(model8j, partial.sd=T)
std.model8k = std.coef(model8k, partial.sd=T)
std.model8l = std.coef(model8l, partial.sd=T)
std.model8m = std.coef(model8m, partial.sd=T)
std.model8n = std.coef(model8n, partial.sd=T)
std.model8o = std.coef(model8o, partial.sd=T)

# models with clay
#   model1b, model1c, model1d, model1f, model1g, model1h, model1l
# renormalize weights for models with clay
sumclay8.wt = model8a.wt +model8b.wt + model8c.wt + model8d.wt + model8f.wt + model8g.wt + 
  model8h.wt + model8l.wt

# model-averaged standardized estimates
stdclay8.wt = ((std.model8a['clay',1] * model8a.wt) +
                (std.model8b['clay',1] * model8b.wt) +
                (std.model8c['clay',1] * model8c.wt) +
                (std.model8d['clay',1] * model8d.wt) +
                (std.model8f['clay',1] * model8f.wt) +
                (std.model8g['clay',1] * model8g.wt) +
                (std.model8h['clay',1] * model8h.wt) +
                (std.model8l['clay',1] * model8l.wt))/sumclay8.wt

# standard errors for model-averaged standardized estimates
stdclay8.se = ((std.model8a['clay',2] * model8a.wt) +
                (std.model8b['clay',2] * model8b.wt) +
                (std.model8c['clay',2] * model8c.wt) +
                (std.model8d['clay',2] * model8d.wt) +
                (std.model8f['clay',2] * model8f.wt) +
                (std.model8g['clay',2] * model8g.wt) +
                (std.model8h['clay',2] * model8h.wt) +
                (std.model8l['clay',2] * model8l.wt))/sumclay8.wt

# models with coarse
#   model1b, model1c, model1e, model1f, model1i, model1j, model1m
# renormalize weights for models with coarse
sumcoarse8.wt = model8a.wt +model8b.wt + model8c.wt + model8e.wt + model8f.wt + model8i.wt + 
  model8j.wt + model8m.wt

# model-averaged standardized estimates
stdcoarse8.wt = ((std.model8a['coarse',1] * model8a.wt) +
                 (std.model8b['coarse',1] * model8b.wt) +
                 (std.model8c['coarse',1] * model8c.wt) +
                 (std.model8e['coarse',1] * model8e.wt) +
                 (std.model8f['coarse',1] * model8f.wt) +
                 (std.model8i['coarse',1] * model8i.wt) +
                 (std.model8j['coarse',1] * model8j.wt) +
                 (std.model8m['coarse',1] * model8m.wt))/sumcoarse8.wt

# standard errors for model-averaged standardized estimates
stdcoarse8.se = ((std.model8a['coarse',2] * model8a.wt) +
                 (std.model8b['coarse',2] * model8b.wt) +
                 (std.model8c['coarse',2] * model8c.wt) +
                 (std.model8e['coarse',2] * model8e.wt) +
                 (std.model8f['coarse',2] * model8f.wt) +
                 (std.model8i['coarse',2] * model8i.wt) +
                 (std.model8j['coarse',2] * model8j.wt) +
                 (std.model8m['coarse',2] * model8m.wt))/sumcoarse8.wt

# models with mean_shrub
#   model1c, model1d, model1e, model1h, model1j, model1k, model1o
# renormalize weights for models with clay
sumshrub8.wt = model8a.wt +model8c.wt + model8d.wt + model8e.wt + model8h.wt + model8j.wt + 
  model8k.wt + model8o.wt

# model-averaged standardized estimates
stdshrub8.wt = ((std.model8a['mean_shrub',1] * model8a.wt) +
                 (std.model8c['mean_shrub',1] * model8c.wt) +
                 (std.model8d['mean_shrub',1] * model8d.wt) +
                 (std.model8e['mean_shrub',1] * model8e.wt) +
                 (std.model8h['mean_shrub',1] * model8h.wt) +
                 (std.model8j['mean_shrub',1] * model8j.wt) +
                 (std.model8k['mean_shrub',1] * model8k.wt) +
                 (std.model8o['mean_shrub',1] * model8o.wt))/sumshrub8.wt

# standard errors for model-averaged standardized estimates
stdshrub8.se = ((std.model8a['mean_shrub',2] * model8a.wt) +
                 (std.model8c['mean_shrub',2] * model8c.wt) +
                 (std.model8d['mean_shrub',2] * model8d.wt) +
                 (std.model8e['mean_shrub',2] * model8e.wt) +
                 (std.model8h['mean_shrub',2] * model8h.wt) +
                 (std.model8j['mean_shrub',2] * model8j.wt) +
                 (std.model8k['mean_shrub',2] * model8k.wt) +
                 (std.model8o['mean_shrub',2] * model8o.wt))/sumshrub8.wt

# models with landform_group
#   model1b, model1d, model1e, model1g, model1i, model1k, model1n
# renormalize weights for models with clay
sumlandform8.wt = model8a.wt +model8b.wt + model8d.wt + model8e.wt + model8g.wt + model8i.wt + 
  model8k.wt + model8n.wt

# model-averaged standardized estimates -- Alluvial Plain
stdAP8.wt = ((std.model8a['landform_groupalluvial_plains',1] * model8a.wt) +
                  (std.model8b['landform_groupalluvial_plains',1] * model8b.wt) +
                  (std.model8d['landform_groupalluvial_plains',1] * model8d.wt) +
                  (std.model8e['landform_groupalluvial_plains',1] * model8e.wt) +
                  (std.model8g['landform_groupalluvial_plains',1] * model8g.wt) +
                  (std.model8i['landform_groupalluvial_plains',1] * model8i.wt) +
                  (std.model8k['landform_groupalluvial_plains',1] * model8k.wt) +
                  (std.model8n['landform_groupalluvial_plains',1] * model8n.wt))/sumlandform8.wt

# standard errors for model-averaged standardized estimates
stdAP8.se = ((std.model8a['landform_groupalluvial_plains',2] * model8a.wt) +
                  (std.model8b['landform_groupalluvial_plains',2] * model8b.wt) +
                  (std.model8d['landform_groupalluvial_plains',2] * model8d.wt) +
                  (std.model8e['landform_groupalluvial_plains',2] * model8e.wt) +
                  (std.model8g['landform_groupalluvial_plains',2] * model8g.wt) +
                  (std.model8i['landform_groupalluvial_plains',2] * model8i.wt) +
                  (std.model8k['landform_groupalluvial_plains',2] * model8k.wt) +
                  (std.model8n['landform_groupalluvial_plains',2] * model8n.wt))/sumlandform8.wt

# model-averaged standardized estimates -- Fan Piedmont
stdFP8.wt = ((std.model8a['landform_groupfan_piedmot',1] * model8a.wt) +
               (std.model8b['landform_groupfan_piedmot',1] * model8b.wt) +
               (std.model8d['landform_groupfan_piedmot',1] * model8d.wt) +
               (std.model8e['landform_groupfan_piedmot',1] * model8e.wt) +
               (std.model8g['landform_groupfan_piedmot',1] * model8g.wt) +
               (std.model8i['landform_groupfan_piedmot',1] * model8i.wt) +
               (std.model8k['landform_groupfan_piedmot',1] * model8k.wt) +
               (std.model8n['landform_groupfan_piedmot',1] * model8n.wt))/sumlandform8.wt

# standard errors for model-averaged standardized estimates
stdFP8.se = ((std.model8a['landform_groupfan_piedmot',2] * model8a.wt) +
               (std.model8b['landform_groupfan_piedmot',2] * model8b.wt) +
               (std.model8d['landform_groupfan_piedmot',2] * model8d.wt) +
               (std.model8e['landform_groupfan_piedmot',2] * model8e.wt) +
               (std.model8g['landform_groupfan_piedmot',2] * model8g.wt) +
               (std.model8i['landform_groupfan_piedmot',2] * model8i.wt) +
               (std.model8k['landform_groupfan_piedmot',2] * model8k.wt) +
               (std.model8n['landform_groupfan_piedmot',2] * model8n.wt))/sumlandform8.wt

# model-averaged standardized estimates -- Gypsiferous
stdG8.wt = ((std.model8a['landform_groupgypsiferous',1] * model8a.wt) +
               (std.model8b['landform_groupgypsiferous',1] * model8b.wt) +
               (std.model8d['landform_groupgypsiferous',1] * model8d.wt) +
               (std.model8e['landform_groupgypsiferous',1] * model8e.wt) +
               (std.model8g['landform_groupgypsiferous',1] * model8g.wt) +
               (std.model8i['landform_groupgypsiferous',1] * model8i.wt) +
               (std.model8k['landform_groupgypsiferous',1] * model8k.wt) +
               (std.model8n['landform_groupgypsiferous',1] * model8n.wt))/sumlandform8.wt

# standard errors for model-averaged standardized estimates
stdG8.se = ((std.model8a['landform_groupgypsiferous',2] * model8a.wt) +
               (std.model8b['landform_groupgypsiferous',2] * model8b.wt) +
               (std.model8d['landform_groupgypsiferous',2] * model8d.wt) +
               (std.model8e['landform_groupgypsiferous',2] * model8e.wt) +
               (std.model8g['landform_groupgypsiferous',2] * model8g.wt) +
               (std.model8i['landform_groupgypsiferous',2] * model8i.wt) +
               (std.model8k['landform_groupgypsiferous',2] * model8k.wt) +
               (std.model8n['landform_groupgypsiferous',2] * model8n.wt))/sumlandform8.wt

# model-averaged standardized estimates -- Lake Plain
stdLP8.wt = ((std.model8a['landform_grouplake_plain',1] * model8a.wt) +
              (std.model8b['landform_grouplake_plain',1] * model8b.wt) +
              (std.model8d['landform_grouplake_plain',1] * model8d.wt) +
              (std.model8e['landform_grouplake_plain',1] * model8e.wt) +
              (std.model8g['landform_grouplake_plain',1] * model8g.wt) +
              (std.model8i['landform_grouplake_plain',1] * model8i.wt) +
              (std.model8k['landform_grouplake_plain',1] * model8k.wt) +
              (std.model8n['landform_grouplake_plain',1] * model8n.wt))/sumlandform8.wt

# standard errors for model-averaged standardized estimates
stdLP8.se = ((std.model8a['landform_grouplake_plain',2] * model8a.wt) +
              (std.model8b['landform_grouplake_plain',2] * model8b.wt) +
              (std.model8d['landform_grouplake_plain',2] * model8d.wt) +
              (std.model8e['landform_grouplake_plain',2] * model8e.wt) +
              (std.model8g['landform_grouplake_plain',2] * model8g.wt) +
              (std.model8i['landform_grouplake_plain',2] * model8i.wt) +
              (std.model8k['landform_grouplake_plain',2] * model8k.wt) +
              (std.model8n['landform_grouplake_plain',2] * model8n.wt))/sumlandform8.wt

# model-averaged standardized estimates -- Sand Sheets
stdSS8.wt = ((std.model8a['landform_groupsand_sheets',1] * model8a.wt) +
               (std.model8b['landform_groupsand_sheets',1] * model8b.wt) +
               (std.model8d['landform_groupsand_sheets',1] * model8d.wt) +
               (std.model8e['landform_groupsand_sheets',1] * model8e.wt) +
               (std.model8g['landform_groupsand_sheets',1] * model8g.wt) +
               (std.model8i['landform_groupsand_sheets',1] * model8i.wt) +
               (std.model8k['landform_groupsand_sheets',1] * model8k.wt) +
               (std.model8n['landform_groupsand_sheets',1] * model8n.wt))/sumlandform8.wt

# standard errors for model-averaged standardized estimates
stdSS8.se = ((std.model8a['landform_groupsand_sheets',2] * model8a.wt) +
               (std.model8b['landform_groupsand_sheets',2] * model8b.wt) +
               (std.model8d['landform_groupsand_sheets',2] * model8d.wt) +
               (std.model8e['landform_groupsand_sheets',2] * model8e.wt) +
               (std.model8g['landform_groupsand_sheets',2] * model8g.wt) +
               (std.model8i['landform_groupsand_sheets',2] * model8i.wt) +
               (std.model8k['landform_groupsand_sheets',2] * model8k.wt) +
               (std.model8n['landform_groupsand_sheets',2] * model8n.wt))/sumlandform8.wt

# alluvial plain wind worked is the final category of landform_group but all the intercepts are 0

# gather together estimates and standard errors, write to csv
model8_standardized_estimates = data.frame(model = c('1996_2016_grass_loss',NA,NA,NA,NA,NA,NA,NA),
           variable = c('clay','coarse','mean_shrub','Alluvial_Plains','Fan_Piedmont','Gypsiferous',
                        'Lake_Plain','Sand_Sheets'),
           standardized_estimate = c(stdclay8.wt, stdcoarse8.wt, stdshrub8.wt, stdAP8.wt, stdFP8.wt,
                                     stdG8.wt, stdLP8.wt, stdSS8.wt),
           standardized_se = c(stdclay8.se, stdcoarse8.se, stdshrub8.se, stdAP8.se, stdFP8.se,
                               stdG8.se, stdLP8.se, stdSS8.se))
write.csv(model8_standardized_estimates, 'trends/parameter_estimates_1995_2016_grass_loss.csv', row.names=F)
