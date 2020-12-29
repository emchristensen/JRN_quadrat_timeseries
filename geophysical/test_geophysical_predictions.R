# What was the effect of soil clay content and depth to caliche on probability of grass loss
# EMC 12/28/20
# last update: 12/28/20

library(dplyr)
library(ggplot2)


quaddates = read.csv('data/quadrats_dates_for_analysis.csv', stringsAsFactors = F)
covariates = read.csv('data/covariates.csv', stringsAsFactors = F) %>% dplyr::filter(quadrat %in% quaddates$quadrat)

# theil-sen slopes from grass_slopes_1950s_2000s.R
slopes = read.csv('data/slopes_50_60_95.csv', stringsAsFactors = F)
slopes_boer = read.csv('data/slopes_boer_50_60_95.csv', stringsAsFactors = F)
slopes_plmu = read.csv('data/slopes_plmu_50_60_95.csv', stringsAsFactors = F)
slopes_sporo = read.csv('data/slopes_sporo_50_60_95.csv', stringsAsFactors = F)
slopes_scbr = read.csv('data/slopes_scbr_50_60_95.csv', stringsAsFactors = F)
slopes_arist = read.csv('data/slopes_arist_50_60_95.csv', stringsAsFactors = F)
slopes_dapu = read.csv('data/slopes_dapu_50_60_95.csv', stringsAsFactors = F)


# ===========================================
# merge slopes with covariates and set up model data

# total grass
modeldata_all = merge(slopes, covariates, all.x=T) %>%
  dplyr::select(significant_50, slope_50,
                clay, mean_depth,
                landform_group) %>%
  dplyr::filter(!is.na(significant_50))
# outcome: either slope is significantly negative, or positive/not significant
modeldata_all$outcome = NA
modeldata_all$outcome[modeldata_all$significant_50==1] <- 1
modeldata_all$outcome[modeldata_all$significant_50 %in% c(0,2)] <- 0

# BOER
modeldata_boer = merge(slopes_boer, covariates, all.x=T) %>%
  dplyr::select(significant_50, slope_50,
                clay, mean_depth) %>%
  dplyr::filter(!is.na(significant_50))
# outcome: either slope is significantly negative, or positive/not significant
modeldata_boer$outcome = NA
modeldata_boer$outcome[modeldata_boer$significant_50==1] <- 1
modeldata_boer$outcome[modeldata_boer$significant_50 %in% c(0,2)] <- 0

# PLMU
modeldata_plmu = merge(slopes_plmu, covariates, all.x=T) %>%
  dplyr::select(significant_50, slope_50,
                clay, mean_depth) %>%
  dplyr::filter(!is.na(significant_50))
# outcome: either slope is significantly negative, or positive/not significant
modeldata_plmu$outcome = NA
modeldata_plmu$outcome[modeldata_plmu$significant_50==1] <- 1
modeldata_plmu$outcome[modeldata_plmu$significant_50 %in% c(0,2)] <- 0

# SPORO
modeldata_sporo = merge(slopes_sporo, covariates, all.x=T) %>%
  dplyr::select(significant_50, slope_50,
                clay, mean_depth) %>%
  dplyr::filter(!is.na(significant_50))
# outcome: either slope is significantly negative, or positive/not significant
modeldata_sporo$outcome = NA
modeldata_sporo$outcome[modeldata_sporo$significant_50==1] <- 1
modeldata_sporo$outcome[modeldata_sporo$significant_50 %in% c(0,2)] <- 0

# SCBR
modeldata_scbr = merge(slopes_scbr, covariates, all.x=T) %>%
  dplyr::select(significant_50, slope_50,
                clay, mean_depth) %>%
  dplyr::filter(!is.na(significant_50))
# outcome: either slope is significantly negative, or positive/not significant
modeldata_scbr$outcome = NA
modeldata_scbr$outcome[modeldata_scbr$significant_50==1] <- 1
modeldata_scbr$outcome[modeldata_scbr$significant_50 %in% c(0,2)] <- 0

# ARIST
modeldata_arist = merge(slopes_arist, covariates, all.x=T) %>%
  dplyr::select(significant_50, slope_50,
                clay, mean_depth) %>%
  dplyr::filter(!is.na(significant_50))
# outcome: either slope is significantly negative, or positive/not significant
modeldata_arist$outcome = NA
modeldata_arist$outcome[modeldata_arist$significant_50==1] <- 1
modeldata_arist$outcome[modeldata_arist$significant_50 %in% c(0,2)] <- 0

# DAPU
modeldata_dapu = merge(slopes_dapu, covariates, all.x=T) %>%
  dplyr::select(significant_50, slope_50,
                clay, mean_depth) %>%
  dplyr::filter(!is.na(significant_50))
# outcome: either slope is significantly negative, or positive/not significant
modeldata_dapu$outcome = NA
modeldata_dapu$outcome[modeldata_dapu$significant_50==1] <- 1
modeldata_dapu$outcome[modeldata_dapu$significant_50 %in% c(0,2)] <- 0

# ====================
# Test: are grasses more resilient during drought on high clay soils? Yao 2006 found that BOER and SPFL were

# all grass
# plot clay vs slope or outcome
ggplot(modeldata_all, aes(x=outcome, y=clay, group=outcome)) +
  geom_boxplot() + geom_point()
ggplot(modeldata_all, aes(x=clay, y=slope_50)) +
  geom_point()

model1_all = glm(outcome ~ clay,
              family=binomial(link='logit'), data=modeldata_all)
summary(model1_all)

# BOER during drought
# plot clay vs slope or outcome
ggplot(modeldata_boer, aes(x=outcome, y=clay, group=outcome)) +
  geom_boxplot() + geom_point()
ggplot(modeldata_boer, aes(x=clay, y=slope_50)) +
  geom_point()

model1_boer = glm(outcome ~ clay,
              family=binomial(link='logit'), data=modeldata_boer)
summary(model1_boer)

# PLMU during drought
# plot clay vs slope or outcome
ggplot(modeldata_plmu, aes(x=outcome, y=clay, group=outcome)) +
  geom_boxplot() + geom_point()
ggplot(modeldata_plmu, aes(x=clay, y=slope_50)) +
  geom_point()
model1_plmu = glm(outcome ~ clay,
              family=binomial(link='logit'), data=modeldata_plmu)
# too few points
#summary(model1_plmu)

# SPORO during drought
# plot clay vs slope or outcome
ggplot(modeldata_sporo, aes(x=outcome, y=clay, group=outcome)) +
  geom_boxplot() + geom_point()
ggplot(modeldata_sporo, aes(x=clay, y=slope_50)) +
  geom_point()
model1_sporo = glm(outcome ~ clay,
              family=binomial(link='logit'), data=modeldata_sporo)
summary(model1_sporo)

# SCBR during drought
# plot clay vs slope or outcome
ggplot(modeldata_scbr, aes(x=outcome, y=clay, group=outcome)) +
  geom_boxplot() + geom_point()
ggplot(modeldata_scbr, aes(x=clay, y=slope_50)) +
  geom_point()
model1_scbr = glm(outcome ~ clay,
              family=binomial(link='logit'), data=modeldata_scbr)
summary(model1_scbr)

# ARIST during drought
# plot clay vs slope or outcome
ggplot(modeldata_arist, aes(x=outcome, y=clay, group=outcome)) +
  geom_boxplot() + geom_point()
ggplot(modeldata_arist, aes(x=clay, y=slope_50)) +
  geom_point()
model1_arist = glm(outcome ~ clay,
              family=binomial(link='logit'), data=modeldata_arist)
summary(model1_arist)
# almost significant

# Did not model DAPU -- all quads are outcome=0

# ====================
# Test: are grasses more resilient on soils with petrocalcic layer?

# all grass combined
ggplot(modeldata_all, aes(x=outcome, y=mean_depth, group=outcome)) +
  geom_boxplot() + geom_point()
ggplot(modeldata_all, aes(x=slope_50, y=mean_depth)) +
  geom_point()
model2_all = glm(outcome ~ mean_depth,
              family=binomial(link='logit'), data=modeldata_all)
summary(model2_all)

# boer
ggplot(modeldata_boer, aes(x=outcome, y=mean_depth, group=outcome)) +
  geom_boxplot() + geom_point()
ggplot(modeldata_boer, aes(x=slope_50, y=mean_depth)) +
  geom_point()
model2_boer = glm(outcome ~ mean_depth,
              family=binomial(link='logit'), data=modeldata_boer)
summary(model2_boer)

# plmu -- not enough points
ggplot(modeldata_plmu, aes(x=outcome, y=mean_depth, group=outcome)) +
  geom_boxplot() + geom_point()
ggplot(modeldata_plmu, aes(x=slope_50, y=mean_depth)) +
  geom_point()
model2_plmu = glm(outcome ~ mean_depth,
                  family=binomial(link='logit'), data=modeldata_plmu)
# not enough points
#summary(model2_plmu)

# sporo
ggplot(modeldata_sporo, aes(x=outcome, y=mean_depth, group=outcome)) +
  geom_boxplot() + geom_point()
ggplot(modeldata_sporo, aes(x=slope_50, y=mean_depth)) +
  geom_point()
model2_sporo = glm(outcome ~ mean_depth,
                  family=binomial(link='logit'), data=modeldata_sporo)
summary(model2_sporo)

# scbr -- not enough points
ggplot(modeldata_scbr, aes(x=outcome, y=mean_depth, group=outcome)) +
  geom_boxplot() + geom_point()
ggplot(modeldata_scbr, aes(x=slope_50, y=mean_depth)) +
  geom_point()
model2_scbr = glm(outcome ~ mean_depth,
                  family=binomial(link='logit'), data=modeldata_scbr)
#summary(model2_scbr)

# arist
ggplot(modeldata_arist, aes(x=outcome, y=mean_depth, group=outcome)) +
  geom_boxplot() + geom_point()
ggplot(modeldata_arist, aes(x=slope_50, y=mean_depth)) +
  geom_point()
model2_arist = glm(outcome ~ mean_depth,
                  family=binomial(link='logit'), data=modeldata_arist)
summary(model2_arist)
