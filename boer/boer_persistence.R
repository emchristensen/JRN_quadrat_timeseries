# persistence of BOER4
# EMC 8/11/20

library(dplyr)
library(ggplot2)

grasstotals = read.csv('data/grass_species_totals.csv', stringsAsFactors = F)
dates = read.csv('data/quadrats_dates_for_analysis.csv', stringsAsFactors = F)
quadtype = read.csv('data/quad_type_coordinate.csv', stringsAsFactors = F) %>%
  dplyr::select(quadrat, vegtype, upland_byspecies)

# quadrats to be used in analysis
quads = unique(dates$quadrat)


# just looking at BOER cover
boer_data = dplyr::filter(grasstotals, species=='BOER4')
boer = boer_data %>%
  merge(dates, all.y=T) %>%
  merge(quadtype, all=T) %>%
  #filter(quadrat %in% quads) %>%
  #filter(quadrat %in% unique(boer_data$quadrat))
  filter(quadrat %in% quads, upland_byspecies %in% c('upland',''))

# fill in 0s where cover is NA (date implies it was sampled, but no BOER found)
boer$totalarea[is.na(boer$totalarea)] <- 0

# how many quadrats were sampled in each project_year
quads_per_year = boer %>% group_by(project_year) %>%
  summarize(nquads = n_distinct(quadrat))

# how many quadrats had boer present in each project_year
boer_quads_per_year = boer %>% 
  dplyr::filter(totalarea>0) %>%
  group_by(project_year) %>%
  summarize(boerquads = n_distinct(quadrat)) %>%
  merge(quads_per_year) %>%
  mutate(pct_boer = boerquads/nquads) %>%
  dplyr::filter(nquads>=max(nquads)/2) # only take years where > 50% of quads were sampled
boer_quads_per_year$project_year = as.numeric(boer_quads_per_year$project_year)

# plot % of quads where boer present through time
boer_presence = ggplot(boer_quads_per_year, aes(x=project_year, y=pct_boer)) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('') +
  ylim(0,1) +
  ggtitle('Presence of BOER4') +
  theme_bw()
boer_presence
ggsave(filename='boer/boer_presence_timeseries.png', plot=boer_presence, width=5, height=4)

# look at data in wide format, did a quadrat go to zero then recover?
boer_wide = boer %>% dplyr::select(quadrat, project_year, totalarea) %>%
  tidyr::pivot_wider(names_from=project_year, values_from=totalarea)
boer_wide = boer_wide[,order(names(boer_wide))]
write.csv(boer_wide,'boer/boer_wide_format.csv', row.names=F)


# average boer cover per quadrat
boer_avg_cover = boer %>%
  dplyr::filter(totalarea>0) %>%
  group_by(project_year) %>%
  summarize(avgcover=mean(totalarea))
boer_avg_cover$project_year = as.numeric(boer_avg_cover$project_year)

# plot avg cover over time
boer_cover = ggplot(boer_avg_cover, aes(x=project_year, y=avgcover)) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('Area (m^2)') +
  ggtitle('Avg. BOER4 cover per quadrat') +
  theme_bw()
boer_cover
ggsave(filename='boer/boer_avg_cover_timeseries.png', plot=boer_cover, width=5, height=4)

# ====================
# what are the patterns of covariates for quads that recovered/did not recover boer?
boer_categories = read.csv('boer/boer_time_perioods_presenceabsence.csv', stringsAsFactors = F)
shrub = read.csv('data/quadrat_veg.csv', stringsAsFactors = F)
soil = read.csv('../JRN_quadrat_datapaper/Soil/Jornada_quadrat_soil_PSA.csv', stringsAsFactors = F)


# get avg shrub cover per quadrat post-1960
shrub1960 = dplyr::filter(shrub, project_year>=1960) %>%
  group_by(quadrat) %>%
  summarize(avg_shrub=mean(total_shrub))
# separate shallow/deep soil samples
soilshallow = dplyr::filter(soil, depth_layer=='shallow') %>%
  dplyr::select(quadrat, shallow_sand = pct_sand, shallow_silt=pct_silt, shallow_clay=pct_clay)
soildeep = dplyr::filter(soil, depth_layer=='deep') %>%
  dplyr::select(quadrat, deep_sand=pct_sand, deep_silt=pct_silt, deep_clay=pct_clay)


# connect data
boer_recovery = merge(boer_categories, shrub1960, by='quadrat', all.x=T) %>%
  dplyr::filter(boer5years==1) %>%
                      merge(soilshallow, all.x=T) %>%
  merge(soildeep, all.x=T) #%>%
  #dplyr::filter(lostbefore==0)

# create category variable
boer_recovery$category = rep(NA)
boer_recovery$category[boer_recovery$boerlost1950s==1 & boer_recovery$boerpresent2==0] <- 'norecovery'
boer_recovery$category[boer_recovery$boerlost1950s==1 & boer_recovery$boerpresent2==1] <- 'recolonized'


# plot category vs. shrub cover
ggplot(boer_recovery, aes(x=category, y=avg_shrub)) +
  geom_jitter(width=.1, height=.01) +
  xlab('') +
  ylab('Shrub cover (m^2)')

# plot category vs. sand
ggplot(boer_recovery, aes(x=category, y=shallow_sand)) +
  geom_jitter(width=.1, height=.01) +
  xlab('') +
  ylab('Sand %')

# =======================================
library(rpart)
# run CART model
modeldat = dplyr::select(boer_recovery, category, avg_shrub, shallow_sand, shallow_clay, deep_sand, deep_clay)
# plot covariates to look for correlation
plot(modeldat[,2:6])
# shallow sand and deep sand are correlated; also maybe sands and clays

# model
boermodel = rpart(category~., data=modeldat, method='class')
plot(boermodel)
# did not work -- no branching in model