# starting to look at variability in grass cover and community composition
library(dplyr)
library(ggplot2)
library(vegan)

shrub_grass = read.csv('data/quadrat_veg.csv')
species_veg = read.csv('data/all_species_counts_cover.csv')

test = dplyr::filter(shrub_grass, quadrat=='A1', year<1946)

mean(vegdist(test[,c('total_shrub','total_grass')],method='bray'))

ggplot(test, aes(x=project_year, y=total_grass)) +
  geom_point() +
  ylim(0,1)

var(test$total_grass)
sd(test$total_grass)
mean(test$total_grass)

# consecutive disparity index? Fernandez-Martinez et al 2018
# D works well with non-Gaussian data; takes into account order of observations-- assesses the average rate of change beween consecutive values
# PV works well with non-Gaussian data, but also does not take order into account (use for spatial variation)

# =============================
# does spatial CV (or PV) increase through time, indicative of shrub encroachment transition?

# use the interpolated yearly time series I put together for Robb
grassts = read.csv('data/grass_shrub_timeseries_imputed.csv', stringsAsFactors = F)


# loop through each year
cv_ts = c()
for (yr in unique(grassts$project_year)) {
  yrts = dplyr::filter(grassts, project_year==yr)
  cv_ts = rbind(cv_ts, data.frame(project_year=yr, cv=EnvStats::cv(yrts$total_grass)))
}

# plot
grasscv = ggplot(cv_ts, aes(x=project_year, y=cv)) +
  geom_point() +
  geom_line() +
  ggtitle('Spatial variation of quadrat grass cover') +
  xlab('') +
  ylab('coefficient of variation') +
  theme_bw()
grasscv

ggsave('Figures/grass_cover_spatial_cv.png', plot=grasscv, width=5, height=3)


# what about PV?
# There's a github repo with functions for it https://rdrr.io/github/T-Engel/CValternatives/
PV <- function (Z){
  n = length(Z)
  pairs = combn(Z,2)
  min_z = apply(pairs,2, min)
  max_z = apply(pairs,2, max)
  z = 1- (min_z/max_z)
  PV=2*sum(z)/(n*(n-1))
  return(PV)
}

PV = function(Z) {
  n = length(Z)
  pairs = combn(Z,2)
  z = c()
  for (x in 1:ncol(pairs)) {
    if (pairs[1,x]==pairs[2,x]) {
      d = 0
    } else {
      d = 1-(min(pairs[,x])/max(pairs[,x]))
    }
    z = rbind(z,d)
  }
  PV = 2*sum(z)/(n*(n-1))
  return(PV)
}

# loop through each year
pv_ts = c()
for (yr in unique(grassts$project_year)) {
  yrts = dplyr::filter(grassts, project_year==yr)
  pv_ts = rbind(pv_ts, data.frame(project_year=yr, pv=PV(yrts$total_grass)))
}

# plot
grasspv = ggplot(pv_ts, aes(x=project_year, y=pv)) +
  geom_point() +
  geom_line() +
  ggtitle('Spatial variation of quadrat grass cover') +
  xlab('') +
  ylab('coefficient of variation') +
  theme_bw()
grasspv
