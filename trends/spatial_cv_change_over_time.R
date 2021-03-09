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

# does spatial CV increase through time, indicative of shrub encroachment transition?