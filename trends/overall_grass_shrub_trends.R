# overall grass/shrub trends 1916-2016
# adapted from SRM poster analysis
# EMC 8/26/20

library(dplyr)
library(ggplot2)

source('data_functions.R')

# colors for figures
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# read in data
veg = read.csv('data/quadrat_veg.csv', stringsAsFactors = F)
dates = read.csv('data/quadrats_dates_for_analysis.csv', stringsAsFactors = F)

# quadrats to use (the ones that are processed as of Aug 2020)
selectedquads = unique(dates$quadrat)

# just using veg from selected quadrats
veg_data = dplyr::filter(veg, quadrat %in% selectedquads)

# # how many quads per year
# quadsperyear = dates %>%
#   group_by(project_year) %>%
#   summarize(nquads = n_distinct(quadrat))

# aggregate grass and shrub to 5-year intervals
grassgrouped = group_by_5yrs(veg_data, 'total_grass') %>% rename(meangrass=mean5year)
shrubgrouped = group_by_5yrs(veg_data, 'total_shrub') %>% rename(meanshrub=mean5year)
covergrouped = merge(grassgrouped, shrubgrouped, all=T)

# now how many quads per year group
quadsperyear = covergrouped %>%
  group_by(yeargroup) %>%
  summarize(nquads = n_distinct(quadrat))

# only select the year (groups) that have at least 60 of the 90 quadrats sampled
selectedyears = quadsperyear$yeargroup[quadsperyear$nquads>=60]

# average cover per quadrat per 5-year interval. number of quadrats per 5-year interval varies. includes blank quadrat samples
meanbydate = dplyr::filter(covergrouped, yeargroup %in% selectedyears) %>%
  group_by(yeargroup) %>%
  summarize(meangrass=mean(meangrass),
            meanshrub=mean(meanshrub))

# Plot: mean grass and mean shrub cover per quadrat per 5-year interval
grassshrubtrend <- ggplot(meanbydate, aes(x=yeargroup)) +
  geom_line(aes(y=meangrass, colour='Grass')) +
  geom_point(aes(y=meangrass, colour='Grass')) +
  geom_line(aes(y=meanshrub, colour='Shrub')) +
  geom_point(aes(y=meanshrub, colour='Shrub')) +
  labs(x = '',
       y='Cover per Quadrat (m^2)',
       colour='Vegetation Type',
       title='Mean Cover By Vegetation Type') +
  theme_bw() +
  scale_color_manual(values=cbPalette[c(7,3)])
grassshrubtrend
  
ggsave('Figures/grass_shrub_trend.png', plot=grassshrubtrend, width=5, height=3)



# =======================================================================
# number of grass-dominated quadrats through time

dominatedby = dplyr::select(covergrouped, quadrat, yeargroup, grass=meangrass,shrub=meanshrub) 
# categorize each chart
dominatedby$grassshrubratio = dominatedby$grass/dominatedby$shrub
dominatedby$category = NA
dominatedby$category[dominatedby$grass < .01 & dominatedby$shrub< .01] <- 'Bare' # <1% shrub and <1% grass
dominatedby$category[dominatedby$grass >=.01 & dominatedby$shrub< .01] <- 'Grass' # <1% shrub and >=1% grass
dominatedby$category[dominatedby$grass < .01 & dominatedby$shrub>=.01] <- 'Shrub' # >=1% shrub and <1% grass
dominatedby$category[dominatedby$grass >= .01 & dominatedby$shrub>=.01 & dominatedby$grassshrubratio >= 2] <- 'Grass' # >=1% grass and shrub but more than double the grass
dominatedby$category[dominatedby$grass >= .01 & dominatedby$shrub>=.01 & dominatedby$grassshrubratio <= .5] <- 'Shrub' # >=1% grass and shrub but more than double the shrub
dominatedby$category[is.na(dominatedby$category)] <- 'Mixed'

dominatedby$category = as.factor(dominatedby$category)

# save to csv so this info can be used for maps
write.csv(dominatedby, 'trends/Quadrat_dominant_veg_type_1915_2016_5yearbins.csv', row.names=F)

# find how many of each category for each year
ncategory = dominatedby %>% group_by(yeargroup, category) %>%
  summarize(ncategory=length(category))

# stacked barplot
cover_barplot <- ggplot(ncategory, aes(x=yeargroup, y=ncategory, fill=category)) +
  geom_bar(stat='identity') +
  theme_bw() +
  labs(x='',
       y="# of Quadrats",
       fill='Cover Type',
       title="Dominant Cover Type of Quadrats") +
  scale_fill_manual(values=cbPalette[c(1,7,5,3)])
cover_barplot
ggsave('Figures/Cover_barplot.png', plot=cover_barplot, width=5, height=3)

# ===========================================
# sensitivity analysis: create above graphs with only quadrats that were sampled regularly through time
# see which quadrats have at least one sample in every 5-year period 
covergroupedtally = covergrouped %>%
  group_by(quadrat) %>%
  summarize(nyeargroup=n_distinct(yeargroup))
bestcoveredquads = covergroupedtally$quadrat[covergroupedtally$nyeargroup==max(covergroupedtally$nyeargroup)]

# mean grass and shrub, dual axes
meanbydate2 = dplyr::filter(covergrouped, quadrat %in% bestcoveredquads) %>%
  group_by(yeargroup) %>%
  summarize(meangrass=mean(meangrass),
            meanshrub=mean(meanshrub))

grassshrubtrend2 <- ggplot(meanbydate2, aes(x=yeargroup)) +
  geom_line(aes(y=meangrass, colour='Grass')) +
  geom_point(aes(y=meangrass, colour='Grass')) +
  geom_line(aes(y=meanshrub, colour='Shrub')) +
  geom_point(aes(y=meanshrub, colour='Shrub')) +
  labs(x = '',
       y='Cover per Quadrat (m^2)',
       colour='Vegetation Type',
       title='Mean Cover By Vegetation Type') +
  theme_bw() +
  scale_color_manual(values=cbPalette[c(7,3)])
grassshrubtrend2
ggsave('Figures/grass_shrub_trend_40quadrats.png', plot=grassshrubtrend2, width=5, height=3)

# what category do these 40 quadrats fall in over time
# find how many of each category for each year
ncategory2 = dominatedby %>% 
  dplyr::filter(quadrat %in% bestcoveredquads) %>%
  group_by(yeargroup, category) %>%
  summarize(ncategory=length(category))

# stacked barplot
cover_barplot2 <- ggplot(ncategory2, aes(x=yeargroup, y=ncategory, fill=category)) +
  geom_bar(stat='identity') +
  theme_bw() +
  labs(x='',
       y="# of Quadrats",
       fill='Cover Type',
       title="Dominant Cover Type of Quadrats") +
  scale_fill_manual(values=cbPalette[c(1,7,5,3)])
cover_barplot2
ggsave('Figures/Cover_barplot_40quadrats.png', plot=cover_barplot2, width=5, height=3)

# ========================================================================
# categorize each year of each quadrat (there are a lot of outliers. hard to understand this)
# selected_veg = merge(veg, dates)
# 
# dominantcover = dplyr::select(selected_veg, quadrat, project_year, grass=total_grass,shrub=total_shrub) 
# # categorize each chart
# dominantcover$grassshrubratio = dominantcover$grass/dominantcover$shrub
# dominantcover$category = NA
# dominantcover$category[dominantcover$grass < .01 & dominantcover$shrub< .01] <- 'Bare' # <1% shrub and <1% grass
# dominantcover$category[dominantcover$grass >=.01 & dominantcover$shrub< .01] <- 'Grass' # <1% shrub and >=1% grass
# dominantcover$category[dominantcover$grass < .01 & dominantcover$shrub>=.01] <- 'Shrub' # >=1% shrub and <1% grass
# dominantcover$category[dominantcover$grass >= .01 & dominantcover$shrub>=.01 & dominantcover$grassshrubratio >= 2] <- 'Grass' # >=1% grass and shrub but more than double the grass
# dominantcover$category[dominantcover$grass >= .01 & dominantcover$shrub>=.01 & dominantcover$grassshrubratio <= .5] <- 'Shrub' # >=1% grass and shrub but more than double the shrub
# #dominatedby$category[dominatedby$grass >= .01 & dominatedby$shrub>=.01 & dominatedby$grassshrubratio < 2 & dominatedby$grassshrubratio > .5] <- 'mixed'
# dominantcover$category[is.na(dominantcover$category)] <- 'Mixed'
# 
# dominantcover$category = as.factor(dominantcover$category)
# 
# write.csv(dominantcover, 'trends/Quadrat_dominant_veg_type_yearly.csv', row.names=F)


# ====================================================================
# loop: plot each quadrat's grass/shrub/category through time (5-year bins)
selected_veg = merge(veg, dates)
for (quad in unique(selected_veg$quadrat)) {
  qdat = dplyr::filter(selected_veg, quadrat==quad)

  colorbars2 = dplyr::filter(dominatedby, quadrat==quad) %>%
    mutate(xmin=yeargroup-2,
           xmax=yeargroup+2,
           ymin=rep(0),
           ymax=rep(1))
  coverplot = ggplot(qdat) +
    geom_rect(data=colorbars2, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax, fill=category), alpha=.3) +
    geom_line(aes(x=project_year, y=total_grass, colour='Grass'), size=1.5) +
    geom_line(aes(x=project_year, y=total_shrub, colour='Shrub'), size=1.5) +
    labs(x = '',
         y='Grass/Shrub Cover',
         colour='Vegetation Type',
         title=quad) +
    theme_bw() +
    scale_color_manual(values=cbPalette[c(7,3)]) +
    scale_fill_manual(breaks=c('Bare','Grass','Mixed','Shrub'), values=cbPalette[c(1,7,5,3)])
  coverplot
  ggsave(plot=coverplot, filename = paste0('Figures/coverplots/',quad,'.png'), width=5, height=3)
}

