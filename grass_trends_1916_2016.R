# grass/shrub trends 1916-2016
# adapted from SRM poster analysis
# EMC 8/26/20

library(dplyr)
library(ggplot2)

cbPalette <- c("#999999","#D55E00", "#0072B2","#E69F00", "#56B4E9", "#009E73", "#F0E442", "#CC79A7")

veg = read.csv('data/quadrat_veg.csv', stringsAsFactors = F)
dates = read.csv('data/quadrats_dates_for_analysis.csv', stringsAsFactors = F)


# quadrats to use (the ones that are processed as of Aug 2020)
selectedquads = unique(dates$quadrat)

# just using veg from selected quadrats
veg_data = dplyr::filter(veg, quadrat %in% selectedquads)

# how many quads per year
quadsperyear = dates %>%
  group_by(project_year) %>%
  summarize(nquads = n_distinct(quadrat))

# plot shrub and grass over time
ggplot(veg_data, aes(x=project_year, y=total_grass, color=quadrat)) +
  geom_point()

ggplot(veg_data, aes(x=project_year, y=total_shrub, color=quadrat)) +
  geom_point()

# =================
# group years: quadrats were not all sampled every year, but I want to get a reliable time series
groupedyears = data.frame(project_year = 1915:2016,
                          yeargroup = c(rep(1917,5),
                                        rep(1922,5),
                                        rep(1927,5),
                                        rep(1932,5),
                                        rep(1937,5),
                                        rep(1942,5),
                                        rep(1947,5),
                                        rep(1952,5),
                                        rep(1957,5),
                                        rep(1962,5),
                                        rep(1967,5),
                                        rep(1972,5),
                                        rep(1977,5),
                                        rep(1982,5),
                                        rep(1987,5),
                                        rep(1992,5),
                                        rep(1995,5),
                                        rep(2001,5),
                                        rep(2006,5),
                                        rep(2011,5),
                                        rep(2016,2)))

covergrouped = merge(veg_data, groupedyears, by='project_year') %>%
  group_by(quadrat, yeargroup) %>%
  summarize(meangrass=mean(total_grass, na.rm=T),
            meanshrub=mean(total_shrub, na.rm=T))



# now how many quads per year group
quadsperyear2 = covergrouped %>%
  group_by(yeargroup) %>%
  summarize(nquads = n_distinct(quadrat))

# only select the year (groups) that have at least 60 of the 90 quadrats sampled
selectedyears = quadsperyear2$yeargroup[quadsperyear2$nquads>=60]

# plot shrub and grass over time
datatoplot = dplyr::filter(covergrouped, yeargroup %in% selectedyears)
datatoplot$yeargroup = as.factor(datatoplot$yeargroup)


# mean grass and shrub, dual axes
meanbydate = dplyr::filter(covergrouped, yeargroup %in% selectedyears) %>%
  group_by(yeargroup) %>%
  summarize(meangrass=mean(meangrass),
            meanshrub=mean(meanshrub))

grassshrubtrend <- ggplot(meanbydate, aes(x=yeargroup)) +
  geom_line(aes(y=meangrass, colour='Grass')) +
  geom_point(aes(y=meangrass, colour='Grass')) +
  #scale_y_continuous(sec.axis = sec_axis(~.,name='Shrub Cover per Quadrat')) +
  geom_line(aes(y=meanshrub, colour='Shrub')) +
  geom_point(aes(y=meanshrub, colour='Shrub')) +
  labs(x = '',
       y='Cover per Quadrat (m^2)',
       colour='Vegetation Type',
       title='Mean Cover By Vegetation Type') +
  theme_bw() +
  scale_color_manual(values=cbPalette[2:3])
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
#dominatedby$category[dominatedby$grass >= .01 & dominatedby$shrub>=.01 & dominatedby$grassshrubratio < 2 & dominatedby$grassshrubratio > .5] <- 'mixed'
dominatedby$category[is.na(dominatedby$category)] <- 'Mixed'

# save to csv so this info can be used for maps
write.csv(dominatedby, 'Quadrat_dominant_veg_type_1915_2016.csv', row.names=F)

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
  scale_fill_manual(values=cbPalette[c(1,2,5,3)])
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
  #scale_y_continuous(sec.axis = sec_axis(~.,name='Shrub Cover per Quadrat')) +
  geom_line(aes(y=meanshrub, colour='Shrub')) +
  geom_point(aes(y=meanshrub, colour='Shrub')) +
  labs(x = '',
       y='Cover per Quadrat (m^2)',
       colour='Vegetation Type',
       title='Mean Cover By Vegetation Type') +
  theme_bw() +
  scale_color_manual(values=cbPalette[2:3])
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
  scale_fill_manual(values=cbPalette[c(1,2,5,3)])
cover_barplot2
ggsave('Figures/Cover_barplot_40quadrats.png', plot=cover_barplot2, width=5, height=3)

# ========================================================================
# get into detail with grass-dominated quads 1995-2016

# find the quadrats that were dominated by grass at some point >1995
moderngrassquads = dplyr::filter(dominatedby, yeargroup>=1995, category=='Grass') %>% 
  dplyr::select(quadrat) %>% unique() %>% unlist() %>% as.vector()





