# get yearly timeseries of avg grass and avg shrub for climate comparisons
# Use interpolation if there is a 1-2 year gap in a quadrat's timeseries
# EMC 3/5/21

library(dplyr)
library(ggplot2)

# colors for figures
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# read in data
veg = read.csv('data/quadrat_veg.csv', stringsAsFactors = F)
dates = read.csv('data/quadrats_dates_for_analysis.csv', stringsAsFactors = F)

# quadrats to use (the ones that are processed as of Aug 2020)
selectedquads = unique(dates$quadrat)

# just using veg from selected quadrats
veg_data = dplyr::filter(veg, quadrat %in% selectedquads)

# years for interpolated timeseries
selectedyears = data.frame(project_year = 1927:1979)

# find longest stretch of NAs for each quadrat
na_stretch = data.frame()
for (quad in unique(veg_data$quadrat)) {
  # get time series with NAs
  quadseries = dplyr::filter(veg_data, project_year %in% selectedyears$project_year, quadrat==quad) %>%
    dplyr::select(project_year, total_grass, total_shrub) %>%
    merge(selectedyears, all.y=T)
  
  # count max NAs in a row
  test = is.na(quadseries$total_grass)
  test[test==F] <- NA
  missing_consecutive =na.contiguous(test) %>% length()
  
  # determine if first and last point are NA; add to final frame
  na_stretch = rbind(na_stretch, data.frame(quadrat=quad, missing_consecutive=missing_consecutive, na_1927=is.na(quadseries$total_grass[1]), 
                                            na_1979=is.na(quadseries$total_grass[53])))
}

# only take quadrats that have 4 or fewer missing in a row (53 of these, out of 91)
ts_quads = na_stretch$quadrat[na_stretch$missing_consecutive<5]

# impute
all_imputed = c()
for (quad in ts_quads) {
  # get time series with NAs
  quadseries = dplyr::filter(veg_data, project_year %in% selectedyears$project_year, quadrat==quad) %>%
    dplyr::select(project_year, total_grass, total_shrub) %>%
    merge(selectedyears, all.y=T)
  
  # impute NAs
  series_imputed = imputeTS::na_interpolation(quadseries, option='linear')
  
  # add to final frame
  all_imputed = rbind(all_imputed, series_imputed)
}

# get yearly mean for plotting
yearly_mean_grass_shrub = all_imputed %>%
  group_by(project_year) %>%
  summarize(mean_grass=mean(total_grass),
            mean_shrub=mean(total_shrub))

# get averages for 1995-2016
yearly_mean_modern = veg_data %>% dplyr::filter(project_year >=1995, quadrat %in% ts_quads) %>%
  group_by(project_year) %>%
  summarize(mean_grass=mean(total_grass),
            mean_shrub=mean(total_shrub))

yearly_mean_grass_shrub = rbind(yearly_mean_grass_shrub, yearly_mean_modern)

# Plot: mean grass and mean shrub cover per quadrat per 5-year interval
grassshrubtrend <- ggplot(yearly_mean_grass_shrub, aes(x=project_year)) +
  geom_line(aes(y=mean_grass, colour='Grass')) +
  geom_point(aes(y=mean_grass, colour='Grass')) +
  geom_line(aes(y=mean_shrub, colour='Shrub')) +
  geom_point(aes(y=mean_shrub, colour='Shrub')) +
  labs(x = '',
       y='Cover per Quadrat (m^2)',
       colour='Vegetation Type',
       title='Mean Cover By Vegetation Type') +
  theme_bw() +
  scale_color_manual(values=cbPalette[c(7,6)])
grassshrubtrend
ggsave('Figures/grass_shrub_trend_yearly_53quads.png', plot=grassshrubtrend, width=5, height=3)

# write to file
write.csv(yearly_mean_grass_shrub, 'trends/grass_shrub_trends_yearly.csv', row.names=F)
