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

# quadrats to use (the 91 that are processed as of Aug 2020)
selectedquads = unique(dates$quadrat)

# just using veg from selected quadrats
veg_data = dplyr::filter(veg, quadrat %in% selectedquads)

# years for interpolated timeseries
selectedyears = data.frame(project_year = 1916:1979)

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

# only take quadrats that have 4 or fewer missing in a row (39 of these, out of 91)
ts_quads = na_stretch$quadrat[na_stretch$missing_consecutive<5]

# impute
all_imputed = c()
for (quad in ts_quads) {
  # get time series with NAs
  quadseries = dplyr::filter(veg_data, project_year %in% selectedyears$project_year, quadrat==quad) %>%
    dplyr::select(project_year, total_grass, total_shrub) %>%
    merge(selectedyears, all.y=T)
  
  # impute NAs
  series_imputed = imputeTS::na_interpolation(quadseries, option='linear') %>%
    mutate(quadrat=quad)
  
  # add to final frame
  all_imputed = rbind(all_imputed, series_imputed)
}

# get modern values and add to imputed values
yearly_grass = veg_data %>% dplyr::filter(project_year >=1995, quadrat %in% ts_quads) %>%
  dplyr::select(project_year, total_grass, total_shrub, quadrat) %>%
  rbind(all_imputed) %>%
  arrange(quadrat, project_year)

# save imputed timeseries to file
write.csv(yearly_grass, 'data/grass_shrub_timeseries_imputed.csv', row.names=F)

# get yearly mean for plotting
yearly_mean_grass_shrub = yearly_grass %>%
  group_by(project_year) %>%
  summarize(mean_grass=mean(total_grass),
            mean_shrub=mean(total_shrub))

# Plot: mean grass and mean shrub cover per quadrat per 5-year interval
grassshrubtrend <- ggplot(yearly_mean_grass_shrub, aes(x=project_year)) +
  geom_line(aes(y=mean_grass, colour='Grass')) +
  geom_point(data=yearly_grass, aes(y=total_grass, colour='Grass'), alpha=.02) +
  geom_line(aes(y=mean_shrub, colour='Shrub')) +
  geom_point(data=yearly_grass, aes(y=total_shrub, colour='Shrub'), alpha=.02) +
  labs(x = '',
       y='Cover per Quadrat (m^2)',
       colour='Vegetation Type',
       title='Mean Cover By Vegetation Type') +
  theme_bw() +
  ylim(0,.4) +
  scale_color_manual(values=cbPalette[c(7,6)])
grassshrubtrend
ggsave('Figures/grass_shrub_trend_yearly_39quads.png', plot=grassshrubtrend, width=5, height=3)

# write to file
write.csv(yearly_mean_grass_shrub, 'trends/grass_shrub_trends_yearly.csv', row.names=F)

# ===========================================================================
# plot yearly interpolated cover for each quadrat
yearly_grass = read.csv('data/grass_shrub_timeseries_imputed.csv')

# one quadrat at a time
quad = 'A1'
for (quad in unique(yearly_grass$quadrat)) {
  quadratgrass = dplyr::filter(yearly_grass, quadrat==quad)
  subplot = ggplot(quadratgrass, aes(x=project_year, y=total_grass)) +
    geom_line(aes(color='grass')) +
    geom_line(aes(y=total_shrub, color='shrub')) +
    ylim(0,1) +
    theme_bw() +
    labs(x='',
         y='Cover (m^2)',
         title=quad) +
    scale_color_manual(values=cbPalette[c(7,6)])
  ggsave(paste0('Figures/coverplots_yearly_interpolated/',quad,'.png'), plot=subplot, width=5, height=3)
}

# # ================================================================
# # interpolate grass 1916-1929 (will be fewer quadrats)
# # find longest stretch of NAs for each quadrat
# na_stretch = data.frame()
# # loop through only the quads that are included in the other timeseries (53)
# for (quad in unique(ts_quads)) {
#   # get time series with NAs
#   quadseries = dplyr::filter(veg_data, project_year %in% 1916:1929, quadrat==quad) %>%
#     dplyr::select(project_year, total_grass, total_shrub) %>%
#     merge(data.frame(project_year = 1916:1929), all.y=T)
#   
#   # count max NAs in a row
#   test = is.na(quadseries$total_grass)
#   test[test==F] <- NA
#   if (all(is.na(test))) {
#     missing_consecutive = 0
#   } else {
#     missing_consecutive =na.contiguous(test) %>% length()
#   }
#   
#   # determine if first and last point are NA; add to final frame
#   na_stretch = rbind(na_stretch, data.frame(quadrat=quad, missing_consecutive=missing_consecutive, na_1916=is.na(quadseries$total_grass[1]), 
#                                             na_1929=is.na(quadseries$total_grass[14])))
# }
# 
# # only take quadrats that have 4 or fewer missing in a row, and were in the original 53 (31 of these)
# early_ts_quads = na_stretch$quadrat[na_stretch$missing_consecutive<5 & na_stretch$na_1916==F]
# 
# # impute
# early_imputed = c()
# for (quad in early_ts_quads) {
#   # get time series with NAs
#   quadseries = dplyr::filter(veg_data, project_year %in% 1916:1929, quadrat==quad) %>%
#     dplyr::select(project_year, total_grass, total_shrub) %>%
#     merge(data.frame(project_year=1916:1929), all.y=T)
#   
#   # impute NAs
#   series_imputed = imputeTS::na_interpolation(quadseries, option='linear') %>%
#     mutate(quadrat=quad)
#   
#   # add to final frame
#   early_imputed = rbind(early_imputed, series_imputed)
# }
# 
# # save imputed timeseries to file
# write.csv(early_imputed, 'data/grass_shrub_timeseries_imputed_1916_1929.csv', row.names=F)
