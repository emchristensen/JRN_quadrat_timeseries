#'  Goal is to find degree of correlation between grass and climate (precip/PDO), and when in the
#' timeseries the relationship breaks down
#' EMC 3/23/21

library(dplyr)
library(ggplot2)

# read in veg data
grassts = read.csv('data/grass_shrub_timeseries_imputed.csv')

totalgrass = grassts %>%
  group_by(project_year) %>%
  summarize(grass = mean(total_grass),
            shrub = mean(total_shrub))
  
ggplot(totalgrass, aes(x=project_year, y=grass)) +
  geom_line() +
  geom_point()

# read in climate data
pptts = read.csv('climate/climate_variables.csv')

# get yearly ppt for correct years
ppt = dplyr::filter(pptts, water_yr %in% totalgrass$project_year) %>%
  dplyr::rename(project_year = water_yr)

ggplot(ppt, aes(x=project_year, y=yearly_ppt_mm)) +
  geom_line()


# ======================================================================
# try plain CCF https://nwfsc-timeseries.github.io/atsa-labs/sec-tslab-correlation-within-and-among-time-series.html

# first create timeseries objects
ppt_ts = ts(data=ppt$yearly_ppt_mm, frequency=1, start=1927)
grass_ts = ts(data=totalgrass$grass, frequency=1, start=1927)

ccf(ppt_ts, grass_ts, ylab='Cross-correlation')

# no significance

# try restricting timeseries to 1927-1957
ppt_ts2 = ts(data=ppt$yearly_ppt_mm[1:30], frequency=1, start=1927)
grass_ts2 = ts(data=totalgrass$grass[1:30], frequency=1, start=1927)

ccf(ppt_ts2, grass_ts2, ylab='Cross-correlation')

#cross correlation at 0 and -1 -- meaning grass is correlated to same year and previous year precip