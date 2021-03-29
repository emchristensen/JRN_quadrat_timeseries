#'  Goal is to find degree of correlation between grass and climate (precip/PDO), and when in the
#' timeseries the relationship breaks down
#' 
#' Negative June-Nov SOI -> higher Oct-March precip in NM
#' PDO cold phase -> drier than normal
#' 
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

# read in pdo data
# pdo = read.csv('climate/PDO_wide_1900_2020.csv')
# pdo_long = tidyr::pivot_longer(pdo, cols=names(pdo)[-1], names_to='month', values_to='pdo')
# pdo_long$month = readr::parse_number(pdo_long$month)
# write.csv(pdo_long, 'climate/PDO_long_1900_2020.csv', row.names=F)
 pdo_long = read.csv('climate/PDO_long_1900_2020.csv') #%>%
#   group_by(year) %>%
#   summarize(mean_pdo=mean(pdo)) %>%
#   dplyr::filter(year %in% totalgrass$project_year)

# read in soi data
# soi = read.csv('climate/SOI_wide_1866_2020.csv')
# soi_long = tidyr::pivot_longer(soi, cols=names(soi)[-1], names_to='month', values_to='soi')
# soi_long$month = readr::parse_number(soi_long$month)
# write.csv(soi_long, 'climate/SOI_long_1866_2020.csv', row.names=F)
soi_long = read.csv('climate/SOI_long_1866_2020.csv')


# plot SOI and PDO
indices = merge(soi_long, pdo_long) %>%
  mutate(date = as.Date(paste(year, month, '15',sep='-'))) %>%
  arrange(date)

# smoothing
require(smoother)
smoothsoi = data.frame(date=indices$date)
smoothsoi$y= smoother::smth(indices$soi, method='sma', n=12)
smoothpdo = data.frame(date=indices$date)
smoothpdo$y = smoother::smth(indices$pdo, method='sma', n=12)

ggplot(indices, aes(x=date)) +
#  geom_line(aes(y=soi, color='soi')) +
  geom_line(data=smoothsoi, aes(x=date, y=y, color='soi')) +
 # geom_line(aes(y=pdo, color='pdo')) +
  geom_line(data=smoothpdo, aes(x=date, y=y, color='pdo')) +
  #geom_smooth(aes(x=soi, color='soi'), method='loess') +
  geom_hline(yintercept=0) +
  theme_bw()


# ======================================================================
# try plain CCF https://nwfsc-timeseries.github.io/atsa-labs/sec-tslab-correlation-within-and-among-time-series.html

# first create timeseries objects
ppt_ts = ts(data=ppt$yearly_ppt_mm[1:53], frequency=1, start=1927)
grass_ts = ts(data=totalgrass$grass[1:53], frequency=1, start=1927)
pdo_ts = ts(data=pdo_long$mean_pdo[1:53], frequency=1, start=1927)

plot(cbind(pdo_ts, grass_ts))

ccf(ppt_ts, grass_ts, ylab='Cross-correlation')

# no significance

# try restricting timeseries to 1927-1957
ppt_ts2 = ts(data=ppt$yearly_ppt_mm[1:30], frequency=1, start=1927)
grass_ts2 = ts(data=totalgrass$grass[1:30], frequency=1, start=1927)
pdo_ts2 = ts(data=pdo_long$mean_pdo[1:30], frequency=1, start=1927)

ccf(ppt_ts2, grass_ts2, ylab='Cross-correlation')

#cross correlation at 0 and -1 -- meaning grass is correlated to same year and previous year precip