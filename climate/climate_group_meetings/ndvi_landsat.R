
library(dplyr)
library(ggplot2)


#' @description NDVI data from google earth engine: get date information from system.index and create date column
#' 
#' @param ndviframe data frame of raw ndvi data from google earth engine
get_date_from_index = function(ndviframe) {
  ndviframe$year = substr(ndviframe$system.index,13,16)
  ndviframe$month = substr(ndviframe$system.index,17,18)
  ndviframe$day = substr(ndviframe$system.index,19,20)
  ndviframe$date = as.Date(paste(ndviframe$year, ndviframe$month, ndviframe$day, sep='-'))
  return(ndviframe)
}

# read in files downloaded from gee
ndvi5_raw = read.csv('climate/climate_group_meetings/Landsat5_SR_NDVI_jornada_1984_2011.csv')
ndvi7_raw = read.csv('climate/climate_group_meetings/Landsat7_SR_NDVI_jornada_1999_2020.csv')

# threshhold for number of good pixels
minpixels5 = 0.75*max(ndvi5_raw$NDVI_count)
minpixels7 = 0.75*max(ndvi7_raw$NDVI_count)

# process raw data
ndvi5 = ndvi5_raw %>% 
  get_date_from_index() %>%
  dplyr::filter(NDVI_count>minpixels5)

ndvi7 = ndvi7_raw %>%
  get_date_from_index() %>%
  dplyr::filter(NDVI_count>minpixels7) %>%
  group_by(month, year) %>%
  summarize(NDVI_mean=mean(NDVI_mean))

ggplot(ndvi5, aes(x=date, y=NDVI_mean, color='landsat5')) +
  geom_point() +
  geom_line() +
  geom_point(data=ndvi7, aes(x=date, y=NDVI_mean, color='landsat7')) +
  geom_line(data=ndvi7, aes(x=date, y=NDVI_mean, color='landsat7'))
