#' get raw climate data and create monthly timeseries
#' Takes daily headquarters weather station data, aggregate to monthly. 
#'  - If more than 7 precip values in a month are missing, fill with monthly PRISM values
#'  - When aggregating to yearly, if more than 1 month NA discard yearly avg 
#'  - summer = May-Sept
#'  - winter = Oct-April and belongs to the following year (Oct 1923-Apr 1924 is winter 1924)
#'  - VPD is from PRISM, precip and temp are from the weather station
# EMC
# last run: 1/14/21

library(dplyr)
library(lubridate)

datarepo = '../JRN_quadrat_datapaper/'
pptdat = read.csv(paste0(datarepo, 'Climate/JRN_379001_NOAA_JER_HQ_daily_climate_data.csv'), stringsAsFactors = F)
prismdat = read.csv(paste0(datarepo, 'Climate/PRISM_ppt_tmean_vpdmax_stable_4km_191501_201701_Headquarters.csv'), stringsAsFactors = F, skip=10)

# deal with dates 
pptdat$Date = as.Date(pptdat$date)
pptdat$year = lubridate::year(pptdat$Date)
pptdat$month = lubridate::month(pptdat$Date)
prismdat$year = as.numeric(substr(prismdat$Date, 1,4))
prismdat$month = as.numeric(substr(prismdat$Date, 6,7))

# get monthly ppt
monthlyppt = pptdat %>%
  group_by(year, month) %>%
  summarize(monthly_ppt_mm= sum(prec_mm, na.rm=T), monthly_ppt_nas = sum(is.na(prec_mm)),
            monthly_maxt = mean(tmax_c, na.rm=T), monthly_temp_nas = sum(is.na(tmax_c)),
            monthly_mint = mean(tmin_c, na.rm=T), monthly_tmin_nas = sum(is.na(tmin_c)))

monthlyprism = prismdat %>%
  group_by(year, month) %>%
  mutate(monthly_ppt_prism = sum(ppt..inches.)*25.4) %>%
  dplyr::select(year, month, monthly_ppt_prism, monthly_vpd = vpdmax..hPa.)

# if our ppt measurements are missing more than 7 days in a month, fill with prism data
#   this only affects 6 vaues: 5/1915, 12/1977, 1/2009, 9/1979, 7/2002, 8/1948
monthly = merge(monthlyppt, monthlyprism)
monthly$monthly_ppt_filled = monthly$monthly_ppt_mm
monthly$monthly_ppt_filled[monthly$monthly_ppt_nas>7] <- monthly$monthly_ppt_prism[monthly$monthly_ppt_nas>7]

# create water_yr column so winter (Oct-Apr) ppt can be calculated
monthly$water_yr = monthly$year
monthly$water_yr[monthly$month %in% c(10,11,12)] <- monthly$year[monthly$month %in% c(10,11,12)] +1



# aggregate by year and summer
yearlyppt = monthly %>%
  group_by(water_yr) %>%
  summarize(yearly_ppt_mm = sum(monthly_ppt_filled),
            yearly_maxt = mean(monthly_maxt, na.rm = T), yearly_maxt_nas = sum(is.na(monthly_maxt)),
            yearly_mint = mean(monthly_mint, na.rm = T), yearly_mint_nas = sum(is.na(monthly_mint)),
            yearly_vpd = mean(monthly_vpd))
# if there is more than one entire month missing from temp, replace with NA (not a reliable avg)
#   this affects 9 values for max, 11 for min
yearlyppt$yearly_maxt[yearlyppt$yearly_maxt_nas>1] <- NA
yearlyppt$yearly_mint[yearlyppt$yearly_mint_nas>1] <- NA

summerppt = monthly %>%
  dplyr::filter(month %in% c(5,6,7,8,9)) %>%
  group_by(water_yr) %>%
  summarize(summer_ppt_mm = sum(monthly_ppt_filled),
            summer_maxt = mean(monthly_maxt, na.rm=T), summer_maxt_nas = sum(is.na(monthly_maxt)),
            summer_vpd = mean(monthly_vpd))
# if there is more than one entire month missing from summer temp, replace with NA (not a reliable avg)
#   this only affects 5 values: 1918, 1921, 1929, 1932, 1937
summerppt$summer_maxt[summerppt$summer_maxt_nas>1] <- NA

winterppt = monthly %>%
  dplyr::filter(month %in% c(1,2,3,4,10,11,12)) %>%
  group_by(water_yr) %>%
  summarize(winter_ppt_mm = sum(monthly_ppt_filled),
            winter_maxt = mean(monthly_maxt, na.rm=T), winter_maxt_nas = sum(is.na(monthly_maxt)),
            winter_vpd = mean(monthly_vpd))
# if there is more than one entire month missing from winter temp, replace with NA (not a reliable avg)
#   this affects 8 values: 1917, 1921, 1929, 1931, 1932, 1933, 1937, 1948
winterppt$winter_maxt[winterppt$winter_maxt_nas>1] <- NA

# merge into one data frame
climatesummary = merge(yearlyppt, summerppt) %>%
  merge(winterppt) %>%
  dplyr::select(water_yr, yearly_ppt_mm, yearly_maxt, yearly_mint, yearly_vpd, 
                summer_ppt_mm, summer_maxt, summer_vpd,
                winter_ppt_mm, winter_maxt, winter_vpd)

write.csv(climatesummary, 'climate/climate_variables.csv', row.names=F)



# ===================================================================
# get monthly sums of past 6 months and 1 year
monthlyppt$date = as.Date(paste(monthlyppt$year, monthlyppt$month, '15', sep='-'))
monthlyppt = arrange(monthlyppt, date)

ppt_6mo_yr = c()
for (n in 12:nrow(monthlyppt)) {
  selectedrows = monthlyppt[(n-11):n,]
  if ((selectedrows$date[12]- selectedrows$date[1])>367) {
    stop()
  }
  ppt_6mo_yr = rbind(ppt_6mo_yr, data.frame(year=selectedrows$year[12], 
                                            month = selectedrows$month[12], 
                                            ppt_6mo = sum(selectedrows$monthly_ppt_mm[7:12]),
                                            ppt_yr = sum(selectedrows$monthly_ppt_mm)))
}

write.csv(ppt_6mo_yr, 'climate/ppt_6mo_year_sums.csv', row.names=F)
