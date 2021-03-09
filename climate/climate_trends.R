# explore climate trends
# EMC 1/14/21

library(dplyr)
library(ggplot2)

climatesummary = read.csv('climate/climate_variables.csv', stringsAsFactors = F)

# ==========================================
# trend plots
# yearly precip
ppt = ggplot(climatesummary, aes(x=water_yr, y=yearly_ppt_mm)) +
  geom_line() +
  geom_hline(yintercept=mean(climatesummary$yearly_ppt_mm), linetype='dashed') +
  xlab('') +
  ylab('yearly precip (mm)') +
  theme_bw()
ppt
ggsave(plot=ppt, 'Figures/climate/ppt_timeseries.png', width=4, height=3)


# summer precip
sppt = ggplot(climatesummary, aes(x=water_yr, y=summer_ppt_mm)) +
  geom_line() +
  geom_hline(yintercept=mean(climatesummary$summer_ppt_mm), linetype='dashed') +
  xlab('') +
  ylab('summer precip (mm)') +
  ggtitle('Summer precip (May-Sept)') +
  theme_bw()
sppt
ggsave(plot=sppt, 'Figures/climate/ppt_timeseries_summer.png', width=4, height=3)

wppt = ggplot(climatesummary, aes(x=water_yr, y=winter_ppt_mm)) +
  geom_line() +
  geom_hline(yintercept=mean(climatesummary$winter_ppt_mm), linetype='dashed') +
  xlab('') +
  ylab('winter precip (mm)') +
  ggtitle('Winter precip (Oct - April)') +
  theme_bw()
wppt
ggsave(plot=wppt, 'Figures/climate/ppt_timeseries_winter.png', width=4, height=3)



vpd = ggplot(climatesummary, aes(x=water_yr, y=yearly_vpd)) +
  geom_line() +
  geom_hline(yintercept=mean(climatesummary$yearly_vpd), linetype='dashed') +
  xlab('') +
  ylab('max VPD (hPa)') +
  theme_bw()
vpd
ggsave(plot=vpd, 'Figures/climate/vpd_timeseries.png', width=4, height=3)

temp = ggplot(climatesummary, aes(x=water_yr, y=yearly_maxt)) +
  geom_line() +
  geom_hline(yintercept=mean(climatesummary$yearly_maxt, na.rm=T), linetype='dashed') +
  xlab('') +
  ylab('max daily temp (C)') +
  theme_bw()
temp
ggsave(plot=temp, 'Figures/climate/maxtemp_timeseries.png', width=4, height=3)
