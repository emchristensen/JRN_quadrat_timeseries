#' Looking at SCBR cover 1995-2016
#' EMC 8/26/20

library(dplyr)
library(ggplot2)

grasstotals = read.csv('data/grass_species_totals.csv', stringsAsFactors = F)
dates = read.csv('data/quadrats_dates_for_analysis.csv', stringsAsFactors = F)
quadtype = read.csv('data/quad_type_coordinate.csv', stringsAsFactors = F) %>%
  dplyr::select(quadrat, vegtype, upland_byspecies)

# quadrats to be used in analysis
quads = unique(dates$quadrat)

# just looking at SCBR cover
scbr_data = dplyr::filter(grasstotals, species=='SCBR2', year >=1995)
moderndates = dplyr::filter(dates, year>=1995)
scbr = scbr_data %>%
  merge(moderndates, all.y=T) %>%
  filter(quadrat %in% unique(scbr_data$quadrat), 
         quadrat %in% quads,
         year >=1995)

# fill in 0s where cover is NA (date implies it was sampled, but no SCBR found)
scbr$totalarea[is.na(scbr$totalarea)] <- 0

# make wide
scbr_wide = tidyr::pivot_wider(scbr[c('quadrat','project_year','totalarea')], 
                               names_from=project_year, values_from=totalarea)

# get mean of all quads
scbr_mean = scbr %>%
  group_by(project_year) %>%
  summarize(mean_area=mean(totalarea))

# figures
scbr_trends = ggplot() +
  geom_line(data=scbr, aes(x=as.numeric(project_year), y=totalarea, color=quadrat), show.legend = F) +
  geom_line(data=scbr_mean, aes(x=as.numeric(project_year), y=mean_area), size=1.5) +
  xlab('') +
  ylab('Cover m^2') +
  ggtitle('S. brevifolius cover per quadrat') +
  theme_bw()
scbr_trends
ggsave('Figures/cover_1995_2016_SCBR.png', plot=scbr_trends, width=4, height=3)

presence = scbr %>%
  dplyr::filter(totalarea>0) %>%
  group_by(project_year) %>%
  summarize(nquads=n_distinct(quadrat))
  
scbr_presence = ggplot(presence, aes(x=as.numeric(project_year), y=nquads)) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('# quadrats SCBR present') +
  ylim(0,15) +
  theme_bw()
scbr_presence
ggsave('Figures/presence_1995_2016_SCBR.png', plot=scbr_presence, width=4, height=3)

# # test effect of soil on boer cover during drought
# soil = read.csv('../JRN_quadrat_datapaper/Soil/Jornada_quadrat_soil_PSA.csv', stringsAsFactors = F)
# soil_deep = dplyr::filter(soil, depth_layer=='deep')
# soil_shallow = dplyr::filter(soil, depth_layer=='shallow')
# boer2011 = dplyr::filter(boer, project_year==2011, totalarea>0) %>%
#   merge(soil_deep, all.x=T)
# 
# plot(boer2011$pct_clay, boer2011$totalarea)
        