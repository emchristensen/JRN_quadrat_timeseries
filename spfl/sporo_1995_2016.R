#' Looking at SPORO cover 1995-2016
#' EMC 8/26/20

library(dplyr)
library(ggplot2)

grasstotals = read.csv('data/grass_species_totals.csv', stringsAsFactors = F)
dates = read.csv('data/quadrats_dates_for_analysis.csv', stringsAsFactors = F)
quadtype = read.csv('data/quad_type_coordinate.csv', stringsAsFactors = F) %>%
  dplyr::select(quadrat, vegtype, upland_byspecies)

# quadrats to be used in analysis
quads = unique(dates$quadrat)

# just looking at SPORO cover
sporo_data = dplyr::filter(grasstotals, species=='SPORO', year >=1995)
moderndates = dplyr::filter(dates, year>=1995)
sporo = sporo_data %>%
  merge(moderndates, all.y=T) %>%
  filter(quadrat %in% unique(sporo_data$quadrat), 
         quadrat %in% quads,
         quadrat !='A2',
         year >=1995)

# fill in 0s where cover is NA (date implies it was sampled, but no SPORO found)
sporo$totalarea[is.na(sporo$totalarea)] <- 0

# make wide
sporo_wide = tidyr::pivot_wider(sporo[c('quadrat','project_year','totalarea')], 
                               names_from=project_year, values_from=totalarea)

# get mean of all quads
sporo_mean = sporo %>%
  group_by(project_year) %>%
  summarize(mean_area=mean(totalarea))

# figures
sporo_trends = ggplot() +
  geom_line(data=sporo, aes(x=as.numeric(project_year), y=totalarea, color=quadrat), show.legend = F) +
  geom_line(data=sporo_mean, aes(x=as.numeric(project_year), y=mean_area), size=1.5) +
  xlab('') +
  ylab('Cover m^2') +
  ggtitle('Sporobolus sp. cover per quadrat') +
  theme_bw()
sporo_trends
ggsave('Figures/cover_1995_2016_SPORO.png', plot=sporo_trends, width=4, height=3)

presence = sporo %>%
  dplyr::filter(totalarea>0) %>%
  group_by(project_year) %>%
  summarize(nquads=n_distinct(quadrat))
  
sporo_presence = ggplot(presence, aes(x=as.numeric(project_year), y=nquads)) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('# quadrats SCBR present') +
  ylim(0,60) +
  theme_bw()
sporo_presence
ggsave('Figures/presence_1995_2016_SPORO.png', plot=sporo_presence, width=4, height=3)

# # test effect of soil on boer cover during drought
# soil = read.csv('../JRN_quadrat_datapaper/Soil/Jornada_quadrat_soil_PSA.csv', stringsAsFactors = F)
# soil_deep = dplyr::filter(soil, depth_layer=='deep')
# soil_shallow = dplyr::filter(soil, depth_layer=='shallow')
# boer2011 = dplyr::filter(boer, project_year==2011, totalarea>0) %>%
#   merge(soil_deep, all.x=T)
# 
# plot(boer2011$pct_clay, boer2011$totalarea)
        