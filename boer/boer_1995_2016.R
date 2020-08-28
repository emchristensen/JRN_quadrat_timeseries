#' Looking at BOER4 cover 1995-2016
#' EMC 8/26/20

library(dplyr)
library(ggplot2)

grasstotals = read.csv('data/grass_species_totals.csv', stringsAsFactors = F)
dates = read.csv('data/quadrats_dates_for_analysis.csv', stringsAsFactors = F)
quadtype = read.csv('data/quad_type_coordinate.csv', stringsAsFactors = F) %>%
  dplyr::select(quadrat, vegtype, upland_byspecies)

# quadrats to be used in analysis
quads = unique(dates$quadrat)

# just looking at BOER cover, just 1995-2016
boer_data = dplyr::filter(grasstotals, species=='BOER4', year>=1995)
moderndates = dplyr::filter(dates, year>=1995)
boer = boer_data %>%
  merge(moderndates, all.y=T) %>%
  filter(quadrat %in% unique(boer_data$quadrat), 
         quadrat %in% quads,
         year >=1995)

# fill in 0s where cover is NA (date implies it was sampled, but no BOER found)
boer$totalarea[is.na(boer$totalarea)] <- 0

# make wide
boer_wide = tidyr::pivot_wider(boer[c('quadrat','project_year','totalarea')], 
                               names_from=project_year, values_from=totalarea)
# get mean of all quads
boer_mean = boer %>%
  group_by(project_year) %>%
  summarize(mean_area=mean(totalarea))

# figures
boer_trends <- ggplot(data=boer) +
  geom_line(data = boer, aes(x=as.numeric(project_year), y=totalarea, color=quadrat), show.legend = F) +
  geom_line(data=boer_mean, aes(x=project_year, y=mean_area), size=1.5) +
  xlab('') +
  ylab('Cover m^2') +
  ggtitle('B. eriopoda cover per quadrat') +
  theme_bw()
boer_trends
# save figure
ggsave('Figures/cover_1995_2016_BOER.png', plot=boer_trends, width=4, height=3)

# how many quads have BOER
presence = boer %>%
  dplyr::filter(totalarea>0) %>%
  group_by(project_year) %>%
  summarize(nquads=n_distinct(quadrat))

boer_presence <- ggplot(presence, aes(x=project_year, y=nquads)) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('# quadrats BOER present') +
  ylim(0,14) +
  theme_bw()
boer_presence
ggsave('Figures/presence_1995_2016_BOER.png', plot=boer_presence, width=4, height=3)
