# persistence of SPFL2
# EMC 8/11/20

library(dplyr)
library(ggplot2)

grasstotals = read.csv('data/grass_species_totals.csv', stringsAsFactors = F)
dates = read.csv('data/quadrats_dates_for_analysis.csv', stringsAsFactors = F)
quadtype = read.csv('data/quad_type_coordinate.csv', stringsAsFactors = F) %>%
  dplyr::select(quadrat, vegtype, upland_byspecies)

# quadrats to be used in analysis
quads = unique(dates$quadrat)


# just looking at SPFL cover
spfl_data = dplyr::filter(grasstotals, species=='SPFL2')
spfl = spfl_data %>%
  merge(dates, all.y=T) %>%
  merge(quadtype, all=T) %>%
  #filter(quadrat %in% quads) %>%
  #filter(quadrat %in% unique(boer_data$quadrat))
  filter(quadrat %in% quads, upland_byspecies %in% c('upland',''))

# fill in 0s where cover is NA (date implies it was sampled, but no SPFL found)
spfl$totalarea[is.na(spfl$totalarea)] <- 0

# how many quadrats were sampled in each project_year
quads_per_year = spfl %>% group_by(project_year) %>%
  summarize(nquads = n_distinct(quadrat))

# how many quadrats had boer present in each project_year
spfl_quads_per_year = spfl %>% 
  dplyr::filter(totalarea>0) %>%
  group_by(project_year) %>%
  summarize(spflquads = n_distinct(quadrat)) %>%
  merge(quads_per_year) %>%
  mutate(pct_spfl = spflquads/nquads) %>%
  dplyr::filter(nquads>=max(nquads)/2) # only take years where > 50% of quads were sampled
spfl_quads_per_year$project_year = as.numeric(spfl_quads_per_year$project_year)

# plot % of quads where spfl present through time
spfl_presence = ggplot(spfl_quads_per_year, aes(x=project_year, y=pct_spfl)) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('') +
  ylim(0,1) +
  ggtitle('Presence of SPFL2') +
  theme_bw()
spfl_presence
ggsave(filename='spfl/spfl_presence_timeseries.png', plot=spfl_presence, width=5, height=4)

# look at data in wide format, did a quadrat go to zero then recover?
spfl_wide = spfl %>% dplyr::select(quadrat, project_year, totalarea) %>%
  tidyr::pivot_wider(names_from=project_year, values_from=totalarea)
spfl_wide = spfl_wide[,order(names(spfl_wide))]
write.csv(spfl_wide,'spfl/spfl_wide_format.csv', row.names=F)


# average spfl cover per quadrat
spfl_avg_cover = spfl %>%
  dplyr::filter(totalarea>0) %>%
  group_by(project_year) %>%
  summarize(avgcover=mean(totalarea))
spfl_avg_cover$project_year = as.numeric(spfl_avg_cover$project_year)

# plot avg cover over time
spfl_cover = ggplot(spfl_avg_cover, aes(x=project_year, y=avgcover)) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('Area (m^2)') +
  ggtitle('Avg. SPFL2 cover per quadrat') +
  theme_bw()
spfl_cover
ggsave(filename='spfl/spfl_avg_cover_timeseries.png', plot=spfl_cover, width=5, height=4)

