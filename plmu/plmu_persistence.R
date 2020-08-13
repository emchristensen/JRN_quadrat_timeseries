# persistence of PLMU3
# EMC 8/11/20

library(dplyr)
library(ggplot2)

grasstotals = read.csv('data/grass_species_totals.csv', stringsAsFactors = F)
dates = read.csv('data/quadrats_dates_for_analysis.csv', stringsAsFactors = F)
quadtype = read.csv('data/quad_type_coordinate.csv', stringsAsFactors = F) %>%
  dplyr::select(quadrat, vegtype, upland_byspecies)

# quadrats to be used in analysis
quads = unique(dates$quadrat)


# just looking at PLMU cover
plmu_data = dplyr::filter(grasstotals, species=='PLMU3')
plmu = plmu_data %>%
  merge(dates, all.y=T) %>%
  merge(quadtype, all=T) %>%
  #filter(quadrat %in% quads) %>%
  #filter(quadrat %in% unique(boer_data$quadrat))
  filter(quadrat %in% quads, upland_byspecies %in% c('lowland',''))

# fill in 0s where cover is NA (date implies it was sampled, but no plmu found)
plmu$totalarea[is.na(plmu$totalarea)] <- 0

# how many quadrats were sampled in each project_year
quads_per_year = plmu %>% group_by(project_year) %>%
  summarize(nquads = n_distinct(quadrat))

# how many quadrats had boer present in each project_year
plmu_quads_per_year = plmu %>% 
  dplyr::filter(totalarea>0) %>%
  group_by(project_year) %>%
  summarize(plmuquads = n_distinct(quadrat)) %>%
  merge(quads_per_year) %>%
  mutate(pct_plmu = plmuquads/nquads) %>%
  dplyr::filter(nquads>=max(nquads)/2) # only take years where > 50% of quads were sampled
plmu_quads_per_year$project_year = as.numeric(plmu_quads_per_year$project_year)

# plot % of quads where plmu present through time
plmu_presence = ggplot(plmu_quads_per_year, aes(x=project_year, y=pct_plmu)) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('') +
  ylim(0,1) +
  ggtitle('Presence of plmu2') +
  theme_bw()
plmu_presence
ggsave(filename='plmu/plmu_presence_timeseries.png', plot=plmu_presence, width=5, height=4)

# look at data in wide format, did a quadrat go to zero then recover?
plmu_wide = plmu %>% dplyr::select(quadrat, project_year, totalarea) %>%
  tidyr::pivot_wider(names_from=project_year, values_from=totalarea)
plmu_wide = plmu_wide[,order(names(plmu_wide))]
write.csv(plmu_wide,'plmu/plmu_wide_format.csv', row.names=F)


# average plmu cover per quadrat
plmu_avg_cover = plmu %>%
  dplyr::filter(totalarea>0) %>%
  group_by(project_year) %>%
  summarize(avgcover=mean(totalarea))
plmu_avg_cover$project_year = as.numeric(plmu_avg_cover$project_year)

# plot avg cover over time
plmu_cover = ggplot(plmu_avg_cover, aes(x=project_year, y=avgcover)) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('Area (m^2)') +
  ggtitle('Avg. PLMU3 cover per quadrat') +
  theme_bw()
plmu_cover
ggsave(filename='plmu/plmu_avg_cover_timeseries.png', plot=plmu_cover, width=5, height=4)

