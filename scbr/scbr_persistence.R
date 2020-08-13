# persistence of scbr2
# EMC 8/11/20

library(dplyr)
library(ggplot2)

grasstotals = read.csv('data/grass_species_totals.csv', stringsAsFactors = F)
dates = read.csv('data/quadrats_dates_for_analysis.csv', stringsAsFactors = F)
quadtype = read.csv('data/quad_type_coordinate.csv', stringsAsFactors = F) %>%
  dplyr::select(quadrat, vegtype, upland_byspecies)

# quadrats to be used in analysis
quads = unique(dates$quadrat)


# just looking at scbr cover
scbr_data = dplyr::filter(grasstotals, species=='SCBR2')
scbr = scbr_data %>%
  merge(dates, all.y=T) %>%
  merge(quadtype, all=T) %>%
  #filter(quadrat %in% quads) %>%
  #filter(quadrat %in% unique(boer_data$quadrat))
  filter(quadrat %in% quads, upland_byspecies %in% c('lowland',''))

# fill in 0s where cover is NA (date implies it was sampled, but no scbr found)
scbr$totalarea[is.na(scbr$totalarea)] <- 0

# how many quadrats were sampled in each project_year
quads_per_year = scbr %>% group_by(project_year) %>%
  summarize(nquads = n_distinct(quadrat))

# how many quadrats had boer present in each project_year
scbr_quads_per_year = scbr %>% 
  dplyr::filter(totalarea>0) %>%
  group_by(project_year) %>%
  summarize(scbrquads = n_distinct(quadrat)) %>%
  merge(quads_per_year) %>%
  mutate(pct_scbr = scbrquads/nquads) %>%
  dplyr::filter(nquads>=max(nquads)/2) # only take years where > 50% of quads were sampled
scbr_quads_per_year$project_year = as.numeric(scbr_quads_per_year$project_year)

# plot % of quads where scbr present through time
scbr_presence = ggplot(scbr_quads_per_year, aes(x=project_year, y=pct_scbr)) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('') +
  ylim(0,1) +
  ggtitle('Presence of scbr2') +
  theme_bw()
scbr_presence
ggsave(filename='scbr/scbr_presence_timeseries.png', plot=scbr_presence, width=5, height=4)

# look at data in wide format, did a quadrat go to zero then recover?
scbr_wide = scbr %>% dplyr::select(quadrat, project_year, totalarea) %>%
  tidyr::pivot_wider(names_from=project_year, values_from=totalarea)
scbr_wide = scbr_wide[,order(names(scbr_wide))]
write.csv(scbr_wide,'scbr/scbr_wide_format.csv', row.names=F)


# average scbr cover per quadrat
scbr_avg_cover = scbr %>%
  dplyr::filter(totalarea>0) %>%
  group_by(project_year) %>%
  summarize(avgcover=mean(totalarea))
scbr_avg_cover$project_year = as.numeric(scbr_avg_cover$project_year)

# plot avg cover over time
scbr_cover = ggplot(scbr_avg_cover, aes(x=project_year, y=avgcover)) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('Area (m^2)') +
  ggtitle('Avg. scbr3 cover per quadrat') +
  theme_bw()
scbr_cover
ggsave(filename='scbr/scbr_avg_cover_timeseries.png', plot=scbr_cover, width=5, height=4)

