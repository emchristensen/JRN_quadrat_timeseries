# persistence of ARIST
# EMC 8/11/20

library(dplyr)
library(ggplot2)

grasstotals = read.csv('data/grass_species_totals.csv', stringsAsFactors = F)
dates = read.csv('data/quadrats_dates_for_analysis.csv', stringsAsFactors = F)
quadtype = read.csv('data/quad_type_coordinate.csv', stringsAsFactors = F) %>%
  dplyr::select(quadrat, vegtype, upland_byspecies)

# quadrats to be used in analysis
quads = unique(dates$quadrat)

# timing of droughts
drought = data.frame(name = c('1951-1956','2010-20012'),
                     start=c(1950, 2009),
                     end=c(1956, 2012))

# just looking at ARIST cover
arist_data = dplyr::filter(grasstotals, species=='ARIST')
arist = arist_data %>%
  merge(dates, all.y=T) %>%
  merge(quadtype, all=T) %>%
  filter(quadrat %in% quads) %>%
  filter(quadrat %in% unique(arist_data$quadrat))
  #filter(quadrat %in% quads, upland_byspecies %in% c('upland',''))

# fill in 0s where cover is NA (date implies it was sampled, but no ARIST found)
arist$totalarea[is.na(arist$totalarea)] <- 0

# how many quadrats were sampled in each project_year
quads_per_year = arist %>% group_by(project_year) %>%
  summarize(nquads = n_distinct(quadrat))

# how many quadrats had arist present in each project_year
arist_quads_per_year = arist %>% 
  dplyr::filter(totalarea>0) %>%
  group_by(project_year) %>%
  summarize(aristquads = n_distinct(quadrat)) %>%
  merge(quads_per_year) %>%
  mutate(pct_arist = aristquads/nquads) %>%
  dplyr::filter(nquads>=max(nquads)/2) # only take years where > 50% of quads were sampled
arist_quads_per_year$project_year = as.numeric(arist_quads_per_year$project_year)

# plot % of quads where boer present through time
arist_presence = ggplot(arist_quads_per_year, aes(x=project_year, y=pct_arist)) +
  geom_rect(data=drought, aes(NULL,NULL,xmin=start, xmax=end),
            ymin=0, ymax=20, fill='black',alpha=.2) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('') +
  ylim(0,1) +
  ggtitle('Presence of Aristida spp.') +
  theme_bw()
arist_presence
ggsave(filename='arist/arist_presence_timeseries.png', plot=arist_presence, width=5, height=4)

# look at data in wide format, did a quadrat go to zero then recover?
arist_wide = arist %>% dplyr::select(quadrat, project_year, totalarea) %>%
  tidyr::pivot_wider(names_from=project_year, values_from=totalarea)
arist_wide = arist_wide[,order(names(arist_wide))]
write.csv(arist_wide,'arist/arist_wide_format.csv', row.names=F)


# average arist cover per quadrat
arist_avg_cover = arist %>%
  dplyr::filter(totalarea>0) %>%
  group_by(project_year) %>%
  summarize(avgcover=mean(totalarea))
arist_avg_cover$project_year = as.numeric(arist_avg_cover$project_year)

# plot avg cover over time
arist_cover = ggplot(arist_avg_cover, aes(x=project_year, y=avgcover)) +
  geom_rect(data=drought, aes(NULL,NULL,xmin=start, xmax=end),
            ymin=0, ymax=20, fill='black',alpha=.2) +
  geom_point() +
  geom_line() +
  xlab('') +
  ylab('Area (m^2)') +
  ggtitle('Aristida spp. cover per quadrat') +
  theme_bw()
arist_cover
ggsave(filename='arist/arist_avg_cover_timeseries.png', plot=arist_cover, width=5, height=4)

