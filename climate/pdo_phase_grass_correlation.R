#' does total perennial grass cover differ by PDO phase?
#' EMC 4/12/21

library(dplyr)
library(ggplot2)

phases = read.csv('climate/PDO_phases_byyear.csv')
veg = read.csv('data/grass_shrub_timeseries_imputed.csv')
yearlygrass = read.csv('trends/grass_shrub_trends_yearly.csv')
early_imputed = read.csv('data/grass_shrub_timeseries_imputed_1916_1929.csv')

# yearly average of early timeseries
early_grass = early_imputed %>%
  group_by(project_year) %>%
  summarize(mean_grass = mean(total_grass),
            mean_shrub = mean(total_shrub)) %>%
  dplyr::filter(project_year < 1927)

grass_pdo = merge(rbind(early_grass,yearlygrass), phases, by.x='project_year', by.y='year')

# plot grass time series
ggplot(grass_pdo, aes(x=project_year, y=mean_grass)) +
  geom_line() +
  geom_point(aes(color=pdo_phase)) +
  theme_bw()


# what if we just use the 31 quads that go back to 1916
quads31 = dplyr::filter(veg, quadrat %in% early_imputed$quadrat) %>% 
  rbind(early_imputed) %>%
  group_by(project_year) %>%
  summarize(mean_grass = mean(total_grass),
            mean_shrub = mean(total_shrub),
            nquads = n_distinct(quadrat)) %>%
  merge(phases, by.x='project_year',by.y='year')

pdo_grass_plot = ggplot(quads31, aes(x=project_year, y=mean_grass)) +
  geom_line() +
  geom_point(aes(color=pdo_phase)) +
  xlab('') +
  ylab('Mean grass cover per m^2') +
  ggtitle('Perennial grass cover by PDO phase') +
  theme_bw()
pdo_grass_plot
ggsave('Figures/climate/grass_by_pdo_phase_31quads.png', plot=pdo_grass_plot, width=5, height=3)
