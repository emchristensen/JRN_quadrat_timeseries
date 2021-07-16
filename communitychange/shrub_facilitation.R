#' Look for evidence of shrubs facilitating herbaceous species
#' EMC 7/13/21

library(dplyr)
library(ggplot2)

# colors for figures
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# read in herbaceous species richness in 2016 -- created in species_richness_difference.R
herbaceous = read.csv('communitychange/richness_comparison_1928_2016.csv')

# read in cover by quadrat data
veg = read.csv('data/quadrat_veg.csv')

# merge two data tables
richness_2016 = veg %>%
  dplyr::filter(project_year == 2016) %>%
  merge(herbaceous, by.x=c('quadrat','project_year'), by.y=c('quadrat','project_year.y')) %>%
  dplyr::select(quadrat, project_year, perennial_cover, grass_cover, shrub_cover, bareground, n_forbs, richness_2016, mean, shrubland)


# plot data: richness vs immediate shrub cover
ggplot(richness_2016, aes(x=shrub_cover, y=richness_2016, color=shrubland)) +
  geom_jitter()


# model: richness vs. shrub cover (immediate) and shrubland status (10m radius > 15% shrub)
richness_2016$shrubland = as.factor(richness_2016$shrubland)
model1 = lm(richness_2016 ~ shrub_cover * shrubland, data=richness_2016)
summary(model1)
# not significant
# just immediate or just neighborhood
model1a = lm(richness_2016 ~ shrub_cover, data=richness_2016)
summary(model1a)
model1b = lm(richness_2016 ~ shrubland, data=richness_2016)
summary(model1b)
# shrubland is significant

# try making shrub_cover a factor: shrub/noshrub
richness_2016$localshrub = NA
richness_2016$localshrub[richness_2016$shrub_cover==0] <- 0
richness_2016$localshrub[richness_2016$shrub_cover>.1] <- 1
richness_2016$localshrub = as.factor(richness_2016$localshrub)
model2 = lm(richness_2016 ~ shrubland + localshrub, data=richness_2016)
summary(model2)
# significant for shrub encroachment (neighborhood)
# just immediate shrub as factor
model2a = lm(richness_2016 ~ localshrub, data=richness_2016)
summary(model2a)
# not significant

# try modeling n_forbs
richness_2016_notencroached = dplyr::filter(richness_2016, shrubland=='not_shrub_encroached')
ggplot(richness_2016_notencroached, aes(x=localshrub, y=n_forbs)) +
  geom_boxplot() +
  geom_jitter()

model3 = lm(n_forbs ~ localshrub * shrubland, data=richness_2016)
summary(model3)
# no significance