# Categorize charts as: high/moderate/sparse grass and shrub
# similar to what was done in overall_grass_shrub_trends.R
# EMC 12/30/20
# last update: 5/7/21

library(dplyr)
library(ggplot2)

# data and setup ===========================

# colors for figures
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# read data (this file was created by overall_grass_shrub_trends_interpolate.R)
yearly_grass = read.csv('data/grass_shrub_timeseries_imputed.csv')

# categorize charts
quadrat_veg = yearly_grass
quadrat_veg$grass_shrub_ratio = quadrat_veg$total_grass/quadrat_veg$total_shrub
quadrat_veg$category = NA
quadrat_veg$category[quadrat_veg$total_grass>0 & quadrat_veg$grass_shrub_ratio>=2] <- 'grass'
quadrat_veg$category[quadrat_veg$total_shrub>0 & quadrat_veg$grass_shrub_ratio<=0.5] <- 'shrub'
quadrat_veg$category[quadrat_veg$total_shrub==0 & quadrat_veg$total_grass==0] <- 'bare'
quadrat_veg$category[quadrat_veg$grass_shrub_ratio>0.5 & quadrat_veg$grass_shrub_ratio<2] <- 'mixed'

table(quadrat_veg$category)

# save data frame
#write.csv(quadrat_veg2,'data/quadrat_veg_categories_from_interp_ts.csv', row.names=F)

# arrange data for stacked barplot: only display years where at least 45 out of 91 quads were sampled
barplot_data = quadrat_veg %>%
  group_by(project_year, category) %>%
  summarize(nquadrats=length(category)) 

# stacked barplot
cover_barplot <- ggplot(barplot_data, aes(x=project_year, y=nquadrats, fill=category)) +
  geom_bar(stat='identity') +
  labs(x='',
       y="# of Quadrats",
       fill='Cover Type',
       title="Dominant Cover Type of Quadrats") +
  scale_fill_manual(values=cbPalette[c(1,7,5,3)]) +
  theme_bw()
cover_barplot
ggsave(cover_barplot, filename='Figures/Cover_barplot_yearly_39quads.png', width=5, height=3)









# # =================================
# # another way to categorize
# # create categories for quadrats
# quadrat_veg2= yearly_grass
# quadrat_veg2$veg_type = NA
# quadrat_veg2$veg_type[quadrat_veg2$total_shrub>=.1 & quadrat_veg2$total_grass<.1] <- 'high_shrub'
# quadrat_veg2$veg_type[quadrat_veg2$total_grass>=.1 & quadrat_veg2$total_shrub<.1] <- 'high_grass'
# quadrat_veg2$veg_type[quadrat_veg2$total_grass>=.1 & quadrat_veg2$total_shrub>=.1] <- 'high_grass_and_shrub'
# quadrat_veg2$veg_type[quadrat_veg2$total_grass==0 & quadrat_veg2$total_shrub==0] <- 'bare'
# #quadrat_veg2$veg_type[quadrat_veg2$total_grass==0 & quadrat_veg2$total_shrub==0 & quadrat_veg2$n!=0] <- 'bare_with_forbs'
# quadrat_veg2$veg_type[quadrat_veg2$total_grass>=.01 & quadrat_veg2$total_grass<.1 & quadrat_veg2$total_shrub<.01] <- 'moderate_grass'
# quadrat_veg2$veg_type[quadrat_veg2$total_shrub>=.01 & quadrat_veg2$total_shrub<.1 & quadrat_veg2$total_grass<.01] <- 'moderate_shrub'
# quadrat_veg2$veg_type[quadrat_veg2$total_shrub>=.01 & quadrat_veg2$total_shrub<.1 & 
#                         quadrat_veg2$total_grass>=.01 & quadrat_veg2$total_grass<.1] <- 'moderate_grass_and_shrub'
# quadrat_veg2$veg_type[quadrat_veg2$total_grass>0 & quadrat_veg2$total_grass<.01 & quadrat_veg2$total_shrub==0] <- 'sparse_grass'
# quadrat_veg2$veg_type[quadrat_veg2$total_shrub>0 & quadrat_veg2$total_shrub<.01 & quadrat_veg2$total_grass==0] <- 'sparse_shrub'
# quadrat_veg2$veg_type[quadrat_veg2$total_shrub>0 & quadrat_veg2$total_shrub<.01 & 
#                         quadrat_veg2$total_grass>0 & quadrat_veg2$total_grass<.01] <- 'sparse_grass_and_shrub'
# 
# table(quadrat_veg2$veg_type)
# 
# # aggregate into main categories: bare, shrub, grass, mixed
# quadrat_veg2$category = NA
# quadrat_veg2$category[quadrat_veg2$veg_type %in% c('bare','bare_with_forbs')] <- 'Bare'
# quadrat_veg2$category[quadrat_veg2$veg_type %in% c('high_grass','moderate_grass','sparse_grass')] <- 'Grass'
# quadrat_veg2$category[quadrat_veg2$veg_type %in% c('high_shrub','moderate_shrub','sparse_shrub')] <- 'Shrub'
# quadrat_veg2$category[quadrat_veg2$veg_type %in% c('high_grass_and_shrub','moderate_grass_and_shrub','sparse_grass_and_shrub')] <- 'Mixed'
# 
# # aggregate into main cover classes: bare, sparse, moderate, high
# quadrat_veg2$cover = NA
# quadrat_veg2$cover[quadrat_veg2$veg_type %in% c('bare','bare_with_forbs')] <- 'Bare'
# quadrat_veg2$cover[quadrat_veg2$veg_type %in% c('high_grass','high_grass_and_shrub','high_shrub')] <- 'High'
# quadrat_veg2$cover[quadrat_veg2$veg_type %in% c('moderate_grass','moderate_grass_and_shrub','moderate_shrub')] <- 'Moderate'
# quadrat_veg2$cover[quadrat_veg2$veg_type %in% c('sparse_grass','sparse_grass_and_shrub','sparse_shrub')] <- 'Sparse'