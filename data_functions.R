#' functions that are getting used over and over in these analyses
#' EMC 9/4/20

library(dplyr)
library(ggplot2)
library(trend)

cbPalette <- c("#999999", "#E69F00", "#56B4E9","#CC79A7", "#0072B2",  "#D55E00","#009E73", "#F0E442")

#' @description Not all quadrats were sampled every year. Aggregating into 5-year intervals smooths out the missing information 
#' so timeseries can by analyzed
#' @param veg_data data frame with a project_year and quadrat columns
#' @param summary_col_name name of column to be aggregated into 5-year intervals
group_by_5yrs = function(veg_data, summary_col_name) {
  # create summary column -- duplicate of indicated column
  veg_data$summary_col = veg_data[,summary_col_name]
  
  # create data frame of 5-year groups
  groupedyears = data.frame(project_year = 1915:2016,
                            yeargroup = c(rep(1917,5), rep(1922,5),
                                          rep(1927,5), rep(1932,5),
                                          rep(1937,5), rep(1942,5),
                                          rep(1947,5), rep(1952,5),
                                          rep(1957,5), rep(1962,5),
                                          rep(1967,5), rep(1972,5),
                                          rep(1977,5), rep(1982,5),
                                          rep(1987,5), rep(1992,5),
                                          rep(1995,5), rep(2001,5),
                                          rep(2006,5), rep(2011,5),
                                          rep(2016,2)))
  # merge data with grouping data frame
  covergrouped = merge(veg_data, groupedyears, by='project_year') %>%
    group_by(quadrat, yeargroup) %>%
    summarize(mean5year=mean(summary_col))
  
  return(covergrouped)
}

#' @description calculate THeil-Sen slopes
calculate_theil_sen = function(species_ts) {
  # calculate theil-sen slopes
  sp_slopes = c()
  # loop through the quadrats that have BOER
  # the sens.slope function calculates the theil-sen slope, and the p-value is a kendall tau test
  for (quad in unique(species_ts$quadrat)) {
    quaddat = species_ts %>% dplyr::filter(quadrat == quad) %>%
      arrange(year, month)
    sen = sens.slope(quaddat$totalarea)
    #ken = kendallTrendTest(quaddat$totalarea)
    sp_slopes = rbind(sp_slopes, data.frame(quadrat=quad, slope=sen$estimates, pvalue=sen$p.value))
     #                                       slope.k=ken$estimate[2], pvalue.k=ken$p.value))
  }
  # 
  # # get mean of all quads
  # species_mean = species_ts %>%
  #   group_by(project_year) %>%
  #   summarize(mean_area=mean(totalarea))
  # mean_sen = sens.slope(species_mean$mean_area)
  # sp_slopes = rbind(sp_slopes, data.frame(quadrat='Mean', slope=mean_sen$estimates, pvalue=mean_sen$p.value))
  # 
  # create column indicating whether slope is significant at .05 level, and direction of slope if so
  sp_slopes$significant_05 = rep(0)
  sp_slopes$significant_05[sp_slopes$pvalue<=.05 & sp_slopes$slope<0] <- 1
  sp_slopes$significant_05[sp_slopes$pvalue<=.05 & sp_slopes$slope>0] <- 2
  sp_slopes$significant_05 = as.factor(sp_slopes$significant_05)
  
  return(sp_slopes)
}

#' @description creates timeseries of selected grass species
#' @param grassdata data frame containing all grass data
#' @param dates_data data frame containing dates data
#' @param target_sp string: target grass species e.g. BOER4
#' @param min_year numeric: minimum project year of analysis
#' @param max_year numeric: maximum project year of analysis
#' @param aggregate_5_year T/F: whether to aggregate data in 5-year intervals
get_grass_ts = function(grassdata, dates_data, target_sp, min_year, max_year, aggregate_5_year) {
  if (target_sp == 'All') {
    species_data = grassdata %>% 
      dplyr::filter(project_year>=min_year, project_year<=max_year) %>%
      group_by(quadrat, project_year, year, month) %>%
      summarize(totalarea = sum(totalarea))
  } else {
    species_data = dplyr::filter(grassdata, species==target_sp, project_year>=min_year, project_year<=max_year)
  }
  selecteddates = dplyr::filter(dates_data, project_year>=min_year, project_year<=max_year)
  species_ts = species_data %>%
    merge(selecteddates, all=T) %>%
    filter(quadrat %in% unique(species_data$quadrat))
  
  # fill in 0s where cover is NA (date implies it was sampled, but no target grass found)
  species_ts$totalarea[is.na(species_ts$totalarea)] <- 0
  
  # aggregate by 5 years
  if (aggregate_5_year==T) {
    species_ts = group_by_5yrs(species_ts, summary_col_name='totalarea') %>% 
      dplyr::rename(totalarea=mean5year,
                    project_year=yeargroup)
    # retain only quadrats that have data points for all time sections
    timesections = length(unique(species_ts$project_year))
    ntimesperquad = species_ts %>% group_by(quadrat) %>% summarize(ntimes=n_distinct(project_year))
    keepquads = ntimesperquad$quadrat[ntimesperquad$ntimes==timesections]
    species_ts = dplyr::filter(species_ts, quadrat %in% keepquads)
  }
  return(species_ts)
}

#' @description wrapper function that creates timeseries of grass, calculates theil-sen slope, and creates plots
#' @param grassdata data frame containing all grass data
#' @param dates_data data frame containing dates data
#' @param target_sp string: target grass species e.g. BOER4
#' @param min_year numeric: minimum project year of analysis
#' @param max_year numeric: maximum project year of analysis
#' @param save_figures T/F: whether to create and save figures to file
#' @param aggregate_5_year T/F: whether to aggregate data in 5-year intervals
grass_trend_analysis = function(grassdata, dates_data, target_sp, min_year, max_year,
                             save_figures=F, aggregate_5_year=F) {
  # get timeseries of grass
  species_ts = get_grass_ts(grassdata, dates_data, target_sp, min_year, max_year, aggregate_5_year)
  
  # calculate theil-sen slopes
  sp_slopes = calculate_theil_sen(species_ts)
  species_ts_theil = merge(species_ts, sp_slopes, by='quadrat', all.x=T)
  
  # create and save figures
  if (save_figures==T) {
    plot_grass_trends(species_ts_theil, target_sp, min_year, max_year)
  }

  return(species_ts_theil)
}

#' @description creates plots of grass trends and saves to file
#' @param species_ts output of get_grass_ts and calculate_theil_sen
#' @param target_sp character: which grass species
#' @param min_year numeric: minimum project year of analysis
#' @param max_year numeric: maximum project year of analysis
plot_grass_trends = function(species_ts_theil, target_sp, min_year, max_year) {
  # get mean of all quads
  species_mean = species_ts_theil %>%
    group_by(project_year) %>%
    summarize(mean_area=mean(totalarea))
  
  # make significance 1/0 a factor
  species_ts_theil$significant_05 = as.factor(species_ts_theil$significant_05)
  # create trend figure
  species_trends <- ggplot(data=species_ts_theil) +
    geom_line(data = species_ts_theil, 
              aes(x=as.numeric(project_year), y=totalarea, group=quadrat, color=significant_05), 
              show.legend = F, alpha=.6) +
    geom_line(data=species_mean, aes(x=as.numeric(project_year), y=mean_area), size=1.5) +
    xlab('') +
    ylab('Cover m^2') +
    ggtitle(paste0(target_sp,' cover per quadrat')) +
    scale_color_manual(values=cbPalette) +
    theme_bw()
  species_trends
  # save figure
  ggsave(paste0('Figures/cover_',min_year,'_',max_year,'_',target_sp,'.png'), 
         plot=species_trends, width=4, height=3)
  
  # # how many quads have the target species
  # presence = species_ts_theil %>%
  #   dplyr::filter(totalarea>0) %>%
  #   group_by(project_year) %>%
  #   summarize(nquads=n_distinct(quadrat))
  # 
  # species_presence <- ggplot(presence, aes(x=as.numeric(project_year), y=nquads)) +
  #   geom_point() +
  #   geom_line() +
  #   xlab('') +
  #   ylab(paste0('# quadrats ',target_sp,' present')) +
  #   ylim(0,max(presence$nquads+1)) +
  #   theme_bw()
  # species_presence
  # ggsave(paste0('Figures/presence_',min_year,'_',max_year,'_',target_sp,'.png'), 
  #        plot=species_presence, width=4, height=3)
  
}