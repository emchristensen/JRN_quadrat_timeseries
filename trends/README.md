# Trend analyses
Testing for significant positive/negative trends in grass cover

### Overall trends
__overall_grass_shrub_trends.R__ 
 * Depends on data_functions.R
 * Creates plot of avg. grass and shrub cover per quadrat 1915-2016
 * Creates plot of # of quadrats by category (grass/shrub/bare) 1915-2016
 * Creates _trends/Quadrat_dominant_veg_type_1915_2016_5yearbinds.csv_
 * Creates individual quadrat cover plots in Figures/coverplots folder
__plot_shrub_grass_timing.R__
 * Depends on time_of_shrub_encroachment.csv
 * Creates a scatterplot of year shrubs appeared vs. year grass disappeared for each quadrat

### Grass loss/gain
__grass_slopes.R__
 * Depends on data_functions.R, grass_species_totals.csv, quadrats_dates_for_analysis.csv
 * Uses sens.slope function from trend package to calculate Theil-Sen slope of total grass cover for each quadrat for time periods: 1945-1956 (during drought), 1955-1980 (recovery from drought), and 1995-2016 (current trend). OLS was used for 1995-2016 because there are only 5 sample times in that period. For 1945-1956, quadrats were removed that were sampled fewer than 5 times during this period including at least once in 1955 or 1956. For 1955-1980, quadrats were removed that were missing samples for 5 or more consecutive years during this period. 
 * Calculates Theil-Sen or OLS slopes for cover of each grass species for above time periods. 
 * Saves results to slopes_50_60_95.csv, slopes_boer_50_60_95.csv, slopes_sporo_50_60_95.csv, slopes_arist_50_60_95, slopes_dapu_50_60_95.csv
 
__logistic_models.R__
 * Depends on slopes data created by grass_slopes.R, and covariates.csv
 * 

### Data files
__Quadrat_dominant_veg_type_1915_2016_5yearbins.csv__ Veg type (grass/shrub/bare) for each quadrat based on avg. shrub and grass cover within 5 year time sections
__time_of_shrub_encroachmenet.csv__ Manually created. For each quadrat, indicates time interval when grass finally disappeared from the quadrat, and when shrubs arrived. NAs possible if either of these events did not occur on a quadrat. 
