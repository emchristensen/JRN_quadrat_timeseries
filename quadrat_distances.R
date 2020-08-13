#' script to calculate quadrat distances

library(geosphere)
library(dplyr)

quadgps = read.csv('../JRN_quadrat_datapaper/SiteandMethods/quadrat_lon_lat.csv', 
                   stringsAsFactors = F, header=F, col.names = c('lat','lon','quadrat'))

quaddist = data.frame()
for (r in 1:(nrow(quadgps)-1)) {
  for (s in (r+1):(nrow(quadgps))) {
    quad1 = quadgps$quadrat[r]
    quad2 = quadgps$quadrat[s]
    distrow = data.frame(quad1=quad1, quad2=quad2, 
                         dist=distm(quadgps[c(r,s),c('lon','lat')], fun=distGeo)[1,2])
    quaddist = rbind(quaddist, distrow)
  }
  
}

write.csv(quaddist,'quadrat_distance_m.csv', row.names=F)
