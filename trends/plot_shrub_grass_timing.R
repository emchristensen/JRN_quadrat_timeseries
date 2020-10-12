#' plot scatter of timing of shrub encroachment vs grass disappearance
library(ggplot2)

dat = read.csv('trends/time_of_shrub_encroachment.csv', stringsAsFactors = F) %>%
  dplyr::select(-note)
shrubencroached = dat[complete.cases(dat),]

shrubencroached$ymin = NA
shrubencroached$ymax = NA
shrubencroached$xmin = NA
shrubencroached$xmax = NA
for (n in 1:nrow(shrubencroached)) {
  shrubencroached$ymin[n] = as.numeric(strsplit(shrubencroached$grass_extirpation[n], split='_')[[1]][1] )
  shrubencroached$ymax[n] = as.numeric(strsplit(shrubencroached$grass_extirpation[n], split='_')[[1]][2] )
  shrubencroached$xmin[n] = as.numeric(strsplit(shrubencroached$shrub_appearance[n], split='_')[[1]][1] )
  shrubencroached$xmax[n] = as.numeric(strsplit(shrubencroached$shrub_appearance[n], split='_')[[1]][2] )
}


timingplot = ggplot(shrubencroached) +
  geom_rect(aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color='black', alpha=.2) +
  theme_bw() +
  ylim(c(1917,2020)) +
  xlim(c(1917,2020)) +
  geom_abline(slope=1) +
  geom_label(aes(x=xmin, y=ymin, label=quadrat)) +
  ylab('Year grass disappeared') +
  xlab('Year shrubs appeared')
timingplot
ggsave(timingplot, filename='Figures/shrub_encroachment_vs_grassloss.png', width=4, height=4)
