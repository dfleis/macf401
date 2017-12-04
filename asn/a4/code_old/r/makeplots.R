library(plyr)
library(ggplot2)
setwd("~/drive/concordia/2015-2016/2_winter2016/macf401/assignments/a4/")
dat <- read.csv("./data/out/implied_vols_old.csv", stringsAsFactors = F)
conv <- read.csv("./data/out/convergence.csv", stringsAsFactors = F)

Nmax <- length(conv$Nvec)

par(mfrow = c(2,2))
plot(conv$vols~conv$Nvec, type = 'l')
plot(conv$vols[50:Nmax]~conv$Nvec[50:Nmax], type = 'l')
plot(conv$vols[500:Nmax]~conv$Nvec[500:Nmax], type = 'l')
plot(conv$vols[800:Nmax]~conv$Nvec[800:Nmax], type = 'l')

par(mfrow = c(1,1))
plot(diff(conv$vols)~conv$Nvec[-1], type = 'l')
plot(abs(diff(conv$vols))~conv$Nvec[-1], type = 'l')

par(mfrow = c(2,1))
plot(conv$vols~conv$Nvec, type = 'l')
plot(abs(diff(conv$vols))~conv$Nvec[-1], log = 'y', type = 'l')

par(mfrow = c(1,1))
plot(log10(abs(diff(conv$vols)))~conv$Nvec[-1], type = 'l')




dat_list <- split(dat, dat$N)

str(dat_list)

dat_list$`1000`


ggplot(dat, aes(x = strike, y = vol, color = N)) +
  geom_point() +
  facet_grid(type~tau)