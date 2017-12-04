library(ggplot2)
setwd("~/drive/concordia/2015-2016/2_winter2016/macf401/assignments/a4/macf_a4_final_code/")
dat <- read.csv("implied_vols_googl.csv", stringsAsFactors = F)
conv <- read.csv("binomial_conv.csv", stringsAsFactors = F)

# clean data a bit...
dat$id <- rep(1:26, times = 5)
dates <- c("2015/06/01", "2015/06/02", "2015/06/03")
dat$days <- round(dat$tau * 365)
dat$dates <- dates[1]
dat$dates[dat$days == unique(dat$days)[2]] <- dates[2]
dat$dates[dat$days == unique(dat$days)[3]] <- dates[3]
dat$spotlab <- paste0("Spot = ", dat$spots)

pdf("../plots/smile_all.pdf", height = 3.5, width = 7)
ggplot(dat[dat$N == max(dat$N),], aes(x = strike, y = vol, shape = type)) +
  labs(shape = "Option\nType", x = "Strike", y = "Implied Volatility", title = "Implied Volatility Smiles for GOOGL\n(All Options)") +
  geom_point(size = 3) + 
  scale_shape(solid = FALSE) + 
  facet_wrap(dates ~ spotlab) +
  theme_bw() + 
  theme(
    strip.background = element_blank())
dev.off()

datOTM <- dat[ ((dat$spots < dat$strike) & (dat$type == 'C')) | ((dat$spots > dat$strike) & (dat$type == 'P')), ]
pdf("../plots/smile_OTM.pdf", height = 3.5, width = 7)
ggplot(datOTM[datOTM$N == max(datOTM$N),], aes(x = strike, y = vol, shape = type)) +
  labs(shape = "Option\nType", x = "Strike", y = "Implied Volatility", title = "Implied Volatility Smiles for GOOGL\n(OTM Options)") +
  geom_point(size = 3) + 
  scale_shape(solid = FALSE) + 
  facet_wrap(dates ~ spotlab) +
  theme_bw() + 
  theme(
    strip.background = element_blank())
dev.off()

pdf("../plots/imp_vol_conv_N.pdf", height = 6, width = 6)
par(mfrow = c(2,2))
plot(dat$vol[dat$id == 1]~dat$N[dat$id == 1], log = 'x', type = 'b',
     ylab = "Implied Volatility", xlab = "N", main = "Call, V = 8.20\nS = 549.21, K = 550, T = 18 days")
plot(dat$vol[dat$id == 6]~dat$N[dat$id == 6], log = 'x', type = 'b',
     ylab = "Implied Volatility", xlab = "N",  main = "Put, V = 0.40\nS = 549.21, K = 500, T = 18 days")
plot(dat$vol[dat$id == 12]~dat$N[dat$id == 12], log = 'x', type = 'b',
     ylab = "Implied Volatility", xlab = "N", main = "Call, V = 3.80\nS = 553.95, K = 565, T = 17 days")
plot(dat$vol[dat$id == 18]~dat$N[dat$id == 18], log = 'x', type = 'b',
     ylab = "Implied Volatility", xlab = "N", main = "Put, V = 2.25\nS = 553.95, K = 535, T = 17 days")
dev.off()

# convergence of the binomial model pricing assuming a "correct"
# value for sigma = 0.17507 for the first option
pdf("../plots/price_conv.pdf", height = 6, width = 6)
par(mfrow = c(2,2))
plot(conv$price~conv$N, type = 'l',
     ylab = "V0", xlab = "N")
plot(conv$price[50:5000]~conv$N[50:5000], type = 'l', xaxt = 'n',
     ylab = "V0", xlab = "N")
  ticks = c(50, 1500, 3000, 4500)
  axis(side = 1, at = ticks)
plot(conv$price[400:1400]~conv$N[400:1400], type = 'l',
     ylab = "V0", xlab = "N")
plot(conv$price[850:950]~conv$N[850:950], type = 'l',
     ylab = "V0", xlab = "N")
mtext("Time Zero Price of a Call Option on GOOGL\nsigma = 0.17507, S = 549.21, K = 550, T = 18 days", outer = T, line = -3)
dev.off()

pdf("../plots/price_conv_diff.pdf", height = 4, width = 6)
par(mfrow = c(1,1))
plot(abs(diff(conv$price))~conv$N[-1], log = 'y', type = 'l',
     ylab = "Absolute Difference", xlab = "N", main = "Absolute Difference in Option Price\nfor Increasing N Steps")
dev.off()

# investigate convergence of the binomial model implied vol
# using the first option in the data set 
pdf("../plots/imp_vol_conv.pdf", height = 6, width = 6)
par(mfrow = c(2,2))
plot(conv$vol~conv$N, type = 'l',
     ylab = "Implied Volatility", xlab = "N")
plot(conv$vol[50:5000]~conv$N[50:5000], type = 'l', xaxt = 'n',
     ylab = "Implied Volatility", xlab = "N")
  ticks = c(50, 1500, 3000, 4500)
  axis(side = 1, at = ticks)
plot(conv$vol[400:1400]~conv$N[400:1400], type = 'l',
     ylab = "Implied Volatility", xlab = "N")
plot(conv$vol[850:950]~conv$N[850:950], type = 'l',
     ylab = "Implied Volatility", xlab = "N")
mtext("Implied Volatility of a Call Option on GOOGL\nV = 8.20, S = 549.21, K = 550, T = 18 days", outer = T, line = -3)
dev.off()

pdf("../plots/imp_vol_conv_diff.pdf", height = 4, width = 6)
par(mfrow = c(1,1))
plot(abs(diff(conv$vol))~conv$N[-1], log = 'y', type = 'l',
     ylab = "Absolute Difference", xlab = "N", main = "Absolute Difference of Implied Volatility\nfor Increasing N Steps")
dev.off()









