library(ggplot2)
setwd("~/drive/concordia/2015-2016/2_winter2016/macf401/assignments/a5/")
datN100 <- read.csv("./data/american_prices_googl_N100.csv", stringsAsFactors = F)
datN1000 <- read.csv("./data/american_prices_googl_N1000.csv", stringsAsFactors = F)
datN10000 <- read.csv("./data/american_prices_googl_N10000.csv", stringsAsFactors = F)
datN100000 <- read.csv("./data/american_prices_googl_N100000.csv", stringsAsFactors = F)

dat_list <- list(datN100, datN1000, datN10000, datN100000)
my_plots <- list()

for (i in 1:length(dat_list)) {
  dat <- dat_list[[i]]
  N <- dat$N[1]
  plot_title <- paste("American Option Prices: N = ", N, "\nBinomial Model Price vs. Observed Price", sep = "")
  dat$Type <- "Call"
  dat$Type[dat$type == "P"] <- "Put"
  
  plot_filename <- paste("./plots/am_price_diff_N", N, ".pdf", sep = "")
  my_plots[[i]] <- ggplot(dat, aes(x = seq(1:length(dat$price)), y = diff, shape = Type)) + 
    labs(title = plot_title) +
    ylab("Binomial - Observed Price Difference") +
    geom_point(size = 3) +
    scale_shape_manual(values = c(1,2)) +  
    geom_hline(yintercept = 0) +
    facet_grid(. ~ Type, scales = "free") + 
    theme_bw() + 
    theme(axis.title.x = element_blank(),
          axis.ticks.x = element_blank(),
          axis.text.x = element_blank(),
          panel.grid.major.x = element_blank(),
          panel.grid.minor.x = element_blank(),
          strip.background = element_blank(),
          legend.key = element_blank())
  
  pdf(plot_filename, height = 3.5, width = 6.5)
  print(my_plots[[i]])
  dev.off()
}



dat <- rbind(datN100, datN1000, datN10000, datN100000)
dat$Type <- "Call"
dat$Type[dat$type == "P"] <- "Put"
dat$idx <- rep(seq(1, length(datN100$price)), length(dat_list))

pdf("./plots/am_price_diff_all_big.pdf", height = 8, width = 7.5)
ggplot(dat, aes(x = idx, y = diff, shape = Type)) + 
  labs(title = "American Option Prices\nBinomial Model Price vs. Observed Price") +
  ylab("Binomial - Observed Price Difference") +
  geom_point(size = 3) +
  scale_shape_manual(values = c(1,2)) +  
  geom_hline(yintercept = 0) +
  facet_grid(N ~ Type) + 
  theme_bw() + 
  theme(axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        strip.background = element_blank(),
        legend.key = element_blank())
dev.off()



