setwd("~/drive/concordia/2015-2016/2_winter2016/macf401/assignments/a4/")
dat <- read.csv("./macf_a4_final_code/google-opt2_old.csv", stringsAsFactors = F)
dat$Date <- as.Date(as.character(dat$Date), format = "%Y%m%d")
dat$Expiration <- as.Date(as.character(dat$Expiration), format = "%Y%m%d")
dat$maturity <- as.numeric(difftime(dat$Expiration, dat$Date)/365)

# assume GOOGL stock (i.e. A class shares)
# closing prices on the given dates
S0_01 <- 549.21
S0_02 <- 553.95
S0_03 <- 555.29

R <- 0.005

p <- 0.5 # global variable
q <- 0.5 # global variable

u_fxn <- function(R, sigma, dt) {
  return ( exp((R - 0.5 * sigma^2) * dt + sigma * sqrt(dt)) )
}
d_fxn <- function(R, sigma, dt) {
  return ( exp((R - 0.5 * sigma^2) * dt - sigma * sqrt(dt)) )
}
asset <- function(S0, n, h, R, sigma, dt) {
  u <- u_fxn(R, sigma, dt)
  d <- d_fxn(R, sigma, dt)
  S0 * u^h * d^(n - h)
}
payoff <- function(S, K, type) {
  if (!(type %in% c('c','p'))) {
    warning("Warning in price_option: Invalid type.")
    return (0)
  } else if (type == 'c') {
    return (max(S - K, 0))
  } else
    return (max(K - S, 0))
}
price_option_r <- function(S0, K, n, h, N, R, sigma, dt, type) {
  if (h > n) {
    warning("Warning in price_option: h > n.")
  }
  if (!(type %in% c('c','p'))) {
    warning("Error in price_option: Invalid type.")
    return (0)
  }
  if (n == N) {
    S <- asset(S0, n, h, R, sigma, dt)
    return (payoff(S, K, type))
  } else {
    Vd <- price_option(S0, K, n + 1, h, N, R, sigma, dt, type)
    Vu <- price_option(S0, K, n + 1, h + 1, N, R, sigma, dt, type)
    return (1/(1 + R)^dt * (p * Vu + q * Vd))
  }
}

price_option_i <- function(S0, K, n, h, N, R, sigma, dt, type) {
  if (h > n) {
    warning("Warning in price_option: h > n.")
  }
  if (!(type %in% c('c','p'))) {
    warning("Error in price_option: Invalid type.")
    return (0)
  }
  prices <- double(N + 1)
  for (l in 0:N) { # final step will have N + 1 nodes
    # final step at node (N, i), Nth step, l heads
    S <- asset(S0, N, l, R, sigma, dt) 
    prices[l + 1] <- payoff(S, K, type)
  }
  for (k in (N - 1):n) { # move backwards through the tree
    for (l in 1:(k + 1)) {
      prices[l] <- 1/(1 + R)^dt * (p * prices[l + 1] + q * prices[l])
    }
  }
  return (prices[1])
}

bisect_price <- function(Vobs, S0, K, h, n, N, R, dt, type, eps) {
  sig_lo <- 0.01
  sig_hi <- 1
  repeat {
    Vtest_lo <- price_option_i(S0, K, n, h, N, R, sig_lo, dt, type)
    Vtest_hi <- price_option_i(S0, K, n, h, N, R, sig_hi, dt, type)
    
    if (abs(Vtest_lo - Vobs) < eps) {
      return (sig_lo)
    }  else if (abs(Vtest_hi - Vobs) < eps) {
      return (sig_hi)
    } else if ( (Vtest_lo < Vobs) && (Vtest_hi > Vobs) ) { # Vobs is somewhere in between
      sig_test <- (sig_lo + sig_hi)/2 # bisect
      Vtest <- price_option_i(S0, K, n, h, N, R, sig_test, dt, type)

      while ((abs(Vtest - Vobs) > eps)) {
        if (Vtest > Vobs) { # price too high, contract right endpoint
          sig_hi <- sig_test
          sig_test <- (sig_lo + sig_hi)/2
        } else { # price too low, contract left endpoint
          sig_lo <- sig_test
          sig_test <- (sig_lo + sig_hi)/2
        }
        Vtest <- price_option_i(S0, K, n, h, N, R, sig_test, dt, type)
      }
      return (sig_test)
      
    } else if (Vtest_lo > Vobs) { # sig_lo was too high
      sig_lo <- sig_lo/10
      if (sig_lo < eps) {
        return (0)
      }
    } else { # sig_hi was too low
      sig_hi <- 2 * sig_hi
    }
  } # end repeat
}





#N <- 100
#type <- as.character(tolower(dat$Call.or.Put[1]))
#vols <- double(N)
#pt <- proc.time()
#for (i in 1:N) {
#  dt <- dat$maturity[1]/i
#  vols[i] <- bisect_price(Vobs = dat$Ask.Price[1], S0 = S0_01, K = dat$Strike[1], n = 0, h = 0, N = i, R = R, dt = dt, type = type, eps = 10^-5)
#}
#plot(vols, type = 'l')
#proc.time() - pt

S0 <- NA
N <- 100
dat$imp_vol <- NA
for (i in 1:nrow(dat)) {
  print(i)
  type <- as.character(tolower(dat$Call.or.Put[i]))
  dt <- dat$maturity[i]/N
  day <- strsplit(as.character(dat$Date[i]), "-")[[1]][3]
  if (day == "01") {
    S0 <- S0_01
  } else if (day == "02") {
    S0 <- S0_02
  } else {
    S0 <- S0_03
  }
  dat$imp_vol[i] <- bisect_price(Vobs = dat$Ask.Price[i], S0 = S0, K = dat$Strike[i], n = 0, h = 0, N = N, R = R, dt = dt, type = type, eps = 10^-5)
}


library(ggplot2)


ggplot(dat, aes(x = Strike, y = imp_vol, shape = Call.or.Put)) +
  geom_point(size = 5) +
  facet_grid(Call.or.Put~Date) +
  theme_bw()


Nvec <- seq(1,10,1)
l <- 1
S0 <- S0_01
vols <- double()
i <- 1
for (N in Nvec) {
  print(N)
  type <- as.character(tolower(dat$Call.or.Put[l]))
  dt <- dat$maturity[l]/N
  vols[i] <- bisect_price(Vobs = dat$Ask.Price[l], S0 = S0, K = dat$Strike[l], n = 0, h = 0, N = N, R = R, dt = dt, type = type, eps = 10^-5)
  i <- i + 1
}

Vobs <- 10
S0 <- 100
K <- 100
n <- 0
h <- 0
N <- 1000
R <- 0.05
sigma <- 0.25
T <- 1
dt <- T/N
type <- 'c'

pt <- proc.time()
price_option_i(S0 = S0, K = K, n = n, h = h, N = N, R = R, sigma = sigma, dt = dt, type = type)
proc.time() - pt

pt <- proc.time()
bisect_price(Vobs = Vobs, S0 = S0, K = K, h = h, n = n, N = N, R = R, dt = dt, type = type, eps = 10^-5)
proc.time() - pt

