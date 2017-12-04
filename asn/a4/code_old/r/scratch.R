R <- 0.05
sigma <- 0.25
dt <- 0.1

p <- 0.5 # global variable
q <- 0.5 # global variable


u_fxn <- function(R, sigma, dt) {
  exp((R - 0.5 * sigma^2) * dt + sigma * sqrt(dt)) 
  return (2)
}
d_fxn <- function(R, sigma, dt) {
  exp((R - 0.5 * sigma^2) * dt - sigma * sqrt(dt))
  return (0.5)
}
asset <- function(S0, h, N, R, sigma, dt) {
  u <- u_fxn(R, sigma, dt)
  d <- d_fxn(R, sigma, dt)
  S0 * u^h * d^(N - h)
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
price_option <- function(S0, K, n, h, N, R, sigma, dt, type) {
  if (!(type %in% c('c','p'))) {
    warning("Warning in price_option: Invalid type.")
    return (0)
  }
  
  prices <- double(N - n + 1)
  for (i in 0:(length(prices) - 1)) { 
    S <- asset(S0, i, N, R, sigma, dt)
    prices[i + 1] <- payoff(S, K, type)
  }

  for (i in N:(n + 1)) {
    for (j in 0:(i - n)) {
      prices[j + 1] <- 1/(1 + R) * 0.5 * (prices[j + 1] + prices[j + 2])
    }
  }
  return (prices[1])
}

S0 <- 4
K <- 5
n <- 0 # desired step to price
h <- 1 # nb of heads
N <- 2 # total steps
R <- 0.25
sigma <- 0.25
T <- 1
dt <- T/N
type <- 'p'

price_option(S0, K, n, h, N, R, sigma, dt, type)







