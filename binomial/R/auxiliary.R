# private auxiliary functions
aux_mean <- function(trials, prob) {
  mean <- trials*prob
  return(mean)
}

aux_variance <- function(trials, prob) {
  variance <- trials*prob*(1-prob)
  return(variance)
}

aux_mode <- function(trials, prob) {
  mode <- floor((trials*prob) + prob)
  return(mode)
}

aux_skewness <- function(trials, prob) {
  skew <- (1 - 2*prob) / sqrt(trials*prob*(1-prob))
  return(skew)
}

aux_kurtosis <- function(trials, prob) {
  kurt <- (1 - (6*prob*(1 - prob))) / (trials*prob*(1 - prob))
  return(kurt)
}
