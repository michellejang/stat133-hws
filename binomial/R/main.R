#' @title Choose
#' @description Calculates number of combinations in which k successes can occur in n trials
#' @param n number of trials
#' @param k number of success (can be vector)
#' @return a numeric value or vector of numeric values
#' @export
#' @examples
#' # number of combinations of 3 successes that can occur in 5 trials
#' bin_choose(5, 3)

bin_choose <- function(n, k) {
  if (!is.numeric(k) | !is.numeric(n)){
    stop("n and k must be numeric values")
  }
  if (any(k > n)){
    stop("k cannot be greater than n")
  }
  number <- factorial(n) / (factorial(k)*factorial(n-k))
  return(number)
}

#' @title Probability
#' @description Calculates the probability of k successes in n trials with a specified probabilit of k occurring
#' @param n number of trials
#' @param k number of successes (can be vector)
#' @param prob probability of k occurring
#' @return a probability
#' @export
#' @examples
#' # probability of getting 2 successes in 5 trials
#' # (assuming probability of success = 0.5)
#' bin_probability(success = 2, trials = 5, prob = 0.5)
#'
#' # 55 heads in 100 tosses of a loaded coin with 45% chance of heads
#' bin_probability(success = 55, trials = 100, prob = 0.45)

bin_probability <- function(trials, success, prob) {
  check_trials(trials)
  if (check_trials(trials) != TRUE){
    stop("invalid trials value")
  }
  check_success(success, trials)
  if (check_success(success, trials) != TRUE){
    stop("invalid success value")
  }
  check_prob(prob)
  if (check_prob(prob) != TRUE){
    stop("invalid prob value")
  }
  result <- bin_choose(trials, success) * (prob^success) * ((1-prob)^(trials - success))
  return(result)
}

#' @title Distribution
#' @description Calculates the corresponding binomial probabilities of each k success in n trials
#' @param n number of trials
#' @param prob probability of a success
#' @return dataframe
#' @export
#' @examples
#' # binomial probability distribution
#' bin_distribution(trials = 5, prob = 0.5)

bin_distribution <- function(trials, prob) {
  table <- data.frame(success = 0:trials,
                      probability = bin_probability(trials, 0:trials, prob))
  class(table) <- c("bindis", "data.frame")
  return(table)
}

#' @export
plot.bindis <- function(x) {
  barplot(x$probability, xlab = "successes", ylab = "probability")
}

#' @title Cumulative Distribution
#' @description Calculates the cumulative probabilities of each k success in n trials
#' @param n number of trials
#' @param prob probability of a success
#' @return dataframe
#' @export
#' @examples
#' # binomial cumulative distribution
#' bin_cumulative(trials = 5, prob = 0.5)

bin_cumulative <- function(trials, prob) {
  table1 <- bin_distribution(trials, prob)
  table1$cumulative <- cumsum(table1$probability)
  class(table1) <- c("bincum", "data.frame")
  return(table1)
}

#' @export
plot.bincum <- function(x) {
  plot(x$success, x$cumulative, "l", xlab = "successes", ylab = "probability")
}

#' @title Variable
#' @description Classifies object as a binomial random variable with given parameters, also returns summary of the random variable
#' @param n number of trials
#' @param prob probability of a success
#' @return an object of class "binvar", a binomial random variable object
#' @export
#' @examples
#' # bin_var(10, 0.5)

bin_var <- function(trials, prob) {
  check_trials(trials)
  check_prob(prob)

  object <- list(trials = trials,
                 prob = prob)
  class(object) <- "binvar"
  return(object)
}

#' @export
print.binvar <- function(x) {
  cat('"Binomial Variable" \n\n')
  cat("Parameters", '\n')
  cat("- number of trials:", x$trials, '\n')
  cat("- prob of success:", x$prob)
}

#' @export
summary.binvar <- function(x) {
  mean <- aux_mean(x$trials, x$prob)
  variance <- aux_variance(x$trials, x$prob)
  mode <- aux_mode(x$trials, x$prob)
  skew <- aux_skewness(x$trials, x$prob)
  kurt <- aux_kurtosis(x$trials, x$prob)
  object1 <- list(trials = x$trials,
                  prob = x$prob,
                  mean = mean,
                  variance = variance,
                  mode = mode,
                  skewness = skew,
                  kurtosis = kurt)
  class(object1) <- "summary.binvar"
  return(object1)
}

#' @export
print.summary.binvar <- function(x) {
  cat('"Summary Binomial" \n\n')
  cat("Parameters", '\n')
  cat("- number of trials:", x$trials, '\n')
  cat("- prob of success:", x$prob, '\n\n')
  cat("Measures", '\n')
  cat("- mean:", x$mean, '\n')
  cat("- variance:", x$variance, '\n')
  cat("- mode:", x$mode, '\n')
  cat("- skewness:", x$skew, '\n')
  cat("- kurtosis:", x$kurt)
}

#' @title Mean
#' @description Calculates the mean
#' @param trials number of trials
#' @param prob probability of success
#' @return a numeric value
#' @export
#' @examples
#' # mean of binomial random distribution with 10 trials and prob = 0.5
#' # bin_mean(10, 0.5)

bin_mean <- function(trials, prob) {
  mean <- aux_mean(trials, prob)
  return(mean)
}

#' @title Variance
#' @description Calculates the variance
#' @param trials number of trials
#' @param prob probability of success
#' @return a numeric value
#' @export
#' @examples
#' # variance of binomial random distribution with 10 trials and prob = 0.5
#' # bin_variance(10, 0.5)

bin_variance <- function(trials, prob) {
  var <- aux_variance(trials, prob)
  return(var)
}

#' @title Mode
#' @description Calculates the mode
#' @param trials number of trials
#' @param prob probability of success
#' @return a numeric value
#' @export
#' @examples
#' mode of binomial random distribution with 10 trials and prob = 0.5
#' # bin_mode(10, 0.5)

bin_mode <- function(trials, prob) {
  mode <- aux_mode(trials, prob)
  return(mode)
}

#' @title Skewness
#' @description Calculates the skewness
#' @param trials number of trials
#' @param prob probability of success
#' @return a numeric value
#' @export
#' @examples
#' skeness of binomial random distribution with 10 trials and prob = 0.5
#' # bin_skewness(10, 0.5)

bin_skewness <- function(trials, prob) {
  skew <- aux_skewness(trials, prob)
  return(skew)
}

#' @title Kurtosis
#' @description Calculates the kurtosis
#' @param trials number of trials
#' @param prob probability of success
#' @return a numeric value
#' @export
#' @examples
#' kurtosis of binomial random distribution with 10 trials and prob = 0.5
#' # bin_kurtosis(10, 0.5)

bin_kurtosis <- function(trials, prob) {
  kurt <- aux_kurtosis(trials, prob)
  return(kurt)
}
