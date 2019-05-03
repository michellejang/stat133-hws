# private checker functions

check_prob <- function(prob) {
  if (!is.numeric(prob)){
    stop("prob must be a numeric value")
  }
  if (length(prob) != 1){
    stop("prob must be one numeric value")
  }
  if (prob < 0 | prob > 1){
    stop("prob value must be between 0 and 1")
  }
  TRUE
}

check_trials <- function(trials) {
  if (!is.numeric(trials)){
    stop("trials must be a numeric value")
  }
  if (!is.wholenumber(trials)){
    stop("trials must be an integer")
  }
  if (trials < 0){
    stop("invalid trials value")
  }
  TRUE
}

check_success <- function(success, trials) {
  if (!is.numeric(success)){
    stop("success must be a numeric value")
  }
  if (any(success < 0)){
    stop("invalid success value")
  }
  if (any(success > trials)){
    stop("success cannot be greater than trials")
  }
  TRUE
}


