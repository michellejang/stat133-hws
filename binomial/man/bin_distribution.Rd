% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/main.R
\name{bin_distribution}
\alias{bin_distribution}
\title{Distribution}
\usage{
bin_distribution(trials, prob)
}
\arguments{
\item{prob}{probability of a success}

\item{n}{number of trials}
}
\value{
dataframe
}
\description{
Calculates the corresponding binomial probabilities of each k success in n trials
}
\examples{
# binomial probability distribution
bin_distribution(trials = 5, prob = 0.5)
}
