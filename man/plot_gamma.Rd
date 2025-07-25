% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_gamma.R
\name{plot_gamma}
\alias{plot_gamma}
\title{Heat Plot for Transition Probability Matrix of
a Bayesian Multilevel Hidden Markov Model}
\usage{
plot_gamma(
  model = NULL,
  level = "group",
  subject = NULL,
  digits = 2,
  facet = TRUE,
  ncol_facet = 2
)
}
\arguments{
\item{model}{Object of type `mHMMbayes::mHMM` or `mHMMbayes::mHMM_gamma`,
created using [mHMMbayes::mHMM()] or [mHMMbayes::obtain_gamma()].}

\item{level}{String specifying the level of transition distributions to plot.
Options are "group" and "subject".}

\item{subject}{Optional integer or integer vector specifying
the subject(s) to plot.}

\item{digits}{Integer specifying the number of digits to round to.}

\item{facet}{Logical specifying whether subjects should be faceted
when plotting subject-specific transition probability matrices.}

\item{ncol_facet}{Integer specifying the number of columns in the facet grid
when plotting subject-specific transition probability matrices.}
}
\value{
Object of type `ggplot2::gg` with the visualized transition distributions.
}
\description{
Heat Plot for Transition Probability Matrix of
a Bayesian Multilevel Hidden Markov Model
}
\examples{
\dontrun{
library(mHMMbayes)
# simulating multivariate continuous data
n_t <- 100
n <- 10
m <- 3
n_dep <- 2

gamma <- matrix(c(
  0.8, 0.1, 0.1,
  0.2, 0.7, 0.1,
  0.2, 0.2, 0.6
), ncol = m, byrow = TRUE)

emiss_distr <- list(
  matrix(c(
    50, 10,
    100, 10,
    150, 10
  ), nrow = m, byrow = TRUE),
  matrix(c(
    5, 2,
    10, 5,
    20, 3
  ), nrow = m, byrow = TRUE)
)

data_cont <- sim_mHMM(
  n_t = n_t, n = n, data_distr = "continuous",
  gen = list(m = m, n_dep = n_dep),
  gamma = gamma, emiss_distr = emiss_distr,
  var_gamma = .1, var_emiss = c(5^2, 0.2^2)
)

# Specify hyper-prior for the continuous emission distribution
manual_prior_emiss <- prior_emiss_cont(
  gen = list(m = m, n_dep = n_dep),
  emiss_mu0 = list(
    matrix(c(30, 70, 170), nrow = 1),
    matrix(c(7, 8, 18), nrow = 1)
  ),
  emiss_K0 = list(1, 1),
  emiss_V = list(rep(5^2, m), rep(0.5^2, m)),
  emiss_nu = list(1, 1),
  emiss_a0 = list(rep(1.5, m), rep(1, m)),
  emiss_b0 = list(rep(20, m), rep(4, m))
)

# Run the model on the simulated data:
# Note that for reasons of running time, J is set at a ridiculous low value.
# One would typically use a number of iterations J of at least 1000,
# and a burn_in of 200.
out_3st_cont_sim <- mHMM(
  s_data = data_cont$obs,
  data_distr = "continuous",
  gen = list(m = m, n_dep = n_dep),
  start_val = c(list(gamma), emiss_distr),
  emiss_hyp_prior = manual_prior_emiss,
  mcmc = list(J = 11, burn_in = 5)
)

plot_gamma(out_3st_cont_sim)
}
}
