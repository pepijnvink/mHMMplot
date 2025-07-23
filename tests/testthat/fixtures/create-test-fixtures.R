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
set.seed(42)
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
out_3st_cont <- mHMM(
  s_data = data_cont$obs,
  data_distr = "continuous",
  gen = list(m = m, n_dep = n_dep),
  start_val = c(list(gamma), emiss_distr),
  emiss_hyp_prior = manual_prior_emiss,
  mcmc = list(J = 100, burn_in = 5)
)
saveRDS(data_cont$obs, testthat::test_path("fixtures", "data_cont.rds"))
saveRDS(out_3st_cont, testthat::test_path("fixtures", "mhmm_cont.rds"))

# Categorical mHMM
data(nonverbal)
m <- 2
n_dep <- 4
q_emiss <- c(3, 2, 3, 2)

# specifying starting values
start_TM <- diag(.8, m)
start_TM[lower.tri(start_TM) | upper.tri(start_TM)] <- .2
start_EM <- list(
  matrix(
    c(
      0.05, 0.90, 0.05,
      0.90, 0.05, 0.05
    ),
    byrow = TRUE,
    nrow = m, ncol = q_emiss[1]
  ), # vocalizing patient
  matrix(
    c(
      0.1, 0.9,
      0.1, 0.9
    ),
    byrow = TRUE, nrow = m,
    ncol = q_emiss[2]
  ), # looking patient
  matrix(
    c(
      0.90, 0.05, 0.05,
      0.05, 0.90, 0.05
    ),
    byrow = TRUE,
    nrow = m, ncol = q_emiss[3]
  ), # vocalizing therapist
  matrix(
    c(
      0.1, 0.9,
      0.1, 0.9
    ),
    byrow = TRUE, nrow = m,
    ncol = q_emiss[4]
  )
) # looking therapist
set.seed(42)
out_2st_cat <- mHMM(
  s_data = nonverbal,
  gen = list(m = m, n_dep = n_dep, q_emiss = q_emiss),
  start_val = c(list(start_TM), start_EM),
  mcmc = list(J = 100, burn_in = 5)
)
set.seed(123)
out_2st_cat_2 <- mHMM(
  s_data = nonverbal,
  gen = list(m = m, n_dep = n_dep, q_emiss = q_emiss),
  start_val = c(list(start_TM), start_EM),
  mcmc = list(J = 100, burn_in = 5)
)
saveRDS(out_2st_cat, testthat::test_path("fixtures", "mhmm_cat.rds"))
saveRDS(out_2st_cat_2, testthat::test_path("fixtures", "mhmm_cat_2.rds"))

vit_2st <- vit_mHMM(
  out_2st_cat,
  nonverbal
)
saveRDS(vit_2st, testthat::test_path("fixtures", "viterbi.rds"))

## gamma matrices
gamma_group <- obtain_gamma(out_2st, level = "group")
gamma_subj <- obtain_gamma(out_2st, level = "subject")
saveRDS(gamma_group, testthat::test_path("fixtures", "gamma_group.rds"))
saveRDS(gamma_subj, testthat::test_path("fixtures", "gamma_subj.rds"))
