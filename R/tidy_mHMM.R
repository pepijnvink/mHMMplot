#' Turn an mHMM object into a tidy tibble
#'
#'
#' @param model An object to be converted into a tidy [tibble::tibble()]
#' @param ... Additional arguments to tidying method
#'
#' @returns A [tibble::tibble()] with information about model components
#'
#' @export
#'
tidy_mHMM <- function(model, ...) {
  UseMethod('tidy_mHMM')
}

#' @keywords internal
# tidy gamma output for tidy_mHMM methods (group level)
tidy_gamma_group <- function(model, m, burn_in, J, ci, ess, quantiles, prob) {
  if(prob){
median_gamma <- apply(model$gamma_int_bar[(burn_in + 1):J, ], 2, stats::median) %>%
    matrix(nrow = m, byrow = TRUE) %>%
    mHMMbayes::int_to_prob() %>%
    t() %>%
    as.vector()
  mean_gamma <- apply(model$gamma_int_bar[(burn_in + 1):J, ], 2, mean) %>%
    matrix(nrow = m, byrow = TRUE) %>%
    mHMMbayes::int_to_prob() %>%
    t() %>%
    as.vector()
  allpars <- data.frame(
    from_state = factor(paste('state', rep(1:m, each = m))),
    to_state = factor(paste('state', rep(1:m, times = m))),
    level = 'group'
  ) %>%
    dplyr::mutate(median = median_gamma, mean = mean_gamma)
    if(ci){
  ci_gamma <- apply(
    model$gamma_prob_bar[(burn_in + 1):J, ],
    2,
    stats::quantile,
    quantiles
  ) %>%
    t()
      allpars <- allpars %>%
        cbind(ci_gamma)
    }
    if(ess){
      ess_bulk_gamma <- apply(model$gamma_prob_bar[(burn_in + 1):J, ], 2, posterior::ess_bulk)
      ess_tail_gamma <- apply(model$gamma_prob_bar[(burn_in + 1):J, ], 2, posterior::ess_bulk)
      ess_both <- data.frame(ess_bulk = ess_bulk_gamma, ess_tail = ess_tail_gamma)
      allpars <- allpars %>%
        cbind(ess_both)
    }
    allpars <- allpars %>%
    tibble::remove_rownames() %>%
    tibble::as_tibble()
  } else {
    median_gamma <- apply(model$gamma_int_bar[(burn_in + 1):J, ], 2, stats::median) %>%
    matrix(nrow = m, byrow = TRUE) %>%
    t() %>%
    as.vector()
  mean_gamma <- apply(model$gamma_int_bar[(burn_in + 1):J, ], 2, mean) %>%
    matrix(nrow = m, byrow = TRUE) %>%
    t() %>%
    as.vector()
  allpars <- data.frame(
    from_state = factor(paste('state', rep(1:m, each = m-1))),
    to_state = factor(paste('state', rep(2:m, times = m))),
    level = 'group'
  ) %>%
    dplyr::mutate(median = median_gamma, mean = mean_gamma)
    if(ci){
  ci_gamma <- apply(
    model$gamma_int_bar[(burn_in + 1):J, ],
    2,
    stats::quantile,
    quantiles
  ) %>%
    t()
      allpars <- allpars %>%
        cbind(ci_gamma)
    }
    if(ess){
      ess_bulk_gamma <- apply(model$gamma_int_bar[(burn_in + 1):J, ], 2, posterior::ess_bulk)
      ess_tail_gamma <- apply(model$gamma_int_bar[(burn_in + 1):J, ], 2, posterior::ess_bulk)
      ess_both <- data.frame(ess_bulk = ess_bulk_gamma, ess_tail = ess_tail_gamma)
      allpars <- allpars %>%
        cbind(ess_both)
    }
    allpars <- allpars %>%
    tibble::remove_rownames() %>%
    tibble::as_tibble()
  }
  return(allpars)
}

#' @keywords internal
# tidy gamma output for tidy_mHMM methods (subject level)
tidy_gamma_subj <- function(model, m, subjects, burn_in, J, ci, ess, quantiles, prob) {
  all_gamma <- vector('list', length(subjects))
  if(prob){
 for (i in subjects) {
    median_gamma <- apply(
      model$gamma_int_subj[[i]][(burn_in + 1):J, ],
      2,
      stats::median
    ) %>%
      matrix(nrow = m, byrow = TRUE) %>%
      mHMMbayes::int_to_prob() %>%
      t() %>%
      as.vector()
    mean_gamma <- apply(
      model$gamma_int_subj[[i]][(burn_in + 1):J, ],
      2,
      mean
    ) %>%
      matrix(nrow = m, byrow = TRUE) %>%
      mHMMbayes::int_to_prob() %>%
      t() %>%
      as.vector()
   allpars <- data.frame(
      from_state = factor(paste('state', rep(1:m, each = m))),
      to_state = factor(paste('state', rep(1:m, times = m))),
      level = 'subject',
      subject = factor(paste('subject', i))
    ) %>%
      dplyr::mutate(median = median_gamma, mean = mean_gamma)
   if(ci){
     ci_gamma <- apply(
      model$PD_subj[[i]]$trans_prob[(burn_in + 1):J, ],
      2,
      stats::quantile,
      quantiles
    ) %>%
      t()
     allpars <- allpars %>%
       cbind(ci_gamma)
   }
    if(ess){
      ess_bulk_gamma <- apply(model$PD_subj[[i]]$trans_prob[(burn_in + 1):J, ], 2, posterior::ess_bulk)
      ess_tail_gamma <- apply(model$PD_subj[[i]]$trans_prob[(burn_in + 1):J, ], 2, posterior::ess_bulk)
      ess_both <- data.frame(ess_bulk = ess_bulk_gamma, ess_tail = ess_tail_gamma)
      allpars <- allpars %>%
        cbind(ess_both)
    }
    all_gamma[[i]] <- allpars %>%
      tibble::remove_rownames() %>%
      tibble::as_tibble()
 }
  } else {
     for (i in subjects) {
    median_gamma <- apply(
      model$gamma_int_subj[[i]][(burn_in + 1):J, ],
      2,
      stats::median
    ) %>%
      matrix(nrow = m, byrow = TRUE) %>%
      t() %>%
      as.vector()
    mean_gamma <- apply(
      model$gamma_int_subj[[i]][(burn_in + 1):J, ],
      2,
      mean
    ) %>%
      matrix(nrow = m, byrow = TRUE) %>%
      t() %>%
      as.vector()
       allpars <- data.frame(
      from_state = factor(paste('state', rep(1:m, each = m-1))),
      to_state = factor(paste('state', rep(2:m, times = m))),
      level = 'subject',
      subject = factor(paste('subject', i))
    ) %>%
      dplyr::mutate(median = median_gamma, mean = mean_gamma)
       if(ci){
         ci_gamma <- apply(
      model$gamma_int_subj[[i]][(burn_in + 1):J, ],
      2,
      stats::quantile,
      quantiles
    ) %>%
      t()
         allpars <- allpars %>%
           cbind(ci_gamma)
       }
    if(ess){
      ess_bulk_gamma <- apply(model$gamma_int_subj[[i]][(burn_in + 1):J, ], 2, posterior::ess_bulk)
      ess_tail_gamma <- apply(model$gamma_int_subj[[i]][(burn_in + 1):J, ], 2, posterior::ess_bulk)
      ess_both <- data.frame(ess_bulk = ess_bulk_gamma, ess_tail = ess_tail_gamma)
      allpars <- allpars %>%
        cbind(ess_both)
    }
    all_gamma[[i]] <- allpars %>%
      tibble::remove_rownames() %>%
      tibble::as_tibble()
     }
  }
  allgamma <- all_gamma %>%
    dplyr::bind_rows()
  return(allgamma)
}

#' Tidy a continuous mHMM object
#'
#' @param model The model of class `mHMM`, fit using [mHMMbayes::mHMM()]
#' @param param String, specifying the parameters to obtain a tidy summary for. Takes 'gamma' or 'emiss'
#' @param level String specifying the level to obtain a tidy summary for. Takes 'group' or 'subject'
#' @param prob If `TRUE`, returns parameters on the probability scale, if FALSE, returns parameters on the logit scale. Only used if `param = 'gamma'`
#' @param ci Logical indicating whether credible intervals should be computed.
#' @param ess Logical indicating whether effective sample size should be computed.
#' @param quantiles Numeric vector specifying the quantiles to use to obtain credible intervals.
#' @param subjects Optional numeric vector specifying the subjects to obtain a tidy summary for. Ignored when `level = 'group'`.
#' @param burn_in Optional integer values specifying the number of burnin samples to discard.
#' @param ... Additional arguments to tidying method. Currently not used
#'
#' @returns A [tibble::tibble()] with summary for the model.
#' @export
#'
#' @examples
#' \dontrun{
#' library(mHMMbayes)
#' # simulating multivariate continuous data
#' n_t <- 100
#' n <- 10
#' m <- 3
#' n_dep <- 2
#'
#' gamma <- matrix(c(
#'   0.8, 0.1, 0.1,
#'   0.2, 0.7, 0.1,
#'   0.2, 0.2, 0.6
#' ), ncol = m, byrow = TRUE)
#'
#' emiss_distr <- list(
#'   matrix(c(
#'     50, 10,
#'     100, 10,
#'     150, 10
#'   ), nrow = m, byrow = TRUE),
#'   matrix(c(
#'     5, 2,
#'     10, 5,
#'     20, 3
#'   ), nrow = m, byrow = TRUE)
#' )
#'
#' data_cont <- sim_mHMM(
#'   n_t = n_t, n = n, data_distr = "continuous",
#'   gen = list(m = m, n_dep = n_dep),
#'   gamma = gamma, emiss_distr = emiss_distr,
#'   var_gamma = .1, var_emiss = c(5^2, 0.2^2)
#' )
#'
#' # Specify hyper-prior for the continuous emission distribution
#' manual_prior_emiss <- prior_emiss_cont(
#'   gen = list(m = m, n_dep = n_dep),
#'   emiss_mu0 = list(
#'     matrix(c(30, 70, 170), nrow = 1),
#'     matrix(c(7, 8, 18), nrow = 1)
#'   ),
#'   emiss_K0 = list(1, 1),
#'   emiss_V = list(rep(5^2, m), rep(0.5^2, m)),
#'   emiss_nu = list(1, 1),
#'   emiss_a0 = list(rep(1.5, m), rep(1, m)),
#'   emiss_b0 = list(rep(20, m), rep(4, m))
#' )
#'
#' # Run the model on the simulated data:
#' # Note that for reasons of running time, J is set at a ridiculous low value.
#' # One would typically use a number of iterations J of at least 1000,
#' # and a burn_in of 200.
#' out_3st_cont_sim <- mHMM(
#'   s_data = data_cont$obs,
#'   data_distr = "continuous",
#'   gen = list(m = m, n_dep = n_dep),
#'   start_val = c(list(gamma), emiss_distr),
#'   emiss_hyp_prior = manual_prior_emiss,
#'   mcmc = list(J = 11, burn_in = 5)
#' )
#'
#' tidy_mHMM(out_3st_cont_sim)
#' }
tidy_mHMM.cont <- function(
  model,
  param = 'gamma',
  level = "group",
  prob = TRUE,
  ci = TRUE,
  ess = TRUE,
  quantiles = c(0.025, 0.975),
  subjects = NULL,
  burn_in = NULL,
  ...
) {
  if (level %nin% c('group', 'subject')) {
    cli::cli_warn(c(
      'The argument {.var level} only takes \'group\' or \'subject\' as levels',
      'i' = 'Returned are the group-level estimates'
    ))
    level <- 'group'
  }
  if (param %nin% c('gamma', 'emiss')) {
    cli::cli_warn(c(
      'The argument {.var level} only takes \'group\' or \'subject\' as levels',
      'i' = 'Returned are estimates for gamma'
    ))
    param <- 'gamma'
  }
    if(!is.numeric(quantiles)){
    cli::cli_abort(c('!' = 'The argument {.var quantiles} must be a numeric vector.',
    'x' = 'You have specified a {.cls {class(quantiles)}} vector.'
    ))
  }
  if(min(quantiles) < 0 | max(quantiles) > 1){
    cli::cli_abort(c('!' = 'The elements in {.var quantiles} must be between 0 and 1.',
    'x' = 'At least one element is outside of these bounds.'
    ))
  }
  J <- model$input$J
  m <- model$input$m
  n_dep <- model$input$n_dep
  vrbs <- model$input$dep_labels
  if (is.null(burn_in)) {
    burn_in <- model$input$burn_in
  }
  if (level == "group") {
    if (param == 'gamma') {
      allpars <- tidy_gamma_group(model, m, burn_in, J, ci, ess, quantiles, prob)
    } else {
      median_emiss_mu <- model$emiss_mu_bar %>%
        lapply(function(x) x[(burn_in + 1):J, ] %>% apply(2, stats::median)) %>%
        unlist()
      mean_emiss_mu <- model$emiss_mu_bar %>%
        lapply(function(x) x[(burn_in + 1):J, ] %>% apply(2, mean)) %>%
        unlist()
      median_emiss_sd <- model$emiss_sd_bar %>%
        lapply(function(x) x[(burn_in + 1):J, ] %>% apply(2, stats::median)) %>%
        unlist()
      mean_emiss_sd <- model$emiss_sd_bar %>%
        lapply(function(x) x[(burn_in + 1):J, ] %>% apply(2, mean)) %>%
        unlist()
      median_emiss_varmu <- model$emiss_varmu_bar %>%
        lapply(function(x) x[(burn_in + 1):J, ] %>% apply(2, stats::median)) %>%
        unlist()
      mean_emiss_varmu <- model$emiss_varmu_bar %>%
        lapply(function(x) x[(burn_in + 1):J, ] %>% apply(2, mean)) %>%
        unlist()
      median_emiss_sdmu <- model$emiss_varmu_bar %>%
        lapply(function(x) x[(burn_in + 1):J, ] %>% sqrt() %>% apply(2, stats::median)) %>%
        unlist()
      mean_emiss_sdmu <- model$emiss_varmu_bar %>%
        lapply(function(x) x[(burn_in + 1):J, ] %>% sqrt() %>% apply(2, mean)) %>%
        unlist()
      all_mu <- data.frame(
        param = 'mu',
        vrb = factor(rep(vrbs, each = m), levels = vrbs),
        state = factor(paste('state', rep(1:m, times = n_dep))),
        level = 'group'
      ) %>%
        dplyr::mutate(median = median_emiss_mu, mean = mean_emiss_mu)
      all_sd <- data.frame(
        param = 'sd',
        vrb = factor(rep(vrbs, each = m), levels = vrbs),
        state = factor(paste('state', rep(1:m, times = n_dep))),
        level = 'group'
      ) %>%
        dplyr::mutate(median = median_emiss_sd, mean = mean_emiss_sd)
      all_varmu <- data.frame(
        param = 'varmu',
        vrb = factor(rep(vrbs, each = m), levels = vrbs),
        state = factor(paste('state', rep(1:m, times = n_dep))),
        level = 'group'
      ) %>%
        dplyr::mutate(median = median_emiss_varmu, mean = mean_emiss_varmu)
      all_sdmu <- data.frame(
        param = 'sdmu',
        vrb = factor(rep(vrbs, each = m), levels = vrbs),
        state = factor(paste('state', rep(1:m, times = n_dep))),
        level = 'group'
      ) %>%
        dplyr::mutate(median = median_emiss_sdmu, mean = mean_emiss_sdmu)
      if(ci){
      ci_emiss_mu <- model$emiss_mu_bar %>%
        lapply(function(x) {
          x[(burn_in + 1):J, ] %>% apply(2, stats::quantile, quantiles) %>% t()
        }) %>%
        do.call(what = rbind)
        all_mu <- all_mu %>%
          cbind(ci_emiss_mu)
      ci_emiss_sd <- model$emiss_sd_bar %>%
        lapply(function(x) {
          x[(burn_in + 1):J, ] %>% apply(2, stats::quantile, quantiles) %>% t()
        }) %>%
        do.call(what = rbind)
        all_sd <- all_sd %>%
          cbind(ci_emiss_sd)
      ci_emiss_varmu <- model$emiss_varmu_bar %>%
        lapply(function(x) {
          x[(burn_in + 1):J, ] %>% apply(2, stats::quantile, quantiles) %>% t()
        }) %>%
        do.call(what = rbind)
        all_varmu <- all_varmu %>%
          cbind(ci_emiss_varmu)
      ci_emiss_sdmu <- model$emiss_varmu_bar %>%
        lapply(function(x) {
          x[(burn_in + 1):J, ] %>% 
            sqrt() %>%
            apply(2, stats::quantile, quantiles) %>% t()
        }) %>%
        do.call(what = rbind)
        all_sdmu <- all_sdmu %>%
          cbind(ci_emiss_sdmu)
      }
      if(ess){
      ess_bulk_emiss_mu <- model$emiss_mu_bar %>%
        lapply(function(x) x[(burn_in + 1):J, ] %>% apply(2, posterior::ess_bulk)) %>%
        unlist()
        ess_tail_emiss_mu <- model$emiss_mu_bar %>%
        lapply(function(x) x[(burn_in + 1):J, ] %>% apply(2, posterior::ess_tail)) %>%
        unlist()
        all_mu <- all_mu %>%
          cbind(data.frame(ess_bulk = ess_bulk_emiss_mu, ess_tail = ess_tail_emiss_mu))
      ess_bulk_emiss_sd <- model$emiss_sd_bar %>%
        lapply(function(x) x[(burn_in + 1):J, ] %>% apply(2, posterior::ess_bulk)) %>%
        unlist()
        ess_tail_emiss_sd <- model$emiss_sd_bar %>%
        lapply(function(x) x[(burn_in + 1):J, ] %>% apply(2, posterior::ess_tail)) %>%
        unlist()
        all_sd <- all_sd %>%
          cbind(data.frame(ess_bulk = ess_bulk_emiss_sd, ess_tail = ess_tail_emiss_sd))
      ess_bulk_emiss_varmu <- model$emiss_varmu_bar %>%
        lapply(function(x) x[(burn_in + 1):J, ] %>% apply(2, posterior::ess_bulk)) %>%
        unlist()
        ess_tail_emiss_varmu <- model$emiss_varmu_bar %>%
        lapply(function(x) x[(burn_in + 1):J, ] %>% apply(2, posterior::ess_tail)) %>%
        unlist()
        all_varmu <- all_varmu %>%
          cbind(data.frame(ess_bulk = ess_bulk_emiss_varmu, ess_tail = ess_tail_emiss_varmu))
      ess_bulk_emiss_sdmu <- model$emiss_varmu_bar %>%
        lapply(function(x) x[(burn_in + 1):J, ] %>% sqrt() %>% apply(2, posterior::ess_bulk)) %>%
        unlist()
        ess_tail_emiss_sdmu <- model$emiss_varmu_bar %>%
        lapply(function(x) x[(burn_in + 1):J, ] %>% sqrt() %>% apply(2, posterior::ess_tail)) %>%
        unlist()
        all_sdmu <- all_sdmu %>%
          cbind(data.frame(ess_bulk = ess_bulk_emiss_sdmu, ess_tail = ess_tail_emiss_sdmu))
      }
      allpars <- rbind(all_mu, all_sd, all_varmu, all_sdmu) %>%
        tibble::remove_rownames() %>%
        tibble::as_tibble()
    }
  } else {
    n_subj <- model$input$n_subj
    if (is.null(subjects)) {
      subjects <- 1:n_subj
    }
    if(!is.numeric(subjects)){
      cli::cli_warn(c(
        "Vector of subject indices {.var subjects} has to be a numeric or integer vector.",
        "x" = "You have supplied a {.cls {class(subjects)}} vector.",
        "i" = "Parameter estimates for all subjects will be returned."
      ))
      subjects <- 1:n_subj
    }
    if (min(subjects) < 1 | max(subjects) > n_subj){
      cli::cli_warn(c(
        'x' = 'Vector of subject indices was invalid.',
        'i' = 'Parameter estimates for all subjects will be returned.'
      ))
      subjects <- 1:n_subj
    }
    if (param == 'gamma') {
      allpars <- tidy_gamma_subj(model, m, subjects, burn_in, J, ci, ess, quantiles, prob)
    } else {
      all_emiss <- vector('list', length(subjects))
      for (i in subjects) {
        median_emiss_mu <- model$PD_subj[[i]]$cont_emiss[
          (burn_in + 1):J,
          1:(m * n_dep)
        ] %>%
          apply(2, stats::median)
        mean_emiss_mu <- model$PD_subj[[i]]$cont_emiss[
          (burn_in + 1):J,
          1:(m * n_dep)
        ] %>%
          apply(2, mean)
        all_emiss_i <- data.frame(
          param = 'mu',
          vrb = factor(rep(vrbs, each = m), levels = vrbs),
          state = factor(paste('state', rep(1:m, times = n_dep))),
          level = 'subject',
          subject = factor(paste('subject', i))
        ) %>%
          dplyr::mutate(median = median_emiss_mu, mean = mean_emiss_mu)
        if(ci){
        ci_emiss_mu <- model$PD_subj[[i]]$cont_emiss[
          (burn_in + 1):J,
          1:(m * n_dep)
        ] %>%
          apply(2, stats::quantile, quantiles) %>%
          t()
          all_emiss_i <- all_emiss_i %>%
            cbind(ci_emiss_mu)
        }
        if(ess){
          ess_bulk_emiss <- model$PD_subj[[i]]$cont_emiss[
          (burn_in + 1):J,
          1:(m * n_dep)
        ] %>%
          apply(2, posterior::ess_bulk)
          ess_tail_emiss <- model$PD_subj[[i]]$cont_emiss[
          (burn_in + 1):J,
          1:(m * n_dep)
        ] %>%
          apply(2, posterior::ess_bulk)
          all_emiss_i <- all_emiss_i %>%
            cbind(data.frame(ess_bulk = ess_bulk_emiss, ess_tail = ess_tail_emiss))
        }
        all_emiss[[i]] <-  all_emiss_i %>%
          tibble::remove_rownames() %>%
          tibble::as_tibble()
      }
      allpars <- all_emiss %>%
        dplyr::bind_rows()
    }
  }
  return(allpars)
}

#' Tidy a categorical mHMM object
#'
#' @param model The model of class `mHMM`, fit using [mHMMbayes::mHMM()]
#' @param param String, specifying the parameters to obtain a tidy summary for. Takes 'gamma' or 'emiss'
#' @param level String specifying the level to obtain a tidy summary for. Takes 'group' or 'subject'
#' @param prob If `TRUE`, returns parameters on the probability scale, if FALSE, returns parameters on the logit scale.
#' @param ci Logical indicating whether credible intervals should be computed.
#' @param ess Logical indicating whether effective sample size should be computed.
#' @param quantiles Numeric vector specifying the quantiles to use to obtain credible intervals.
#' @param subjects Optional numeric vector specifying the subjects to obtain a tidy summary for. Ignored when `level = 'group'`
#' @param burn_in Optional integer values specifying the number of burnin samples to discard.
#' @param ... Additional arguments to tidying method. Currently not used
#'
#' @returns A [tibble::tibble()] with summary for the model.
#' @export
#'
#' @examples
#' \dontrun{
#' library(mHMMbayes)
#' # simulating multivariate continuous data
#' n_t <- 100
#' n <- 10
#' m <- 3
#' n_dep <- 2
#'
#' gamma <- matrix(c(
#'   0.8, 0.1, 0.1,
#'   0.2, 0.7, 0.1,
#'   0.2, 0.2, 0.6
#' ), ncol = m, byrow = TRUE)
#'
#' emiss_distr <- list(
#'   matrix(c(
#'     50, 10,
#'     100, 10,
#'     150, 10
#'   ), nrow = m, byrow = TRUE),
#'   matrix(c(
#'     5, 2,
#'     10, 5,
#'     20, 3
#'   ), nrow = m, byrow = TRUE)
#' )
#'
#' data_cont <- sim_mHMM(
#'   n_t = n_t, n = n, data_distr = "continuous",
#'   gen = list(m = m, n_dep = n_dep),
#'   gamma = gamma, emiss_distr = emiss_distr,
#'   var_gamma = .1, var_emiss = c(5^2, 0.2^2)
#' )
#'
#' # Specify hyper-prior for the continuous emission distribution
#' manual_prior_emiss <- prior_emiss_cont(
#'   gen = list(m = m, n_dep = n_dep),
#'   emiss_mu0 = list(
#'     matrix(c(30, 70, 170), nrow = 1),
#'     matrix(c(7, 8, 18), nrow = 1)
#'   ),
#'   emiss_K0 = list(1, 1),
#'   emiss_V = list(rep(5^2, m), rep(0.5^2, m)),
#'   emiss_nu = list(1, 1),
#'   emiss_a0 = list(rep(1.5, m), rep(1, m)),
#'   emiss_b0 = list(rep(20, m), rep(4, m))
#' )
#'
#' # Run the model on the simulated data:
#' # Note that for reasons of running time, J is set at a ridiculous low value.
#' # One would typically use a number of iterations J of at least 1000,
#' # and a burn_in of 200.
#' out_3st_cont_sim <- mHMM(
#'   s_data = data_cont$obs,
#'   data_distr = "continuous",
#'   gen = list(m = m, n_dep = n_dep),
#'   start_val = c(list(gamma), emiss_distr),
#'   emiss_hyp_prior = manual_prior_emiss,
#'   mcmc = list(J = 11, burn_in = 5)
#' )
#'
#' tidy_mHMM(out_3st_cont_sim)
#' }
tidy_mHMM.cat <- function(
  model,
  param = 'gamma',
  level = "group",
  prob = TRUE,
  ci = TRUE,
  ess = TRUE,
  quantiles = c(0.025, 0.975),
  subjects = NULL,
  burn_in = NULL,
  ...
) {
  if (level %nin% c('group', 'subject')) {
    cli::cli_warn(c(
      'The argument {.var level} only takes \'group\' or \'subject\' as levels',
      'i' = 'Returned are the group-level estimates'
    ))
    level <- 'group'
  }
  if (param %nin% c('gamma', 'emiss')) {
    cli::cli_warn(c(
      'The argument {.var level} only takes \'group\' or \'subject\' as levels',
      'i' = 'Returned are estimates for gamma'
    ))
    param <- 'gamma'
  }
  if(!is.numeric(quantiles)){
    cli::cli_abort(c('!' = 'The argument {.var quantiles} must be a numeric vector.',
    'x' = 'You have specified a {.cls {class(quantiles)}} vector.'
    ))
  }
  if(min(quantiles) < 0 | max(quantiles) > 1){
    cli::cli_abort(c('!' = 'The elements in {.var quantiles} must be between 0 and 1.',
    'x' = 'At least one element is outside of these bounds.'
    ))
  }
  J <- model$input$J
  m <- model$input$m
  n_dep <- model$input$n_dep
  vrbs <- model$input$dep_labels
  q_emiss <- model$input$q_emiss
  if (is.null(burn_in)) {
    burn_in <- model$input$burn_in
  }
  if (level == "group") {
    if (param == 'gamma') {
      allpars <- tidy_gamma_group(model, m, burn_in, J, ci, ess, quantiles, prob)
    } else if(param == 'emiss') {
      if(prob){
        median_emiss <- lapply(model$emiss_int_bar, function(x) {
        apply(x[(burn_in + 1):J, ], 2, stats::median) %>%
          matrix(byrow = TRUE, nrow = m) %>%
          mHMMbayes::int_to_prob() %>%
          t() %>%
          as.vector() %>%
          tibble::as_tibble()
      }) %>%
        dplyr::bind_rows() %>%
        dplyr::rename(median = 'value')
        mean_emiss <- lapply(model$emiss_int_bar, function(x) {
        apply(x[(burn_in + 1):J, ], 2, mean) %>%
          matrix(byrow = TRUE, nrow = m) %>%
          mHMMbayes::int_to_prob() %>%
          t() %>%
          as.vector() %>%
          tibble::as_tibble()
      }) %>%
        dplyr::bind_rows() %>%
        dplyr::rename(mean = 'value')
        allpars <- tibble::tibble(
        param = 'emiss_prob',
        vrb = factor(rep(vrbs, times = q_emiss * m)),
        category = factor(paste(
          'category',
          unlist(lapply(q_emiss, function(q) rep(1:q, times = m)))
        )),
        state = factor(unlist(lapply(q_emiss, function(q) {
          rep(paste('state', 1:m), each = q)
        }))),
        level = 'group'
      ) %>%
        cbind(median_emiss, mean_emiss)
        if(ci){
          ci_emiss <- lapply(model$emiss_prob_bar, function(x) {
        apply(x[(burn_in + 1):J, ], 2, stats::quantile, quantiles) %>%
          t() %>%
          tibble::as_tibble()
      }) %>%
        dplyr::bind_rows()
          allpars <- allpars %>%
            cbind(ci_emiss)
        }
        if(ess){
          ess_bulk_emiss_prob <- lapply(model$emiss_prob_bar, function(x) {
        apply(x[(burn_in + 1):J, ], 2, posterior::ess_bulk)
          })
          ess_bulk_emiss_prob <- do.call('c', ess_bulk_emiss_prob)
          ess_tail_emiss_prob <- lapply(model$emiss_prob_bar, function(x) {
        apply(x[(burn_in + 1):J, ], 2, posterior::ess_tail)
          })
          ess_tail_emiss_prob <- do.call('c', ess_tail_emiss_prob)
          allpars <- allpars %>%
            cbind(data.frame(ess_bulk = ess_bulk_emiss_prob, ess_tail = ess_tail_emiss_prob))
          }
        allpars <- allpars %>%
        tibble::as_tibble()
      } else {
        median_emiss <- lapply(model$emiss_int_bar, function(x) {
        apply(x[(burn_in + 1):J, ], 2, stats::median) %>%
          matrix(byrow = TRUE, nrow = m) %>%
          t() %>%
          as.vector() %>%
          tibble::as_tibble()
      }) %>%
        dplyr::bind_rows() %>%
        dplyr::rename(median = 'value')
      mean_emiss <- lapply(model$emiss_int_bar, function(x) {
        apply(x[(burn_in + 1):J, ], 2, mean) %>%
          matrix(byrow = TRUE, nrow = m) %>%
          t() %>%
          as.vector() %>%
          tibble::as_tibble()
      }) %>%
        dplyr::bind_rows() %>%
        dplyr::rename(mean = 'value')
        allpars <- tibble::tibble(
        param = 'emiss_int',
        vrb = factor(rep(vrbs, times = (q_emiss-1) * m)),
        category = factor(paste(
          'category',
          unlist(lapply(q_emiss, function(q) rep(2:q, times = m)))
        )),
        state = factor(unlist(lapply(q_emiss, function(q) {
          rep(paste('state', 1:m), each = q-1)
        }))),
        level = 'group'
      ) %>%
        cbind(median_emiss, mean_emiss)
        if(ci){
          ci_emiss <- lapply(model$emiss_int_bar, function(x) {
        apply(x[(burn_in + 1):J, ], 2, stats::quantile, quantiles) %>%
          t() %>%
          tibble::as_tibble()
      }) %>%
        dplyr::bind_rows()
          allpars <- allpars %>%
            cbind(ci_emiss)
        }
        if(ess){
          ess_bulk_emiss_int <- lapply(model$emiss_int_bar, function(x) {
        apply(x[(burn_in + 1):J, ], 2, posterior::ess_bulk)
          })
          ess_bulk_emiss_int <- do.call('c', ess_bulk_emiss_int)
          ess_tail_emiss_int <- lapply(model$emiss_int_bar, function(x) {
        apply(x[(burn_in + 1):J, ], 2, posterior::ess_tail)
          })
          ess_tail_emiss_int <- do.call('c', ess_tail_emiss_int)
          allpars <- allpars %>%
            cbind(data.frame(ess_bulk = ess_bulk_emiss_int, ess_tail = ess_tail_emiss_int))
        }
        allpars <-  allpars %>%
        tibble::as_tibble()
      }
  }
  } else if(level == 'subject'){
    n_subj <- model$input$n_subj
    if (is.null(subjects)) {
      subjects <- 1:n_subj
    }
    if(!is.numeric(subjects)){
      cli::cli_warn(c(
        "Vector of subject indices {.var subjects} has to be a numeric or integer vector.",
        "x" = "You have supplied a {.cls {class(subjects)}} vector.",
        "i" = "Parameter estimates for all subjects will be returned."
      ))
      subjects <- 1:n_subj
    }
    if (min(subjects) < 1 | max(subjects) > n_subj){
      cli::cli_warn(c(
        'x' = 'Vector of subject indices was invalid.',
        'i' = 'Parameter estimates for all subjects will be returned.'
      ))
      subjects <- 1:n_subj
    }
    if (param == 'gamma') {
      allpars <- tidy_gamma_subj(model, m, subjects, burn_in, J, ci, ess, quantiles, prob)
    } else if(param == 'emiss'){
      if(prob){
        all_emiss <- vector('list', length(subjects))
      for (i in subjects) {
        median_emiss <- lapply(model$emiss_int_subj[[i]], function(x) {
          apply(x[(burn_in + 1):J, ], 2, stats::median) %>%
            matrix(byrow = TRUE, nrow = m) %>%
            mHMMbayes::int_to_prob() %>%
            t() %>%
            as.vector() %>%
            tibble::as_tibble()
        }) %>%
          dplyr::bind_rows() %>%
          dplyr::rename(median = 'value')
        mean_emiss <- lapply(model$emiss_int_subj[[i]], function(x) {
          apply(x[(burn_in + 1):J, ], 2, mean) %>%
            matrix(byrow = TRUE, nrow = m) %>%
            mHMMbayes::int_to_prob() %>%
            t() %>%
            as.vector() %>%
            tibble::as_tibble()
        }) %>%
          dplyr::bind_rows() %>%
          dplyr::rename(mean = 'value')
        all_emiss_i <- tibble::tibble(
          param = 'emiss_prob',
          vrb = factor(rep(vrbs, times = q_emiss * m)),
          category = factor(paste(
            'category',
            unlist(lapply(q_emiss, function(q) rep(1:q, times = m)))
          )),
          state = factor(unlist(lapply(q_emiss, function(q) {
            rep(paste('state', 1:m), each = q)
          }))),
          level = 'subject',
          subject = factor(paste('subject', i))
        ) %>%
          cbind(median_emiss, mean_emiss)
        if(ci){
        ci_emiss <- apply(
          model$PD_subj[[i]]$cat_emiss[(burn_in + 1):J, ],
          2,
          stats::quantile,
          quantiles
        ) %>%
          t()
          all_emiss_i <- all_emiss_i %>%
            cbind(ci_emiss)
        }
        if(ess){
          ess_bulk_emiss_prob <- apply(model$PD_subj[[i]]$cat_emiss[(burn_in + 1):J, ], 2, posterior::ess_bulk)
          ess_tail_emiss_prob <- apply(model$PD_subj[[i]]$cat_emiss[(burn_in + 1):J, ], 2, posterior::ess_tail)
          allpars <- allpars %>%
            cbind(data.frame(ess_bulk = ess_bulk_emiss_prob, ess_tail = ess_tail_emiss_prob))
        }
        all_emiss[[i]] <- all_emiss_i %>%
          tibble::as_tibble()
      }
      allpars <- all_emiss %>%
        dplyr::bind_rows()
      } else { ## if on logit scale
        all_emiss <- vector('list', length(subjects))
      for (i in subjects) {
        median_emiss <- lapply(model$emiss_int_subj[[i]], function(x) {
          apply(x[(burn_in + 1):J, ], 2, stats::median) %>%
            matrix(byrow = TRUE, nrow = m) %>%
            t() %>%
            as.vector() %>%
            tibble::as_tibble()
        }) %>%
          dplyr::bind_rows() %>%
          dplyr::rename(median = 'value')
        mean_emiss <- lapply(model$emiss_int_subj[[i]], function(x) {
          apply(x[(burn_in + 1):J, ], 2, mean) %>%
            matrix(byrow = TRUE, nrow = m) %>%
            t() %>%
            as.vector() %>%
            tibble::as_tibble()
        }) %>%
          dplyr::bind_rows() %>%
          dplyr::rename(mean = 'value')
        all_emiss_i <- tibble::tibble(
          param = 'emiss_int',
          vrb = factor(rep(vrbs, times = (q_emiss-1) * m), levels = vrbs),
          category = factor(paste(
            'category',
            unlist(lapply(q_emiss, function(q) rep(2:q, times = m)))
          )),
          state = factor(unlist(lapply(q_emiss, function(q) {
            rep(paste('state', 1:m), each = q-1)
          }))),
          level = 'subject',
          subject = factor(paste('subject', i))
        ) %>%
          cbind(median_emiss, mean_emiss)
        if(ci){
        ci_emiss <- lapply(model$emiss_int_subj[[i]], function(x) {
          apply(x[(burn_in + 1):J, ], 2, stats::quantile, quantiles) %>%
            matrix(byrow = TRUE, nrow = m) %>%
            t() %>%
            tibble::as_tibble()
        }) %>%
          dplyr::bind_rows()
          all_emiss_i <- all_emiss_i %>%
            cbind(ci_emiss)
        }
        if(ess){
          ess_bulk_emiss_int <- lapply(model$emiss_int_subj[[i]], function(x) {
        apply(x[(burn_in + 1):J, ], 2, posterior::ess_bulk)
          })
          ess_bulk_emiss_int <- do.call('c', ess_bulk_emiss_int)
          ess_tail_emiss_int <- lapply(model$emiss_int_subj[[i]], function(x) {
        apply(x[(burn_in + 1):J, ], 2, posterior::ess_tail)
          })
          ess_tail_emiss_int <- do.call('c', ess_tail_emiss_int)
          all_emiss_i <- all_emiss_i %>%
            cbind(data.frame(ess_bulk = ess_bulk_emiss_int, ess_tail = ess_tail_emiss_int))
        }
        all_emiss[[i]] <-  all_emiss_i %>%
          tibble::as_tibble()
      }
      allpars <- all_emiss %>%
        dplyr::bind_rows()
      }
    }
  }
  return(allpars)
}

#' Tidy a mHMM object with varying emission distributions
#'
#' @param model The model of class `mHMM`, fit using [mHMMbayes::mHMM()]
#' @param param String, specifying the parameters to obtain a tidy summary for. Takes 'gamma' or 'emiss'
#' @param data_distr String specifying whether 'continuous' or 'categorical' variables should be returned.
#' @param level String specifying the level to obtain a tidy summary for. Takes 'group' or 'subject'
#' @param prob If `TRUE`, returns parameters on the probability scale, if FALSE, returns parameters on the logit scale.
#' @param ci Logical indicating whether credible intervals should be computed.
#' @param ess Logical indicating whether effective sample size should be computed.
#' @param quantiles Numeric vector specifying the quantiles to use to obtain credible intervals.
#' @param subjects Optional numeric vector specifying the subjects to obtain a tidy summary for. Ignored when `level = 'group'`
#' @param burn_in Optional integer values specifying the number of burnin samples to discard.
#' @param ... Additional arguments to tidying method. Currently not used
#'
#' @returns A [tibble::tibble()] with summary for the model.
#' @export
#'
#' @examples
#' \dontrun{
#' library(mHMMbayes)
#' # simulating multivariate continuous data
#' n_t <- 100
#' n <- 10
#' m <- 3
#' n_dep <- 2
#'
#' gamma <- matrix(c(
#'   0.8, 0.1, 0.1,
#'   0.2, 0.7, 0.1,
#'   0.2, 0.2, 0.6
#' ), ncol = m, byrow = TRUE)
#'
#' emiss_distr <- list(
#'   matrix(c(
#'     50, 10,
#'     100, 10,
#'     150, 10
#'   ), nrow = m, byrow = TRUE),
#'   matrix(c(
#'     5, 2,
#'     10, 5,
#'     20, 3
#'   ), nrow = m, byrow = TRUE)
#' )
#'
#' data_cont <- sim_mHMM(
#'   n_t = n_t, n = n, data_distr = "continuous",
#'   gen = list(m = m, n_dep = n_dep),
#'   gamma = gamma, emiss_distr = emiss_distr,
#'   var_gamma = .1, var_emiss = c(5^2, 0.2^2)
#' )
#'
#' # Specify hyper-prior for the continuous emission distribution
#' manual_prior_emiss <- prior_emiss_cont(
#'   gen = list(m = m, n_dep = n_dep),
#'   emiss_mu0 = list(
#'     matrix(c(30, 70, 170), nrow = 1),
#'     matrix(c(7, 8, 18), nrow = 1)
#'   ),
#'   emiss_K0 = list(1, 1),
#'   emiss_V = list(rep(5^2, m), rep(0.5^2, m)),
#'   emiss_nu = list(1, 1),
#'   emiss_a0 = list(rep(1.5, m), rep(1, m)),
#'   emiss_b0 = list(rep(20, m), rep(4, m))
#' )
#'
#' # Run the model on the simulated data:
#' # Note that for reasons of running time, J is set at a ridiculous low value.
#' # One would typically use a number of iterations J of at least 1000,
#' # and a burn_in of 200.
#' out_3st_cont_sim <- mHMM(
#'   s_data = data_cont$obs,
#'   data_distr = "continuous",
#'   gen = list(m = m, n_dep = n_dep),
#'   start_val = c(list(gamma), emiss_distr),
#'   emiss_hyp_prior = manual_prior_emiss,
#'   mcmc = list(J = 11, burn_in = 5)
#' )
#'
#' tidy_mHMM(out_3st_cont_sim)
#' }
tidy_mHMM.mHMM_vary <- function(
  model,
  param = 'gamma',
  data_distr = 'categorical',
  level = "group",
  prob = TRUE,
  ci = TRUE,
  ess = TRUE,
  quantiles = c(0.025, 0.975),
  subjects = NULL,
  burn_in = NULL,
  ...
){
  if(param == 'gamma'){
    class(model) <- c('cat', 'mHMM', 'list')
    model$input$n_dep <- sum(model$input$data_distr == 'continuous')
    model$input$dep_labels <- model$input$dep_labels[model$input$data_distr == 'continuous']
    model$input$data_distr <- 'continuous'
    tidy_mHMM(
      model = model,
      param = 'gamma',
      level = level,
      prob = prob,
      ci = ci,
      ess = ess,
      quantiles = quantiles,
      subjects = subjects,
      burn_in = burn_in
    )
  } else if(param == 'emiss'){
    if(data_distr == 'continuous'){
      class(model) <- c('cont', 'mHMM', 'list')
    model$input$n_dep <- sum(model$input$data_distr == 'continuous')
    model$input$dep_labels <- model$input$dep_labels[model$input$data_distr == 'continuous']
    model$input$data_distr <- 'continuous'
    tidy_mHMM(
      model = model,
      param = 'emiss',
      level = level,
      prob = prob,
      ci = ci,
      ess = ess,
      quantiles = quantiles,
      subjects = subjects,
      burn_in = burn_in
    )
    } else if(data_distr == 'categorical'){
      class(model) <- c('cat', 'mHMM', 'list')
    model$input$n_dep <- sum(model$input$data_distr == 'categorical')
    model$input$dep_labels <- model$input$dep_labels[model$input$data_distr == 'categorical']
    model$input$q_emiss <- model$input$q_emiss[model$input$data_distr == 'categorical']
    model$input$data_distr <- 'categorical'
    tidy_mHMM(
      model = model,
      param = 'emiss',
      level = level,
      prob = prob,
      ci = ci,
      ess = ess,
      quantiles = quantiles,
      subjects = subjects,
      burn_in = burn_in
    )
    }
  }
}
