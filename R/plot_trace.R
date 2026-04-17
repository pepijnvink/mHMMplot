#' Create trace plots of an mHMM object
#'
#'
#' @param model An object to obtain trace plots for.
#' @param ... Additional arguments to method
#'
#' @returns A trace plot of type [[ggplot2::ggplot]].
#'
#' @export
#'
plot_trace <- function(model, ...) {
  UseMethod('plot_trace')
}

#' Plot trace plots to assess convergence for continuous data
#' of a Bayesian Multilevel Hidden Markov Model
#'
#' @param model Object or a list of objects of type `mHMMbayes::mHMM`
#' created using [mHMMbayes::mHMM()].
#' @param component Character string specifying the component to plot.
#' Takes "gamma" or "emiss".
#' @param param Character string specifying the parameter to plot
#' for the plotted component. For emission distribution, takes 'mu' to the means (or probabilities), "varmu" for between-person variances,
#' "sd" for standard deviations of normal emission distributions,
#' and "cov" for regression coefficients of covariates. For transition probabilities, takes 'int', 'prob', or 'varint'.
#' @param level Character string specifying the level of parameter to plot.
#' Takes "group" or "subject".
#' @param vrb Character string specifying the variable to plot.
#' @param subject Integer specifying the subject to plot
#' subject specific parameters for.
#' @param ... Currently not in use.
#'
#' @return Object of type `ggplot2::gg`, plotting parameter distributions.
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
#' plot_trace(
#'   model = out_3st_cont_sim,
#'   param = "gamma",
#'   level = "group",
#'   prob = TRUE
#' )
#' }
plot_trace.cont <- function(
  model,
  component = "gamma",
  param = NULL,
  level = "group",
  vrb = NULL,
  subject = NULL,
  ...
) {
  if (is.null(level)) {
    cli::cli_abort(
      c(
        "x" = "{.var level} specifying the level to plot
        has not been specified.",
        "!" = "Please specify 'group' to plot the group-level parameters,
        or 'subject' to plot the subject level parameters."
      )
    )
  }
  if (component %nin% c("gamma", "emiss")) {
    comp <- cli::cli_vec(c("gamma", "emiss"), style = list("vec-sep2" = " or "))
    cli::cli_abort(
      "Must provide {.val {comp}} to {.var component}
                   to specify the component to plot."
    )
  }
  if (level %nin% c("group", "subject")) {
    cli::cli_abort(
      "x" = "{.val {level}} is not a valid input
                   for {.var level}.",
      "i" = "Valid inputs are {.val group} or {.val subject}."
    )
  }
  if (component == 'emiss' & level == 'subject') {
    param <- 'mu'
  }
  if (component == 'gamma' & level == 'subject') {
    param <- 'prob'
  }
  if (is.null(subject) && level == "subject") {
    cli::cli_abort(
      c(
        "{.code plot_convergence} needs an indicator specifying
        the subject to plot when plotting subject-specific parameters.",
        "i" = "Please provide a subject indicator using the {.var subject}
        argument, or specify {.code level = {.val group}}."
      )
    )
  }
  if (!is.null(vrb) && component == "gamma") {
    cli::cli_warn(
      c(
        "x" = "You provided a variable to plot using {.var vrb}
        while plotting transition probabilities",
        "i" = "The variable name you provided will be ignored."
      )
    )
  }
  m <- model$input$m
  if (is.null(vrb)) {
    vrb <- model$input$dep_labels
  }
  if (!is.null(subject) && level == "group") {
    cli::cli_warn(
      c(
        "You provided a subject indicator while plotting
        group-level distributions.",
        "i" = "The subject indicator you provided will be ignored."
      )
    )
  }
  if (component == "emiss") {
    vrb_ind <- which(vrb %in% model$input$dep_labels)
    dep_labels <- vrb
    if (is.null(param)) {
      param <- "mu"
    } else if (level == "group") {
      allowed <- c("mu", "varmu", "cov", "sd")
      if (param %nin% allowed) {
        allowed_vec <- cli::cli_vec(
          allowed,
          style = list(
            "vec-last" = ", or ",
            "vec-sep2" = " or "
          )
        )
        cli::cli_abort(
          c(
            "x" = "{.val {param}} is not a valid value
            for {.var param} for your type of data.",
            "i" = "You want to plot a {data_distr}
            variable at the group level.",
            "i" = "Allowed values for {.var param} are:
            {.val {allowed_vec}} (or NULL)."
          )
        )
      }
    } else {
      param <- "mu"
    }
    if (level == 'group') {
      param_comb <- paste0('emiss_', param, '_bar')
      output <- model[[param_comb]][vrb] %>%
        lapply(tibble::as_tibble)
      output <- do.call(cbind, output) %>%
        dplyr::rename_with(
          ~ paste0(rep(vrb, each = m), '_', param, '_state_', 1:m)
        )
    } else {
      ncont <- length(model$input$dep_labels)
      model$PD_subj[[subject]]$cont_emiss <- model$PD_subj[[
        subject
      ]]$cont_emiss[, 1:(m * ncont)]
      colnames(model$PD_subj[[subject]]$cont_emiss) <- paste0(
        rep(model$input$dep_labels, each = m),
        '_mu_state_',
        1:m
      )
      output <- model$PD_subj[[subject]]$cont_emiss %>%
        tibble::as_tibble() %>%
        dplyr::select(tidyselect::starts_with(vrb))
    }
    output_long <- output %>%
      dplyr::mutate(iter = 1:dplyr::n()) %>%
      tidyr::pivot_longer(
        -'iter',
        values_to = 'value',
        names_to = c('vrb', 'state'),
        names_pattern = paste0('(\\w+)_', param, '_state_(\\d+)')
      ) %>%
      dplyr::mutate(
        state = factor(.data$state, levels = 1:m, labels = paste('state', 1:m)),
        vrb = factor(.data$vrb, labels = dep_labels)
      )
    gg <- output_long %>%
      ggplot2::ggplot(ggplot2::aes(
        x = .data$iter,
        y = .data$value
      )) +
      ggplot2::geom_line() +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$state),
        cols = ggplot2::vars(.data$vrb)
      )
  } else {
    if (level == 'group') {
      subject <- NULL
    }
    allowed <- c("int", "varint", "prob")
    if (param %nin% allowed) {
      allowed_vec <- cli::cli_vec(
        allowed,
        style = list(
          "vec-last" = ", or ",
          "vec-sep2" = " or "
        )
      )
      cli::cli_abort(
        c(
          "x" = "{.val {param}} is not a valid value
            for {.var param} for the desired component.",
          "i" = "Allowed values for {.var param} are:
            {.val {allowed_vec}}."
        )
      )
    }
    gg <- plot_trace_gamma(model, m, param = param, level, subject = subject)
  }
  return(gg)
}

#' Plot trace plots to assess convergence for categorical data
#' of a Bayesian Multilevel Hidden Markov Model
#'
#' @param model Object or a list of objects of type `mHMMbayes::mHMM`
#' created using [mHMMbayes::mHMM()].
#' @param component Character string specifying the component to plot.
#' Takes "gamma" or "emiss".
#' @param param Optional character string specifying the parameter to plot
#' for the plotted component. Takes 'int' or 'varint'.
#' @param level Character string specifying the level of parameter to plot.
#' Takes "group" or "subject".
#' @param vrb Optional character string specifying the variable to plot
#' when plotting categorical emission distributions.
#' @param subject Integer specifying the subject to plot
#' subject specific parameters for.
#' @param ... Currently not in use.
#'
#' @return Object of type `ggplot2::gg`, plotting parameter distributions.
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
#' plot_trace(
#'   model = out_3st_cont_sim,
#'   param = "gamma",
#'   level = "group",
#'   prob = TRUE
#' )
#' }
plot_trace.cat <- function(
  model,
  component = "gamma",
  param = 'int',
  level = "group",
  vrb = NULL,
  subject = NULL,
  ...
) {
  if (is.null(level)) {
    cli::cli_abort(
      c(
        "x" = "{.var level} specifying the level to plot
        has not been specified.",
        "!" = "Please specify 'group' to plot the group-level parameters,
        or 'subject' to plot the subject level parameters."
      )
    )
  }
  if (component %nin% c("gamma", "emiss")) {
    comp <- cli::cli_vec(c("gamma", "emiss"), style = list("vec-sep2" = " or "))
    cli::cli_abort(
      "Must provide {.val {comp}} to {.var component}
                   to specify the component to plot."
    )
  }
  if (param %nin% c("int", "prob", 'varint')) {
    comp <- cli::cli_vec(
      c("int", "prob", 'varint'),
      style = list("vec-sep2" = " or ", 'vec-last' = ', or ')
    )
    cli::cli_abort(
      "Must provide {.val {comp}} to {.var param}
                   to specify the parameter to plot."
    )
  }
  if (param == 'prob') {
    prob = TRUE
  } else {
    prob = FALSE
  }
  if (level %nin% c("group", "subject")) {
    cli::cli_abort(
      "x" = "{.val {level}} is not a valid input
                   for {.var level}.",
      "i" = "Valid inputs are {.val group} or {.val subject}."
    )
  }
  if (is.null(subject) && level == "subject") {
    cli::cli_abort(
      c(
        "{.code plot_convergence} needs an indicator specifying
        the subject to plot when plotting subject-specific parameters.",
        "i" = "Please provide a subject indicator using the {.var subject}
        argument, or specify {.code level = {.val group}}."
      )
    )
  }
  if (!is.null(vrb) && component == "gamma") {
    cli::cli_warn(
      c(
        "x" = "You provided a variable to plot using {.var vrb}
        while plotting transition probabilities",
        "i" = "The variable name you provided will be ignored."
      )
    )
  }
  if (is.null(vrb) && component != "gamma") {
    cli::cli_warn(
      c(
        "x" = "You did not provide a variable to plot using {.var vrb}",
        "i" = "Please provide a variable name."
      )
    )
  }
  m <- model$input$m
  dep_labels <- model$input$dep_labels
  if (is.null(vrb)) {
    vrb <- dep_labels[1]
  } else {
    if (vrb %nin% dep_labels) {
      comp <- cli::cli_vec(
        dep_labels,
        style = list("vec-sep2" = " and ", 'vec-last' = ', and ')
      )
      cli::cli_abort(c(
        'x' = 'The variable name you provided for {.var vrb} was not used to build the model',
        'i' = 'Valid values for {.var vrb} are {.val {comp}}'
      ))
    }
  }
  vrb_ind <- NULL
  if (!is.null(subject) && level == "group") {
    cli::cli_warn(
      c(
        "You provided a subject indicator while plotting
        group-level distributions.",
        "i" = "The subject indicator you provided will be ignored."
      )
    )
  }

  if (component == "emiss") {
    vrb_ind <- which(model$input$dep_labels == vrb)
    q_vrb <- model$input$q_emiss[vrb_ind]
    if (level == 'group') {
      if (prob) {
        output <- model$emiss_prob_bar[[vrb]] %>%
          tibble::as_tibble()
        output_long <- output %>%
          dplyr::mutate(iter = 1:dplyr::n()) %>%
          tidyr::pivot_longer(
            -'iter',
            values_to = 'value',
            names_to = c('category', 'state'),
            names_pattern = 'int_Emiss(\\d+)_S(\\d+)'
          ) %>%
          dplyr::mutate(
            state = factor(
              .data$state,
              levels = 1:m,
              labels = paste('state', 1:m)
            ),
            category = factor(
              .data$category,
              levels = 1:q_vrb,
              labels = paste('category', 1:q_vrb)
            )
          )
      } else {
        if (param == 'int') {
          output <- model$emiss_int_bar[[vrb]] %>%
            tibble::as_tibble()
          output_long <- output %>%
            dplyr::mutate(iter = 1:dplyr::n()) %>%
            tidyr::pivot_longer(
              -'iter',
              values_to = 'value',
              names_to = c('category', 'state'),
              names_pattern = 'int_Emiss(\\d+)_S(\\d+)'
            ) %>%
            dplyr::mutate(
              state = factor(
                .data$state,
                levels = 1:m,
                labels = paste('state', 1:m)
              ),
              category = factor(
                .data$category,
                levels = 2:q_vrb,
                labels = paste('category', 2:q_vrb)
              )
            )
        } else {
          output <- model$emiss_V_int_bar[[vrb]] %>%
            tibble::as_tibble()
          output_long <- output %>%
            dplyr::mutate(iter = 1:dplyr::n()) %>%
            tidyr::pivot_longer(
              -'iter',
              values_to = 'value',
              names_to = c('category', 'state'),
              names_pattern = 'var_int_Emiss(\\d+)_S(\\d+)'
            ) %>%
            dplyr::mutate(
              state = factor(
                .data$state,
                levels = 1:m,
                labels = paste('state', 1:m)
              ),
              category = factor(
                .data$category,
                levels = 2:q_vrb,
                labels = paste('category', 2:q_vrb)
              )
            )
        }
      }
    } else {
      if (prob) {
        ncat <- length(model$input$dep_labels)
        dep_cat <- model$input$dep_labels
        q_emiss <- model$input$q_emiss
        if (vrb_ind == 1) {
          vrb_col_indices <- 1:(q_vrb * m)
        } else {
          cumul_categories <- sum(q_emiss[1:(vrb_ind - 1)])
          vrb_col_indices <- (cumul_categories * m + 1):((cumul_categories *
            m +
            q_vrb))
        }
        output <- model$PD_subj[[subject]]$cat_emiss[, vrb_col_indices] %>%
          tibble::as_tibble()
        output_long <- output %>%
          dplyr::mutate(iter = 1:dplyr::n()) %>%
          tidyr::pivot_longer(
            -'iter',
            values_to = 'value',
            names_to = c('state', 'category'),
            names_pattern = 'dep\\d+_S(\\d+)_emiss(\\d+)'
          ) %>%
          dplyr::mutate(
            state = factor(
              .data$state,
              levels = 1:m,
              labels = paste('state', 1:m)
            ),
            category = factor(
              .data$category,
              levels = 1:q_vrb,
              labels = paste('category', 1:q_vrb)
            )
          )
      } else {
        output <- model$emiss_int_subj[[subject]][[vrb]] %>%
          tibble::as_tibble()
        output_long <- output %>%
          dplyr::mutate(iter = 1:dplyr::n()) %>%
          tidyr::pivot_longer(
            -'iter',
            values_to = 'value',
            names_to = c('category', 'state'),
            names_pattern = 'int_Emiss(\\d+)_S(\\d+)'
          ) %>%
          dplyr::mutate(
            state = factor(
              .data$state,
              levels = 1:m,
              labels = paste('state', 1:m)
            ),
            category = factor(
              .data$category,
              levels = 2:q_vrb,
              labels = paste('category', 2:q_vrb)
            )
          )
      }
    }
    gg <- output_long %>%
      ggplot2::ggplot(ggplot2::aes(
        x = .data$iter,
        y = .data$value
      )) +
      ggplot2::geom_line() +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$state),
        cols = ggplot2::vars(.data$category)
      )
  } else {
    if (level == 'group') {
      subject <- NULL
    }
    if (is.null(param)) {
      param <- 'int'
    }
    gg <- plot_trace_gamma(model, m, param = param, level, subject = subject)
  }
  return(gg)
}

#' @keywords internal
# trace plot for gamma (group level)
plot_trace_gamma <- function(model, m, param = 'int', level, subject = NULL) {
  if (param == 'prob') {
    if (level == 'group') {
      output <- model$gamma_prob_bar %>%
        tibble::as_tibble()
    } else {
      output <- model$PD_subj[[subject]]$trans_prob %>%
        tibble::as_tibble(
          .name_repair = ~ vctrs::vec_as_names(
            names = paste0('S', rep(1:m, each = m), 'toS', rep(1:m, times = m)),
            quiet = TRUE
          )
        )
    }
    output_long <- output %>%
      dplyr::mutate(iter = 1:dplyr::n()) %>%
      tidyr::pivot_longer(
        -'iter',
        names_to = c('From', 'To'),
        names_pattern = 'S(\\d+)toS(\\d+)',
        values_to = 'prob'
      ) %>%
      dplyr::mutate(
        From = factor(
          .data$From,
          levels = 1:m,
          labels = paste('From State', 1:m)
        ),
        To = factor(.data$To, levels = 1:m, labels = paste('To State', 1:m))
      )
    gg <- output_long %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$iter, y = .data$prob)) +
      ggplot2::geom_line() +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$From),
        cols = ggplot2::vars(.data$To)
      )
  } else if (param == 'int') {
    if (level == 'group') {
      output <- model$gamma_int_bar %>%
        tibble::as_tibble()
    } else {
      output <- model$gamma_int_subj[[subject]] %>%
        tibble::as_tibble()
    }
    output_long <- output %>%
      dplyr::mutate(iter = 1:dplyr::n()) %>%
      tidyr::pivot_longer(
        -'iter',
        names_to = c('From', 'To'),
        names_pattern = 'int_S(\\d+)toS(\\d+)',
        values_to = 'prob'
      ) %>%
      dplyr::mutate(
        From = factor(
          .data$From,
          levels = 1:m,
          labels = paste('From State', 1:m)
        ),
        To = factor(.data$To, levels = 2:m, labels = paste('To State', 2:m))
      )
    gg <- output_long %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$iter, y = .data$prob)) +
      ggplot2::geom_line() +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$From),
        cols = ggplot2::vars(.data$To)
      )
  } else if (param == 'varint') {
    output <- model$gamma_V_int_bar %>%
      tibble::as_tibble()
    output_long <- output %>%
      dplyr::mutate(iter = 1:dplyr::n()) %>%
      tidyr::pivot_longer(
        -'iter',
        names_to = c('From', 'To'),
        names_pattern = 'var_int_S(\\d+)toS(\\d+)',
        values_to = 'prob'
      ) %>%
      dplyr::mutate(
        From = factor(
          .data$From,
          levels = 1:m,
          labels = paste('From State', 1:m)
        ),
        To = factor(.data$To, levels = 2:m, labels = paste('To State', 2:m))
      )
    gg <- output_long %>%
      ggplot2::ggplot(ggplot2::aes(x = .data$iter, y = .data$prob)) +
      ggplot2::geom_line() +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$From),
        cols = ggplot2::vars(.data$To)
      )
  }
  return(gg)
}

#' Plot trace plots to assess convergence for data of varying types
#' of a Bayesian Multilevel Hidden Markov Model
#'
#' @param model Object or a list of objects of type `mHMMbayes::mHMM`
#' created using [mHMMbayes::mHMM()].
#' @param data_distr String specifying the data type to plot. Takes 'categorical' or 'continuous'.
#' @param component Character string specifying the component to plot.
#' Takes "gamma" or "emiss".
#' @param param Optional character string specifying the parameter to plot
#' for the plotted component.
#' If `NULL` (default) or 'mu', plots the means (or probabilities).
#' Takes "varmu" for between-person variances,
#' "sd" for standard deviations of normal emission distributions,
#' and "cov" for regression coefficients of covariates.
#' @param level Character string specifying the level of parameter to plot.
#' Takes "group" or "subject".
#' @param vrb Character string specifying the variable to plot.
#' @param subject Integer specifying the subject to plot
#' subject specific parameters for.
#' @param ... Currently not in use
#'
#' @return Object of type `ggplot2::gg`, plotting parameter distributions.
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
#' plot_trace(
#'   model = out_3st_cont_sim,
#'   param = "gamma",
#'   level = "group",
#'   prob = TRUE
#' )
#' }
plot_trace.mHMM_vary <- function(
  model,
  data_distr = 'categorical',
  component = "gamma",
  param = NULL,
  level = "group",
  vrb = NULL,
  subject = NULL,
  ...
) {
  if (component == 'gamma') {
    class(model) <- c('cont', 'mHMM', 'list')
    model$input$n_dep <- sum(model$input$data_distr == 'continuous')
    model$input$dep_labels <- model$input$dep_labels[
      model$input$data_distr == 'continuous'
    ]
    model$input$data_distr <- 'continuous'
    plot_trace(
      model = model,
      component = component,
      param = param,
      level = level,
      vrb = vrb,
      subject = subject
    )
  } else if (component == 'emiss') {
    if (data_distr == 'continuous') {
      class(model) <- c('cont', 'mHMM', 'list')
      model$input$n_dep <- sum(model$input$data_distr == 'continuous')
      model$input$dep_labels <- model$input$dep_labels[
        model$input$data_distr == 'continuous'
      ]
      model$input$data_distr <- 'continuous'
      plot_trace(
        model = model,
        component = component,
        param = param,
        level = level,
        vrb = vrb,
        subject = subject
      )
    } else if (data_distr == 'categorical') {
      class(model) <- c('cat', 'mHMM', 'list')
      model$input$n_dep <- sum(model$input$data_distr == 'categorical')
      model$input$dep_labels <- model$input$dep_labels[
        model$input$data_distr == 'categorical'
      ]
      model$input$q_emiss <- model$input$q_emiss[
        model$input$data_distr == 'categorical'
      ]
      model$input$data_distr <- 'categorical'
      if (is.null(vrb)) {
        vrb <- model$input$dep_labels[1]
      }
      plot_trace(
        model = model,
        component = component,
        level = level,
        param = param,
        vrb = vrb,
        subject = subject
      )
    }
  }
}

#' Plot trace plots to assess convergence for continuous data
#' of a Bayesian Multilevel Hidden Markov Model
#'
#' @param model Object of type `mHMM_list_cont`
#' created using [mHMMplot::mHMM_list()].
#' @param component Character string specifying the component to plot.
#' Takes "gamma" or "emiss".
#' @param param Character string specifying the parameter to plot
#' for the plotted component. For emission distribution, takes 'mu' to the means (or probabilities), "varmu" for between-person variances,
#' "sd" for standard deviations of normal emission distributions,
#' and "cov" for regression coefficients of covariates. For transition probabilities, takes 'int', 'prob', or 'varint'.
#' @param level Character string specifying the level of parameter to plot.
#' Takes "group" or "subject".
#' @param vrb Character string specifying the variable to plot.
#' @param subject Integer specifying the subject to plot
#' subject specific parameters for.
#' @param alpha alpha value to set the transparency
#' @param ... Currently not in use.
#'
#' @return Object of type `ggplot2::gg`, plotting parameter distributions.
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
#' plot_trace(
#'   model = out_3st_cont_sim,
#'   param = "gamma",
#'   level = "group",
#'   prob = TRUE
#' )
#' }
plot_trace.mHMM_list_cont <- function(
  model,
  component = "gamma",
  param = NULL,
  level = "group",
  vrb = NULL,
  subject = NULL,
  alpha = 0.5,
  ...
) {
  if (is.null(level)) {
    cli::cli_abort(
      c(
        "x" = "{.var level} specifying the level to plot
        has not been specified.",
        "!" = "Please specify 'group' to plot the group-level parameters,
        or 'subject' to plot the subject level parameters."
      )
    )
  }
  if (component %nin% c("gamma", "emiss")) {
    comp <- cli::cli_vec(c("gamma", "emiss"), style = list("vec-sep2" = " or "))
    cli::cli_abort(
      "Must provide {.val {comp}} to {.var component}
                   to specify the component to plot."
    )
  }
  if (level %nin% c("group", "subject")) {
    cli::cli_abort(
      "x" = "{.val {level}} is not a valid input
                   for {.var level}.",
      "i" = "Valid inputs are {.val group} or {.val subject}."
    )
  }
  if (component == 'emiss' & level == 'subject') {
    param <- 'mu'
  }
  if (component == 'gamma' & level == 'subject') {
    param <- 'prob'
  }
  if (is.null(subject) && level == "subject") {
    cli::cli_abort(
      c(
        "{.code plot_convergence} needs an indicator specifying
        the subject to plot when plotting subject-specific parameters.",
        "i" = "Please provide a subject indicator using the {.var subject}
        argument, or specify {.code level = {.val group}}."
      )
    )
  }
  if (!is.null(vrb) && component == "gamma") {
    cli::cli_warn(
      c(
        "x" = "You provided a variable to plot using {.var vrb}
        while plotting transition probabilities",
        "i" = "The variable name you provided will be ignored."
      )
    )
  }
  m <- model[[1]]$input$m
  nchains <- length(model)
  if (is.null(vrb)) {
    vrb <- model[[1]]$input$dep_labels
  }
  if (!is.null(subject) && level == "group") {
    cli::cli_warn(
      c(
        "You provided a subject indicator while plotting
        group-level distributions.",
        "i" = "The subject indicator you provided will be ignored."
      )
    )
  }
  if (component == "emiss") {
    vrb_ind <- which(vrb %in% model[[1]]$input$dep_labels)
    if (is.null(param)) {
      param <- "mu"
    } else if (level == "group") {
      allowed <- c("mu", "varmu", "cov", "sd")
      if (param %nin% allowed) {
        allowed_vec <- cli::cli_vec(
          allowed,
          style = list(
            "vec-last" = ", or ",
            "vec-sep2" = " or "
          )
        )
        cli::cli_abort(
          c(
            "x" = "{.val {param}} is not a valid value
            for {.var param} for your type of data.",
            "i" = "You want to plot a {data_distr}
            variable at the group level.",
            "i" = "Allowed values for {.var param} are:
            {.val {allowed_vec}} (or NULL)."
          )
        )
      }
    } else {
      param <- "mu"
    }
    if (level == 'group') {
      param_comb <- paste0('emiss_', param, '_bar')
      output <- lapply(model, function(x) {
        x[[param_comb]][vrb] %>%
          lapply(tibble::as_tibble) %>%
          dplyr::bind_cols(.name_repair = 'minimal') %>%
          stats::setNames(paste0(
            rep(vrb, each = m),
            '_',
            param,
            '_state_',
            1:m
          ))
      }) %>%
        dplyr::bind_rows(.id = 'chain')
    } else {
      ncont <- length(model[[1]]$input$dep_labels)
      for (i in 1:nchains) {
        model[[i]]$PD_subj[[subject]]$cont_emiss <- model[[i]]$PD_subj[[
          subject
        ]]$cont_emiss[, 1:(m * ncont)]
        colnames(model[[i]]$PD_subj[[subject]]$cont_emiss) <- paste0(
          rep(model[[1]]$input$dep_labels, each = m),
          '_mu_state_',
          1:m
        )
      }
      output <- lapply(model, function(x) {
        x$PD_subj[[subject]]$cont_emiss %>%
          tibble::as_tibble() %>%
          dplyr::select(tidyselect::starts_with(paste0(vrb, '_')))
      }) %>%
        dplyr::bind_rows(.id = 'chain')
    }
    output_long <- output %>%
      dplyr::group_by(.data$chain) %>%
      dplyr::mutate(iter = 1:dplyr::n()) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_longer(
        -c('iter', 'chain'),
        values_to = 'value',
        names_to = c('vrb', 'state'),
        names_pattern = paste0('(\\w+)_', param, '_state_(\\d+)')
      ) %>%
      dplyr::mutate(
        state = factor(.data$state, levels = 1:m, labels = paste('state', 1:m)),
        vrb = factor(.data$vrb),
        chain = factor(.data$chain, labels = paste('chain', 1:nchains))
      )
    gg <- output_long %>%
      ggplot2::ggplot(ggplot2::aes(
        x = .data$iter,
        y = .data$value,
        color = .data$chain
      )) +
      ggplot2::geom_line(alpha = alpha) +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$state),
        cols = ggplot2::vars(.data$vrb)
      )
  } else {
    if (level == 'group') {
      subject <- NULL
    }
    allowed <- c("int", "varint", "prob")
    if (param %nin% allowed) {
      allowed_vec <- cli::cli_vec(
        allowed,
        style = list(
          "vec-last" = ", or ",
          "vec-sep2" = " or "
        )
      )
      cli::cli_abort(
        c(
          "x" = "{.val {param}} is not a valid value
            for {.var param} for the desired component.",
          "i" = "Allowed values for {.var param} are:
            {.val {allowed_vec}}."
        )
      )
    }
    gg <- plot_trace_gamma_list(
      model,
      m,
      param = param,
      level,
      subject = subject,
      nchains = nchains,
      alpha = alpha
    )
  }
  return(gg)
}

#' Plot trace plots to assess convergence for categorical data
#' of a Bayesian Multilevel Hidden Markov Model
#'
#' @param model Object of type `mHMM_list_cat`
#' created using [mHMMplot::mHMM_list()].
#' @param component Character string specifying the component to plot.
#' Takes "gamma" or "emiss".
#' @param param Optional character string specifying the parameter to plot
#' for the plotted component. Takes 'int' or 'varint'.
#' @param level Character string specifying the level of parameter to plot.
#' Takes "group" or "subject".
#' @param vrb Optional character string specifying the variable to plot
#' when plotting categorical emission distributions.
#' @param subject Integer specifying the subject to plot
#' subject specific parameters for.
#' @param alpha alpha value to set the transparency
#' @param ... Currently not in use.
#'
#' @return Object of type `ggplot2::gg`, plotting parameter distributions.
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
#' plot_trace(
#'   model = out_3st_cont_sim,
#'   param = "gamma",
#'   level = "group",
#'   prob = TRUE
#' )
#' }
plot_trace.mHMM_list_cat <- function(
  model,
  component = "gamma",
  param = 'int',
  level = "group",
  vrb = NULL,
  subject = NULL,
  alpha = 0.5,
  ...
) {
  if (is.null(level)) {
    cli::cli_abort(
      c(
        "x" = "{.var level} specifying the level to plot
        has not been specified.",
        "!" = "Please specify 'group' to plot the group-level parameters,
        or 'subject' to plot the subject level parameters."
      )
    )
  }
  if (component %nin% c("gamma", "emiss")) {
    comp <- cli::cli_vec(c("gamma", "emiss"), style = list("vec-sep2" = " or "))
    cli::cli_abort(
      "Must provide {.val {comp}} to {.var component}
                   to specify the component to plot."
    )
  }
  if (param %nin% c("int", "prob", 'varint')) {
    comp <- cli::cli_vec(
      c("int", "prob", 'varint'),
      style = list("vec-sep2" = " or ", 'vec-last' = ', or ')
    )
    cli::cli_abort(
      "Must provide {.val {comp}} to {.var param}
                   to specify the parameter to plot."
    )
  }
  if (param == 'prob') {
    prob = TRUE
  } else {
    prob = FALSE
  }
  if (level %nin% c("group", "subject")) {
    cli::cli_abort(
      "x" = "{.val {level}} is not a valid input
                   for {.var level}.",
      "i" = "Valid inputs are {.val group} or {.val subject}."
    )
  }
  if (is.null(subject) && level == "subject") {
    cli::cli_abort(
      c(
        "{.code plot_convergence} needs an indicator specifying
        the subject to plot when plotting subject-specific parameters.",
        "i" = "Please provide a subject indicator using the {.var subject}
        argument, or specify {.code level = {.val group}}."
      )
    )
  }
  if (!is.null(vrb) && component == "gamma") {
    cli::cli_warn(
      c(
        "x" = "You provided a variable to plot using {.var vrb}
        while plotting transition probabilities",
        "i" = "The variable name you provided will be ignored."
      )
    )
  }
  if (is.null(vrb) && component != "gamma") {
    cli::cli_warn(
      c(
        "x" = "You did not provide a variable to plot using {.var vrb}",
        "i" = "Please provide a variable name."
      )
    )
  }
  m <- model[[1]]$input$m
  dep_labels <- model[[1]]$input$dep_labels
  nchains <- length(model)
  if (is.null(vrb)) {
    vrb <- dep_labels[1]
  } else {
    if (vrb %nin% dep_labels) {
      comp <- cli::cli_vec(
        dep_labels,
        style = list("vec-sep2" = " and ", 'vec-last' = ', and ')
      )
      cli::cli_abort(c(
        'x' = 'The variable name you provided for {.var vrb} was not used to build the model',
        'i' = 'Valid values for {.var vrb} are {.val {comp}}'
      ))
    }
  }
  vrb_ind <- NULL
  if (!is.null(subject) && level == "group") {
    cli::cli_warn(
      c(
        "You provided a subject indicator while plotting
        group-level distributions.",
        "i" = "The subject indicator you provided will be ignored."
      )
    )
  }

  if (component == "emiss") {
    vrb_ind <- which(model[[1]]$input$dep_labels == vrb)
    q_vrb <- model[[1]]$input$q_emiss[vrb_ind]
    if (level == 'group') {
      if (prob) {
        output <- model$emiss_prob_bar[[vrb]] %>%
          tibble::as_tibble()
        output <- lapply(model, function(x) {
          x$emiss_prob_bar[[vrb]] %>%
            tibble::as_tibble()
        }) %>%
          dplyr::bind_rows(.id = 'chain')
        output_long <- output %>%
          dplyr::group_by(.data$chain) %>%
          dplyr::mutate(iter = 1:dplyr::n()) %>%
          dplyr::ungroup() %>%
          tidyr::pivot_longer(
            -c('iter', 'chain'),
            values_to = 'value',
            names_to = c('category', 'state'),
            names_pattern = 'int_Emiss(\\d+)_S(\\d+)'
          ) %>%
          dplyr::mutate(
            state = factor(
              .data$state,
              levels = 1:m,
              labels = paste('state', 1:m)
            ),
            category = factor(
              .data$category,
              levels = 1:q_vrb,
              labels = paste('category', 1:q_vrb)
            ),
            chain = factor(.data$chain, labels = paste('chain', 1:nchains))
          )
      } else {
        if (param == 'int') {
          output <- lapply(model, function(x) {
            x$emiss_int_bar[[vrb]] %>%
              tibble::as_tibble()
          }) %>%
            dplyr::bind_rows(.id = 'chain')
          output_long <- output %>%
            dplyr::group_by(.data$chain) %>%
            dplyr::mutate(iter = 1:dplyr::n()) %>%
            dplyr::ungroup() %>%
            tidyr::pivot_longer(
              -c('iter', 'chain'),
              values_to = 'value',
              names_to = c('category', 'state'),
              names_pattern = 'int_Emiss(\\d+)_S(\\d+)'
            ) %>%
            dplyr::mutate(
              state = factor(
                .data$state,
                levels = 1:m,
                labels = paste('state', 1:m)
              ),
              category = factor(
                .data$category,
                levels = 2:q_vrb,
                labels = paste('category', 2:q_vrb)
              ),
              chain = factor(.data$chain, labels = paste('chain', 1:nchains))
            )
        } else {
          output <- lapply(model, function(x) {
            x$emiss_V_int_bar[[vrb]] %>%
              tibble::as_tibble()
          }) %>%
            dplyr::bind_rows(.id = 'chain')
          output_long <- output %>%
            dplyr::group_by(.data$chain) %>%
            dplyr::mutate(iter = 1:dplyr::n()) %>%
            dplyr::ungroup() %>%
            tidyr::pivot_longer(
              -c('iter', 'chain'),
              values_to = 'value',
              names_to = c('category', 'state'),
              names_pattern = 'var_int_Emiss(\\d+)_S(\\d+)'
            ) %>%
            dplyr::mutate(
              state = factor(
                .data$state,
                levels = 1:m,
                labels = paste('state', 1:m)
              ),
              category = factor(
                .data$category,
                levels = 2:q_vrb,
                labels = paste('category', 2:q_vrb)
              ),
              chain = factor(.data$chain, labels = paste('chain', 1:nchains))
            )
        }
      }
    } else {
      if (prob) {
        ncat <- length(model[[1]]$input$dep_labels)
        dep_cat <- model[[1]]$input$dep_labels
        q_emiss <- model[[1]]$input$q_emiss
        if (vrb_ind == 1) {
          vrb_col_indices <- 1:(q_vrb * m)
        } else {
          cumul_categories <- sum(q_emiss[1:(vrb_ind - 1)])
          vrb_col_indices <- (cumul_categories * m + 1):((cumul_categories *
            m +
            q_vrb))
        }
        output <- lapply(model, function(x) {
          x$PD_subj[[subject]]$cat_emiss[, vrb_col_indices] %>%
            tibble::as_tibble()
        }) %>%
          dplyr::bind_rows(.id = 'chain')
        output_long <- output %>%
          dplyr::group_by(.data$chain) %>%
          dplyr::mutate(iter = 1:dplyr::n()) %>%
          dplyr::ungroup() %>%
          tidyr::pivot_longer(
            -c('iter', 'chain'),
            values_to = 'value',
            names_to = c('state', 'category'),
            names_pattern = 'dep\\d+_S(\\d+)_emiss(\\d+)'
          ) %>%
          dplyr::mutate(
            state = factor(
              .data$state,
              levels = 1:m,
              labels = paste('state', 1:m)
            ),
            category = factor(
              .data$category,
              levels = 1:q_vrb,
              labels = paste('category', 1:q_vrb)
            ),
            chain = factor(.data$chain, labels = paste('chain', 1:nchains))
          )
      } else {
        output <- lapply(model, function(x) {
          x$emiss_int_subj[[subject]][[vrb]] %>%
            tibble::as_tibble()
        }) %>%
          dplyr::bind_rows(.id = 'chain')
        output_long <- output %>%
          dplyr::group_by(.data$chain) %>%
          dplyr::mutate(iter = 1:dplyr::n()) %>%
          dplyr::ungroup() %>%
          tidyr::pivot_longer(
            -c('iter', 'chain'),
            values_to = 'value',
            names_to = c('category', 'state'),
            names_pattern = 'int_Emiss(\\d+)_S(\\d+)'
          ) %>%
          dplyr::mutate(
            state = factor(
              .data$state,
              levels = 1:m,
              labels = paste('state', 1:m)
            ),
            category = factor(
              .data$category,
              levels = 2:q_vrb,
              labels = paste('category', 2:q_vrb)
            ),
            chain = factor(.data$chain, labels = paste('chain', 1:nchains))
          )
      }
    }
    gg <- output_long %>%
      ggplot2::ggplot(ggplot2::aes(
        x = .data$iter,
        y = .data$value,
        color = .data$chain
      )) +
      ggplot2::geom_line(alpha = alpha) +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$state),
        cols = ggplot2::vars(.data$category)
      )
  } else {
    if (level == 'group') {
      subject <- NULL
    }
    if (is.null(param)) {
      param <- 'int'
    }
    gg <- plot_trace_gamma_list(
      model,
      m,
      param = param,
      level,
      subject = subject,
      nchains = nchains,
      alpha = alpha
    )
  }
  return(gg)
}

#' @keywords internal
# trace plot for gamma (group level)
plot_trace_gamma_list <- function(
  model,
  m,
  param = 'int',
  level,
  subject = NULL,
  nchains,
  alpha
) {
  if (param == 'prob') {
    if (level == 'group') {
      output <- lapply(model, function(x) {
        x$gamma_prob_bar %>%
          tibble::as_tibble()
      }) %>%
        dplyr::bind_rows(.id = 'chain')
    } else {
      output <- lapply(model, function(x) {
        x$PD_subj[[subject]]$trans_prob %>%
          tibble::as_tibble(
            .name_repair = ~ vctrs::vec_as_names(
              names = paste0(
                'S',
                rep(1:m, each = m),
                'toS',
                rep(1:m, times = m)
              ),
              quiet = TRUE
            )
          )
      }) %>%
        dplyr::bind_rows(.id = 'chain')
    }
    output_long <- output %>%
      dplyr::group_by(.data$chain) %>%
      dplyr::mutate(iter = 1:dplyr::n()) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_longer(
        -c('iter', 'chain'),
        names_to = c('From', 'To'),
        names_pattern = 'S(\\d+)toS(\\d+)',
        values_to = 'prob'
      ) %>%
      dplyr::mutate(
        From = factor(
          .data$From,
          levels = 1:m,
          labels = paste('From State', 1:m)
        ),
        To = factor(.data$To, levels = 1:m, labels = paste('To State', 1:m)),
        chain = factor(.data$chain, labels = paste('chain', 1:nchains))
      )
    gg <- output_long %>%
      ggplot2::ggplot(ggplot2::aes(
        x = .data$iter,
        y = .data$prob,
        color = .data$chain
      )) +
      ggplot2::geom_line(alpha = alpha) +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$From),
        cols = ggplot2::vars(.data$To)
      )
  } else if (param == 'int') {
    if (level == 'group') {
      output <- lapply(model, function(x) {
        x$gamma_int_bar %>%
          tibble::as_tibble()
      }) %>%
        dplyr::bind_rows(.id = 'chain')
    } else {
      output <- lapply(model, function(x) {
        x$gamma_int_subj[[subject]] %>%
          tibble::as_tibble()
      }) %>%
        dplyr::bind_rows(.id = 'chain')
    }
    output_long <- output %>%
      dplyr::group_by(.data$chain) %>%
      dplyr::mutate(iter = 1:dplyr::n()) %>%
      dplyr::ungroup() %>%
      tidyr::pivot_longer(
        -c('iter', 'chain'),
        names_to = c('From', 'To'),
        names_pattern = 'int_S(\\d+)toS(\\d+)',
        values_to = 'prob'
      ) %>%
      dplyr::mutate(
        From = factor(
          .data$From,
          levels = 1:m,
          labels = paste('From State', 1:m)
        ),
        To = factor(.data$To, levels = 2:m, labels = paste('To State', 2:m)),
        chain = factor(.data$chain, labels = paste('chain', 1:nchains))
      )
    gg <- output_long %>%
      ggplot2::ggplot(ggplot2::aes(
        x = .data$iter,
        y = .data$prob,
        color = .data$chain
      )) +
      ggplot2::geom_line(alpha = alpha) +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$From),
        cols = ggplot2::vars(.data$To)
      )
  } else if (param == 'varint') {
    output <- lapply(model, function(x) {
      x$gamma_V_int_bar %>%
        tibble::as_tibble()
    }) %>%
      dplyr::bind_rows(.id = 'chain')
    output_long <- output %>%
      dplyr::group_by(.data$chain) %>%
      dplyr::mutate(iter = 1:dplyr::n()) %>%
      tidyr::pivot_longer(
        -c('iter', 'chain'),
        names_to = c('From', 'To'),
        names_pattern = 'var_int_S(\\d+)toS(\\d+)',
        values_to = 'prob'
      ) %>%
      dplyr::mutate(
        From = factor(
          .data$From,
          levels = 1:m,
          labels = paste('From State', 1:m)
        ),
        To = factor(.data$To, levels = 2:m, labels = paste('To State', 2:m)),
        chain = factor(.data$chain, labels = paste('chain', 1:nchains))
      )
    gg <- output_long %>%
      ggplot2::ggplot(ggplot2::aes(
        x = .data$iter,
        y = .data$prob,
        color = .data$chain
      )) +
      ggplot2::geom_line(alpha = alpha) +
      ggplot2::facet_grid(
        rows = ggplot2::vars(.data$From),
        cols = ggplot2::vars(.data$To)
      )
  }
  return(gg)
}

#' Plot trace plots to assess convergence for data of varying types
#' of a Bayesian Multilevel Hidden Markov Model
#'
#' @param model Object or a list of objects of type `mHMMbayes::mHMM`
#' created using [mHMMbayes::mHMM()].
#' @param data_distr String specifying the data type to plot. Takes 'categorical' or 'continuous'.
#' @param component Character string specifying the component to plot.
#' Takes "gamma" or "emiss".
#' @param param Optional character string specifying the parameter to plot
#' for the plotted component.
#' If `NULL` (default) or 'mu', plots the means (or probabilities).
#' Takes "varmu" for between-person variances,
#' "sd" for standard deviations of normal emission distributions,
#' and "cov" for regression coefficients of covariates.
#' @param level Character string specifying the level of parameter to plot.
#' Takes "group" or "subject".
#' @param vrb Character string specifying the variable to plot.
#' @param subject Integer specifying the subject to plot
#' subject specific parameters for.
#' @param alpha alpha value to set the transparency
#' @param ... Currently not in use
#'
#' @return Object of type `ggplot2::gg`, plotting parameter distributions.
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
#' plot_trace(
#'   model = out_3st_cont_sim,
#'   param = "gamma",
#'   level = "group",
#'   prob = TRUE
#' )
#' }
plot_trace.mHMM_list_vary <- function(
  model,
  data_distr = 'categorical',
  component = "gamma",
  param = NULL,
  level = "group",
  vrb = NULL,
  subject = NULL,
  alpha = 0.5,
  ...
) {
    nchains <- length(model)
  if (component == 'gamma') {
    class(model) <- c('mHMM_list_cont', 'mHMM_list', 'list')
    for (i in 1:nchains) {
      model[[i]]$input$n_dep <- sum(model[[i]]$input$data_distr == 'continuous')
      model[[i]]$input$dep_labels <- model[[i]]$input$dep_labels[
        model[[i]]$input$data_distr == 'continuous'
      ]
      model[[i]]$input$data_distr <- 'continuous'
    }
    plot_trace(
      model = model,
      component = component,
      param = param,
      level = level,
      vrb = vrb,
      subject = subject
    )
  } else if (component == 'emiss') {
    if (data_distr == 'continuous') {
      class(model) <- c('mHMM_list_cont', 'mHMM_list', 'list')
      for (i in 1:nchains) {
        model[[i]]$input$n_dep <- sum(
          model[[i]]$input$data_distr == 'continuous'
        )
        model[[i]]$input$dep_labels <- model[[i]]$input$dep_labels[
          model[[i]]$input$data_distr == 'continuous'
        ]
        model[[i]]$input$data_distr <- 'continuous'
      }
      plot_trace(
        model = model,
        component = component,
        param = param,
        level = level,
        vrb = vrb,
        subject = subject
      )
    } else if (data_distr == 'categorical') {
      class(model) <- c('mHMM_list_cat', 'mHMM_list', 'list')
      for (i in 1:nchains) {
        model[[i]]$input$n_dep <- sum(
          model[[i]]$input$data_distr == 'categorical'
        )
        model[[i]]$input$dep_labels <- model[[i]]$input$dep_labels[
          model[[i]]$input$data_distr == 'categorical'
        ]
        model[[i]]$input$q_emiss <- model[[i]]$input$q_emiss[
          model[[i]]$input$data_distr == 'categorical'
        ]
        model[[i]]$input$data_distr <- 'categorical'
      }
      if (is.null(vrb)) {
        vrb <- model[[1]]$input$dep_labels[1]
      }
      plot_trace(
        model = model,
        component = component,
        param = param,
        level = level,
        vrb = vrb,
        subject = subject
      )
    }
  }
}
