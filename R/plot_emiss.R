#' Plot emission distribution of an mHMM object
#'
#'
#' @param model An object to plot emission distributions for.
#' @param ... Additional arguments to tidying method
#'
#' @returns A plot of type [[ggplot2::ggplot]] visualizing the emission distribution.
#'
#' @export
#'
plot_emiss <- function(model, ...) {
  UseMethod('plot_emiss')
}

#' Plot emission distributions of a Bayesian Multilevel Hidden Markov Model for continuous data.
#'
#' @param model Object of type `mHMMbayes::mHMM`,
#' created using [mHMMbayes::mHMM()].
#' @param type String specifying the type of plot to return.
#' Takes "bar", "point", and "boxplot".
#' @param subject_effects Logical specifying whether a layer of individual
#' estimates should be plotted.
#' @param line Logical indicating whether to plot lines when plotting
#' individual-level distributions.
#' @param subject Vector indicating the subjects to plot when
#' `subject_effects = TRUE`. Default is `NULL`, which means
#' all subjects are plotted.
#' @param facet String specifying the dimension to facet. Takes 'state' (default) or 'vrb'.
#' @param errorbar Optional string indicating the type of error bar to use.
#' @param errorbar_prob Optional scalar between 0 and 1 indicating the confidence level to create errorbars for. Only used when `errorbar` is equal to `ci` or `hpd`.
#' @param alpha Numeric value indicating transparency of subject-specific
#' posterior densities.
#' @param jitter Object created with ggplot2::position_jitter indicating
#' the amount of jitter.
#' @param burn_in Optional integer specifying the number of burnin iterations.
#' @param ... Currently not used
#'
#' @return
#' Object of type `ggplot2::gg` plotting emission distributions.
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
#' plot_emiss(out_3st_cont_sim)
#' }
plot_emiss.cont <- function(
  model,
  type = "bar",
  subject_effects = TRUE,
  line = FALSE,
  subject = NULL,
  facet = 'state',
  errorbar = 'ci',
  errorbar_prob = 0.95,
  alpha = 0.3,
  jitter = ggplot2::position_jitter(
    width = 0.2,
    height = 0
  ),
  burn_in = NULL,
  ...
) {
  check_model(model, classes = "mHMM")
  m <- model$input$m
  n_subj <- model$input$n_subj
  if (is.null(subject)) {
    subject <- 1:n_subj
  }
  if (is.null(burn_in)) {
    burn_in <- model$input$burn_in
  }
  if (!is.null(errorbar)) {
    if (errorbar %nin% c('sd', 'ci')) {
      cli::cli_abort(c(
        "Argument {.var errorbar} takes 'sd' or 'ci'",
        'x' = 'Invalid entry for argument {.var errorbar}',
        'i' = 'Please specify a different value'
      ))
    }
    if ((length(errorbar_prob) != 1)) {
      cli::cli_abort(c(
        'Argument {.var errorbar} should be a single number between 0 and 1',
        'x' = 'You specified a vector',
        'i' = 'Please specify a different value'
      ))
    }
    if (!is.numeric(errorbar_prob)) {
      cli::cli_abort(c(
        'Argument {.var errorbar} should be a single number between 0 and 1',
        'x' = 'You specified a value of class {.cls {class(errorbar_prob)}}',
        'i' = 'Please specify a numeric value'
      ))
    }
    if (errorbar_prob <= 0 | errorbar_prob >= 1) {
      cli::cli_abort(c(
        'Argument {.var errorbar} should be a number between 0 and 1',
        'x' = 'You specified a value outside of these bounds',
        'i' = 'Please specify a numeric value between 0 and 1'
      ))
    }
  }
  if (type == "point" & is.null(errorbar)) {
    errorbar <- "ci"
  }
  if (errorbar == 'ci') {
    request_ci = TRUE
    bounds_errorbar <- c((1 - errorbar_prob) / 2, (1 - (1 - errorbar_prob) / 2))
  } else {
    request_ci = FALSE
    bounds_errorbar <- NULL
  }
  state_labels <- paste("State", 1:m)
  distr <- model$input$data_distr
  n_dep <- model$input$n_dep
  if (type == "bar" | type == "point") {
    emiss_group_melt <- tidy_mHMM(
      model,
      param = 'emiss',
      ci = request_ci,
      ess = FALSE,
      quantiles = bounds_errorbar,
      burn_in = burn_in
    )
    emiss_group_mu <- emiss_group_melt %>%
      dplyr::filter(.data$param == 'mu')
    if (!is.null(errorbar)) {
      if (errorbar == "sd") {
        emiss_group_sdmu <- emiss_group_melt %>%
          dplyr::filter(.data$param == 'sdmu') %>%
          dplyr::pull(.data$median)
        emiss_group_mu <- emiss_group_mu %>%
          dplyr::select('vrb', 'state', 'median') %>%
          dplyr::rename(mean = 'median') %>%
          dplyr::mutate(
            lower = .data$mean - emiss_group_sdmu,
            upper = .data$mean + emiss_group_sdmu
          )
        note_errorbar <- "Errorbars represent the between-person standard deviation"
      } else if (errorbar == 'ci') {
        emiss_group_mu <- emiss_group_mu %>%
          dplyr::select(-c('param', 'level', 'mean')) %>%
          dplyr::rename_with(~ c('vrb', 'state', 'mean', 'lower', 'upper'))
        note_errorbar <- paste0(
          "Errorbars represent the ",
          errorbar_prob * 100,
          "% credible interval"
        )
      }
    } else {
      emiss_group_mu <- emiss_group_mu %>%
        dplyr::select('vrb', 'state', 'median') %>%
        dplyr::rename(mean = .data$median)
    }
    if (type == "bar") {
      if (facet == 'state') {
        gg <- ggplot2::ggplot(
          data = emiss_group_mu,
          mapping = ggplot2::aes(
            x = .data$vrb,
            y = .data$mean,
            fill = .data$vrb
          )
        )
      } else if (facet == 'vrb') {
        gg <- ggplot2::ggplot(
          data = emiss_group_mu,
          mapping = ggplot2::aes(
            x = .data$state,
            y = .data$mean,
            fill = .data$vrb
          )
        )
      }
    } else if (type == "point") {
      if (facet == 'state') {
        gg <- ggplot2::ggplot(
          data = emiss_group_mu,
          mapping = ggplot2::aes(
            x = .data$vrb,
            y = .data$mean
          )
        )
      } else if (facet == 'vrb') {
        gg <- ggplot2::ggplot(
          data = emiss_group_mu,
          mapping = ggplot2::aes(
            x = .data$state,
            y = .data$mean
          )
        )
      }
    }
    if (type == "bar") {
      gg <- gg +
        ggplot2::geom_col()
    }
    if (subject_effects) {
      gg_emiss_subject <- tidy_mHMM(
        model,
        param = 'emiss',
        level = 'subject',
        ci = request_ci,
        ess = FALSE,
        quantiles = bounds_errorbar,
        subject = subject,
        burn_in = burn_in
      )
      if (facet == 'state') {
        gg <- gg +
          ggplot2::geom_jitter(
            data = gg_emiss_subject,
            mapping = ggplot2::aes(
              x = .data$vrb,
              y = .data$mean,
              fill = .data$vrb
            ),
            alpha = alpha,
            color = "black",
            pch = 21,
            position = jitter,
            size = 3
          )
        if (line) {
          gg <- gg +
            ggplot2::geom_line(
              data = gg_emiss_subject,
              mapping = ggplot2::aes(
                x = .data$vrb,
                y = .data$mean,
                group = .data$subject
              ),
              alpha = alpha,
              color = "grey"
            )
        }
      } else if (facet == 'vrb') {
        gg <- gg +
          ggplot2::geom_jitter(
            data = gg_emiss_subject,
            mapping = ggplot2::aes(
              x = .data$state,
              y = .data$mean,
              fill = .data$vrb
            ),
            alpha = alpha,
            color = "black",
            pch = 21,
            position = jitter,
            size = 3
          )
        if (line) {
          gg <- gg +
            ggplot2::geom_line(
              data = gg_emiss_subject,
              mapping = ggplot2::aes(
                x = .data$state,
                y = .data$mean,
                group = .data$subject
              ),
              alpha = alpha,
              color = "grey"
            )
        }
      }
    }
    if (!is.null(errorbar)) {
      gg <- gg +
        ggplot2::geom_point(size = 4) +
        ggplot2::geom_segment(
          ggplot2::aes(y = .data$lower, yend = .data$upper),
          linewidth = 2.5,
          lineend = "round"
        ) +
        ggplot2::geom_segment(
          ggplot2::aes(y = .data$lower, yend = .data$upper, color = .data$vrb),
          linewidth = 1.5,
          lineend = "round"
        ) +
        ggplot2::geom_point(size = 3, ggplot2::aes(color = .data$vrb)) +
        ggplot2::labs(caption = note_errorbar)
    }
  } else if (type == "boxplot") {
    emiss_subj <- mHMMbayes::obtain_emiss(object = model, level = "subject")
    vrb_labels <- names(emiss_subj)
    gg_emiss_subject <- data.frame(
      Subj = rep(rep(1:n_subj, each = m), n_dep),
      State = factor(rep(1:m, n_subj * n_dep), labels = state_labels),
      Dep = factor(
        c(rep(
          vrb_labels,
          each = m * n_subj
        )),
        levels = vrb_labels
      )
    )
    gg_emiss_subject$Mean <- mapply(
      function(x, y, z) {
        emiss_subj[[x]][[y]][z, 1]
      },
      x = gg_emiss_subject$Dep,
      y = gg_emiss_subject$Subj,
      z = gg_emiss_subject$State
    )
    gg <- ggplot2::ggplot(
      data = gg_emiss_subject,
      mapping = ggplot2::aes(
        x = .data$State,
        y = .data$Mean,
        fill = .data$Dep
      )
    ) +
      ggplot2::geom_boxplot()
  }
  if (facet == 'state') {
    gg <- gg +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$state))
  } else {
    gg <- gg +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$vrb))
  }
  gg <- gg +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab("Mood State") +
    ggplot2::guides(fill = "none", color = "none")
  if (distr == "categorical") {
    gg <- gg + ggplot2::ylab("Probability")
  }
  return(gg)
}

#' Plot emission distributions of a Bayesian Multilevel Hidden Markov Model for categorical data.
#'
#' @param model Object of type `mHMMbayes::mHMM`,
#' created using [mHMMbayes::mHMM()].
#' @param type String specifying the type of plot to return.
#' Takes "bar", "point", and "boxplot".
#' @param subject_effects Logical specifying whether a layer of individual
#' estimates should be plotted.
#' @param cat_labels Character vector of labels for the categorical variables.
#' @param line Logical indicating whether to plot lines when plotting
#' individual-level distributions.
#' @param subject Vector indicating the subjects to plot when
#' `subject_effects = TRUE`. Default is `NULL`, which means
#' all subjects are plotted.
#' @param vrb Optional string specifying the variable to plot when using
#' categorical data. If not specified, it plots the first variable.
#' @param facet String specifying the dimension to facet. Takes 'state' (default) or 'vrb'.
#' @param errorbar Logical indicating whether to include errorbars.
#' @param errorbar_prob Optional scalar between 0 and 1 indicating the confidence level to create errorbars for. Only used when `errorbar` is equal to `ci` or `hpd`.
#' @param alpha Numeric value indicating transparency of subject-specific
#' posterior densities.
#' @param jitter Object created with ggplot2::position_jitter indicating
#' the amount of jitter.
#' @param burn_in Optional integer specifying the number of burnin iterations.
#' @param ... Currently not used
#'
#' @return
#' Object of type `ggplot2::gg` plotting emission distributions.
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
#' plot_emiss(out_3st_cont_sim)
#' }
plot_emiss.cat <- function(
  model,
  type = "bar",
  subject_effects = TRUE,
  cat_labels = NULL,
  line = FALSE,
  subject = NULL,
  vrb = NULL,
  facet = 'state',
  errorbar = TRUE,
  errorbar_prob = 0.95,
  alpha = 0.3,
  jitter = ggplot2::position_jitter(
    width = 0.2,
    height = 0
  ),
  burn_in = NULL,
  ...
) {
  check_model(model, classes = "mHMM")
  m <- model$input$m
  n_subj <- model$input$n_subj
  if (is.null(subject)) {
    subject <- 1:n_subj
  }
  if (is.null(burn_in)) {
    burn_in <- model$input$burn_in
  }
  if (inherits(errorbar, 'character')) {
    cli::cli_abort(c(
      'Argument {.var errorbar} should be a logical indicating whether to plot the credible interval',
      'x' = 'You specified a string',
      'i' = 'Please specify a logical'
    ))
  }
  if (errorbar) {
    if ((length(errorbar_prob) != 1)) {
      cli::cli_abort(c(
        'Argument {.var errorbar} should be a single number between 0 and 1',
        'x' = 'You specified a vector',
        'i' = 'Please specify a different value'
      ))
    }
    if (!is.numeric(errorbar_prob)) {
      cli::cli_abort(c(
        'Argument {.var errorbar} should be a single number between 0 and 1',
        'x' = 'You specified a value of class {.cls {class(errorbar_prob)}}',
        'i' = 'Please specify a numeric value'
      ))
    }
    if (errorbar_prob <= 0 | errorbar_prob >= 1) {
      cli::cli_abort(c(
        'Argument {.var errorbar} should be a number between 0 and 1',
        'x' = 'You specified a value outside of these bounds',
        'i' = 'Please specify a numeric value between 0 and 1'
      ))
    }
  }
  if (type == "point" & is.null(errorbar)) {
    errorbar <- TRUE
  }
  if (errorbar) {
    request_ci <- TRUE
    bounds_errorbar <- c((1 - errorbar_prob) / 2, (1 - (1 - errorbar_prob) / 2))
  } else {
    request_ci <- FALSE
    bounds_errorbar <- NULL
  }
  state_labels <- paste("State", 1:m)
  distr <- model$input$data_distr
  n_dep <- model$input$n_dep
  if (type == "bar" | type == "point") {
    emiss_group_melt <- tidy_mHMM(
      model,
      param = 'emiss',
      level = 'group',
      ci = request_ci,
      ess = FALSE,
      quantiles = bounds_errorbar,
      burn_in = burn_in
    )
    emiss_group_mu <- emiss_group_melt %>%
      dplyr::filter(.data$param == 'emiss_prob')
    if (errorbar) {
      emiss_group_mu <- emiss_group_mu %>%
        dplyr::select(-c('param', 'level', 'mean')) %>%
        dplyr::rename_with(
          ~ c('vrb', 'category', 'state', 'mean', 'lower', 'upper')
        )
      note_errorbar <- paste0(
        "Errorbars represent the ",
        errorbar_prob * 100,
        "% credible interval"
      )
    } else {
      emiss_group_mu <- emiss_group_mu %>%
        dplyr::select('category', 'state', 'median') %>%
        dplyr::rename(mean = .data$median)
    }
    if (type == "bar") {
      if (facet == 'state') {
        gg <- ggplot2::ggplot(
          data = emiss_group_mu,
          mapping = ggplot2::aes(
            x = .data$category,
            y = .data$mean,
            fill = .data$category
          )
        )
      } else if (facet == 'category') {
        gg <- ggplot2::ggplot(
          data = emiss_group_mu,
          mapping = ggplot2::aes(
            x = .data$state,
            y = .data$mean,
            fill = .data$category
          )
        )
      }
    } else if (type == "point") {
      if (facet == 'state') {
        gg <- ggplot2::ggplot(
          data = emiss_group_mu,
          mapping = ggplot2::aes(
            x = .data$category,
            y = .data$mean
          )
        )
      } else if (facet == 'category') {
        gg <- ggplot2::ggplot(
          data = emiss_group_mu,
          mapping = ggplot2::aes(
            x = .data$state,
            y = .data$mean
          )
        )
      }
    }
    if (type == "bar") {
      gg <- gg +
        ggplot2::geom_col()
    }
    if (subject_effects) {
      gg_emiss_subject <- tidy_mHMM(
        model,
        param = 'emiss',
        level = 'subject',
        burn_in = burn_in,
        ess = FALSE,
        ci = FALSE,
        subject = subject
      )
      if (facet == 'state') {
        gg <- gg +
          ggplot2::geom_jitter(
            data = gg_emiss_subject,
            mapping = ggplot2::aes(
              x = .data$category,
              y = .data$mean,
              fill = .data$category
            ),
            alpha = alpha,
            color = "black",
            pch = 21,
            position = jitter,
            size = 3
          )
        if (line) {
          gg <- gg +
            ggplot2::geom_line(
              data = gg_emiss_subject,
              mapping = ggplot2::aes(
                x = .data$category,
                y = .data$mean,
                group = .data$subject
              ),
              alpha = alpha,
              color = "grey"
            )
        }
      } else if (facet == 'category') {
        gg <- gg +
          ggplot2::geom_jitter(
            data = gg_emiss_subject,
            mapping = ggplot2::aes(
              x = .data$state,
              y = .data$mean,
              fill = .data$category
            ),
            alpha = alpha,
            color = "black",
            pch = 21,
            position = jitter,
            size = 3
          )
        if (line) {
          gg <- gg +
            ggplot2::geom_line(
              data = gg_emiss_subject,
              mapping = ggplot2::aes(
                x = .data$state,
                y = .data$mean,
                group = .data$subject
              ),
              alpha = alpha,
              color = "grey"
            )
        }
      }
    }
    if (errorbar) {
      gg <- gg +
        ggplot2::geom_point(size = 4) +
        ggplot2::geom_segment(
          ggplot2::aes(y = .data$lower, yend = .data$upper),
          linewidth = 2.5,
          lineend = "round"
        ) +
        ggplot2::geom_segment(
          ggplot2::aes(
            y = .data$lower,
            yend = .data$upper,
            color = .data$category
          ),
          linewidth = 1.5,
          lineend = "round"
        ) +
        ggplot2::geom_point(size = 3, ggplot2::aes(color = .data$category)) +
        ggplot2::labs(caption = note_errorbar)
    }
  } else if (type == "boxplot") {
    emiss_subj <- mHMMbayes::obtain_emiss(object = model, level = "subject")
    vrb_labels <- names(emiss_subj)
    gg_emiss_subject <- data.frame(
      Subj = rep(rep(1:n_subj, each = m), n_dep),
      State = factor(rep(1:m, n_subj * n_dep), labels = state_labels),
      Dep = factor(
        c(rep(
          vrb_labels,
          each = m * n_subj
        )),
        levels = vrb_labels
      )
    )
    gg_emiss_subject$Mean <- mapply(
      function(x, y, z) {
        emiss_subj[[x]][[y]][z, 1]
      },
      x = gg_emiss_subject$Dep,
      y = gg_emiss_subject$Subj,
      z = gg_emiss_subject$State
    )
    gg <- ggplot2::ggplot(
      data = gg_emiss_subject,
      mapping = ggplot2::aes(
        x = .data$State,
        y = .data$Mean,
        fill = .data$Dep
      )
    ) +
      ggplot2::geom_boxplot()
  }
  if (facet == 'state') {
    gg <- gg +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$state))
  } else {
    gg <- gg +
      ggplot2::facet_grid(cols = ggplot2::vars(.data$category))
  }
  gg <- gg +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab("Mood State") +
    ggplot2::guides(fill = "none", color = "none")
  if (distr == "categorical") {
    gg <- gg + ggplot2::ylab("Probability")
  }
  return(gg)
}

#' Plot emission distributions of a Bayesian Multilevel Hidden Markov Model for varying data types.
#'
#' @param model Object of type `mHMMbayes::mHMM`,
#' created using [mHMMbayes::mHMM()].
#' @param data_distr String specifying the data type to plot. Takes 'categorical' or 'continuous.
#' @param type String specifying the type of plot to return.
#' Takes "bar", "point", and "boxplot".
#' @param subject_effects Logical specifying whether a layer of individual
#' estimates should be plotted.
#' @param cat_labels Character vector of labels for the categorical variables.
#' @param line Logical indicating whether to plot lines when plotting
#' individual-level distributions.
#' @param subject Vector indicating the subjects to plot when
#' `subject_effects = TRUE`. Default is `NULL`, which means
#' all subjects are plotted.
#' @param vrb Optional string specifying the variable to plot when using
#' categorical data. If not specified, it plots the first variable.
#' @param facet String specifying the dimension to facet. Takes 'state' (default) or 'vrb'.
#' @param errorbar Optional string indicating the type of error bar to use.
#' @param errorbar_prob Optional scalar between 0 and 1 indicating the confidence level to create errorbars for. Only used when `errorbar` is equal to `ci` or `hpd`.
#' @param alpha Numeric value indicating transparency of subject-specific
#' posterior densities.
#' @param jitter Object created with ggplot2::position_jitter indicating
#' the amount of jitter.
#' @param burn_in Optional integer specifying the number of burnin iterations.
#' @param ... Currently not in use.
#'
#' @return
#' Object of type `ggplot2::gg` plotting emission distributions.
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
#' plot_emiss(out_3st_cont_sim)
#' }
plot_emiss.mHMM_vary <- function(
  model,
  data_distr = 'categorical',
  type = "bar",
  subject_effects = TRUE,
  cat_labels = NULL,
  line = FALSE,
  subject = NULL,
  vrb = NULL,
  facet = 'state',
  errorbar = 'ci',
  errorbar_prob = 0.95,
  alpha = 0.3,
  jitter = ggplot2::position_jitter(
    width = 0.2,
    height = 0
  ),
  burn_in = NULL,
  ...
) {
  if (data_distr == 'continuous') {
    class(model) <- c('mHMM', 'cont')
    model$input$n_dep <- sum(model$input$data_distr == 'continuous')
    model$input$dep_labels <- model$input$dep_labels[
      model$input$data_distr == 'continuous'
    ]
    model$input$data_distr <- 'continuous'
    plot_emiss(
      model = model,
      type = type,
      subject_effects = subject_effects,
      line = line,
      subject = subject,
      facet = facet,
      errorbar = errorbar,
      errorbar_prob = errorbar_prob,
      alpha = alpha,
      jitter = jitter,
      burn_in = burn_in
    )
  } else if (data_distr == 'categorical') {
    class(model) <- c('mHMM', 'cat')
    model$input$n_dep <- sum(model$input$data_distr == 'categorical')
    model$input$dep_labels <- model$input$dep_labels[
      model$input$data_distr == 'categorical'
    ]
    model$input$q_emiss <- model$input$q_emiss[
      model$input$data_distr == 'categorical'
    ]
    model$input$data_distr <- 'categorical'
    if (errorbar == 'ci') {
      errorbar <- TRUE
    } else {
      errorbar = FALSE
    }
    plot_emiss(
      model = model,
      type = type,
      subject_effects = subject_effects,
      cat_labels = cat_labels,
      line = line,
      subject = subject,
      vrb = vrb,
      facet = facet,
      errorbar = errorbar,
      errorbar_prob = errorbar_prob,
      alpha = alpha,
      jitter = jitter,
      burn_in = burn_in
    )
  }
}
