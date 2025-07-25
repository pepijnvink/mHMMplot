#' Plot emission distributions of a Bayesian Multilevel Hidden Markov Model
#'
#' @param model Object of type `mHMMbayes::mHMM`,
#' created using [mHMMbayes::mHMM()].
#' @param type String specifying the type of plot to return.
#' Takes "bar", "point", and "boxplot".
#' @param subject_effects Logical specifying whether a layer of individual
#' estimates should be plotted.
#' @param cat_labels Character vector of labels for the categorical variables.
#' @param alpha Numeric value indicating transparency of subject-specific
#' posterior densities.
#' @param jitter Object created with ggplot2::position_jitter indicating
#' the amount of jitter.
#' @param line Logical indicating whether to plot lines when plotting
#' individual-level distributions.
#' @param subject Vector indicating the subjects to plot when
#' `subject_effects = TRUE`. Default is `NULL`, which means
#' all subjects are plotted.
#' @param vrb Optional string specifying the variable to plot when using
#' categorical data.
#' @param burn_in Optional integer specifying the number of burnin iterations.
#' @param errorbar Optional string indicating the type of error bar to use.
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
plot_emiss <- function(model,
                       type = "bar",
                       subject_effects = TRUE,
                       cat_labels = NULL,
                       jitter = ggplot2::position_jitter(
                         width = 0.2,
                         height = 0
                       ),
                       alpha = 0.75,
                       line = FALSE,
                       subject = NULL,
                       vrb = NULL,
                       burn_in = NULL,
                       errorbar = NULL) {
  check_model(model, classes = "mHMM")
  m <- model$input$m
  n_subj <- model$input$n_subj
  if (is.null(subject)) {
    subject <- 1:n_subj
  }
  if (is.null(burn_in)){
    burn_in <- model$input$burn_in
  }
  if (type == "point" & is.null(errorbar)) {
    errorbar <- "hpd"
  }
  state_labels <- paste("State", 1:m)
  distr <- model$input$data_distr
  n_dep <- model$input$n_dep
  if (type == "bar" | type == "point") {
    emiss_group <- mHMMbayes::obtain_emiss(object = model, level = "group", burn_in = burn_in)
    if (distr == "continuous") {
      facets <- names(emiss_group)
      emiss_group <- lapply(emiss_group, function(x, m) {
        rownames(x) <- paste0(1:m)
        x
      }, m = m)
      emiss_group_melt <- lapply(emiss_group, function(x) {
        x %>%
          as.data.frame() %>%
          tibble::rownames_to_column(var = "State")
      }) %>%
        dplyr::bind_rows(.id = "Dep") %>%
        dplyr::mutate(
          State = factor(.data$State, labels = state_labels),
          Dep = factor(.data$Dep, labels = facets, levels = facets)
        )
      if (!is.null(errorbar)) {
        if (errorbar == "sd") {
          emiss_group_melt$sdmu <- lapply(model$emiss_varmu_bar,
                                          function(x) {
                                            x[-(1:burn_in),] %>%
                                              apply(2, stats::median)
                                          }) %>%
            unlist() %>%
            sqrt()
          emiss_group_melt <- emiss_group_melt %>%
            dplyr::mutate(
              lower = .data$Mean - .data$sdmu,
              upper = .data$Mean + .data$sdmu
            )
          note_errorbar <- "Errorbars represent the between-person standard deviation"
        } else if (errorbar == "hpd") {
          hpd <- lapply(model$emiss_mu_bar,
                        function(x) {
                          coda::mcmc(x,
                                     start = burn_in + 1) %>%
                            coda::HPDinterval() %>%
                            as.data.frame()
                        }) %>%
            dplyr::bind_rows()
          emiss_group_melt <- cbind(emiss_group_melt, hpd)
          note_errorbar <- "Errorbars represent the 95% highest posterior density interval"
        } else {
          eti <- lapply(model$emiss_mu_bar,
                        function(x) {
                          apply(x[-(1:burn_in), ], 2, stats::quantile,
                                probs = c(0.025, 0.975)
                          ) %>%
                            t() %>%
                            as.data.frame()
                        }) %>%
            dplyr::bind_rows() %>%
            dplyr::rename_with(~c("lower", "upper"))
          emiss_group_melt <- cbind(emiss_group_melt, eti)
          note_errorbar <- "Errorbars represent the 95% equal-tailed interval"
        }
      }
    } else if (distr == "categorical") {
      if(is.null(vrb)){
        vrb <- model$input$dep_labels[[1]]
      }
      emiss_group <- emiss_group[[vrb]]
      rownames(emiss_group) <- paste0(1:m)
      q <- ncol(emiss_group)
      if (is.null(cat_labels)) {
        facets <- paste0("Category ", 1:q)
      } else {
        facets <- cat_labels
      }
      emiss_group_melt <- as.data.frame(emiss_group) %>%
        tibble::rownames_to_column(var = "State") %>%
        tidyr::pivot_longer(
          cols = -.data$State,
          names_to = "Dep",
          values_to = "Mean"
        ) %>%
        dplyr::mutate(
          State = factor(.data$State, labels = state_labels),
          Dep = factor(.data$Dep, labels = facets, levels = facets)
        )
      if (!is.null(errorbar)) {
        if (errorbar == "sd") {
          cli::cli_alert(c("x" = "Errorbars for between-person standard deviation cannot be computed for categorical data.",
                           "i" = "No error bar will be shown."))
          errorbar <- NULL
        } else if (errorbar == "hpd") {
          hpd <- model[["emiss_prob_bar"]][[vrb]] %>%
            coda::mcmc(start = burn_in+1) %>%
            coda::HPDinterval() %>%
            as.data.frame()
          emiss_group_melt <- cbind(emiss_group_melt, hpd)
          note_errorbar <- "Errorbars represent the 95% highest posterior density interval"
        } else {
          eti <- apply(model[["emiss_prob_bar"]][[vrb]], 2, stats::quantile, probs = c(0.025, 0.975)) %>%
            t() %>%
            as.data.frame() %>%
            dplyr::rename_with(~c("lower", "upper"))
          emiss_group_melt <- cbind(emiss_group_melt, eti)
          note_errorbar <- "Errorbars represent the 95% equal-tailed interval"
        }
      }
    }
    if (type == "bar") {
      gg <- ggplot2::ggplot(
        data = emiss_group_melt,
        mapping = ggplot2::aes(
          x = .data$State,
          y = .data$Mean,
          fill = .data$Dep
        )
      )
    } else if (type == "point") {
      gg <- ggplot2::ggplot(
        data = emiss_group_melt,
        mapping = ggplot2::aes(
          x = .data$State,
          y = .data$Mean
        )
      )
    }
    if(type == "bar") {
      gg <- gg +
        ggplot2::geom_col()
    }
    if (subject_effects) {
      emiss_subj <- mHMMbayes::obtain_emiss(object = model, level = "subject", burn_in = burn_in)
      if (distr == "continuous") {
        gg_emiss_subject <- data.frame(
          Subj = rep(rep(subject, each = m), n_dep),
          State = factor(rep(1:m, length(subject) * n_dep),
                         labels = state_labels
          ),
          Dep = factor(c(rep(
            facets,
            each = m * length(subject)
          )), levels = facets)
        )
        gg_emiss_subject$Mean <- mapply(
          function(x, y, z) {
            emiss_subj[[x]][[y]][z, 1]
          },
          x = gg_emiss_subject$Dep,
          y = gg_emiss_subject$Subj,
          z = gg_emiss_subject$State
        )
      } else if (distr == "categorical"){
        gg_emiss_subject <- data.frame(
          Subj = rep(rep(subject, each = m), q),
          State = factor(rep(1:m, length(subject) * q),
                         labels = state_labels
          ),
          Dep = factor(c(rep(
            facets,
            each = m * length(subject)
          )), levels = facets)
        )
        gg_emiss_subject$Mean <- mapply(
          function(x, y, z) {
            emiss_subj[[vrb]][[y]][z, x]
          },
          x = gg_emiss_subject$Dep,
          y = gg_emiss_subject$Subj,
          z = gg_emiss_subject$State
        )
      }
      gg <- gg +
        ggplot2::geom_jitter(
          data = gg_emiss_subject,
          mapping = ggplot2::aes(
            x = .data$State,
            y = .data$Mean,
            fill = .data$Dep
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
              x = .data$State,
              y = .data$Mean,
              group = .data$Subj
            ),
            alpha = alpha,
            color = "grey"
          )
      }
    }
    if (!is.null(errorbar)) {
      gg <- gg +
        ggplot2::geom_point(size = 4) +
        ggplot2::geom_segment(ggplot2::aes(y = .data$lower, yend = .data$upper), linewidth = 2, lineend = "round") +
        ggplot2::labs(caption = note_errorbar)
    }
  } else if (type == "boxplot") {
    emiss_subj <- mHMMbayes::obtain_emiss(object = model, level = "subject")
    facets <- names(emiss_subj)
    gg_emiss_subject <- data.frame(
      Subj = rep(rep(1:n_subj, each = m), n_dep),
      State = factor(rep(1:m, n_subj * n_dep), labels = state_labels),
      Dep = factor(c(rep(
        facets,
        each = m * n_subj
      )), levels = facets)
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
  gg <- gg +
    ggplot2::facet_grid(cols = ggplot2::vars(.data$Dep)) +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab("Mood State") +
    ggplot2::guides(fill = "none", color = "none") +
    theme_mhmm() +
    scale_color_mhmm(which = "fill")
  if (distr == "categorical") {
    gg <- gg + ggplot2::ylab("Probability")
  }
  return(gg)
}
