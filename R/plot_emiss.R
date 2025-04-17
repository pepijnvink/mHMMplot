#' Plot emission distributions of a Bayesian Multilevel Hidden Markov Model
#'
#' @param model Object of type `mHMMbayes::mHMM` or `mHMMbayes:mHMM_vary`, created using [mHMMbayes::mHMM()] or [mHMMbayes::mHMM_vary()].
#' @param type String specifying the type of plot to return. Currently takes "bar" and "boxplot".
#' @param distr String specifying the Data Type (i.e. "categorical" or "continuous").
#' @param subject_effects Logical specifying whether a layer of individual estimates should be plotted.
#' @param cat_labels Character vector of labels for the categorical variables.
#' @param alpha Numeric value indicating transparency of subject-specific posterior densities.
#' @param position Object created with ggplot2::position_jitter indicating the amount of jitter.
#' @param line Logical indicating whether to plot lines when plotton individual-level distributions.
#'
#' @return
#' Object of type `ggplot2::gg` plotting emission distributions.
#' @export
#'
#' @examples
#' \dontrun{
#' library(mHMMbayes)
#' # simulating multivariate continuous data
#' n_t     <- 100
#' n       <- 10
#' m       <- 3
#' n_dep   <- 2
#'
#' gamma   <- matrix(c(0.8, 0.1, 0.1,
#'                     0.2, 0.7, 0.1,
#'                     0.2, 0.2, 0.6), ncol = m, byrow = TRUE)
#'
#' emiss_distr <- list(matrix(c( 50, 10,
#'                               100, 10,
#'                               150, 10), nrow = m, byrow = TRUE),
#'                     matrix(c(5, 2,
#'                              10, 5,
#'                              20, 3), nrow = m, byrow = TRUE))
#'
#' data_cont <- sim_mHMM(n_t = n_t, n = n, data_distr = 'continuous',
#'                       gen = list(m = m, n_dep = n_dep),
#'                       gamma = gamma, emiss_distr = emiss_distr,
#'                       var_gamma = .1, var_emiss = c(5^2, 0.2^2))
#'
#' # Specify hyper-prior for the continuous emission distribution
#' manual_prior_emiss <- prior_emiss_cont(
#'                         gen = list(m = m, n_dep = n_dep),
#'                         emiss_mu0 = list(matrix(c(30, 70, 170), nrow = 1),
#'                                          matrix(c(7, 8, 18), nrow = 1)),
#'                         emiss_K0 = list(1, 1),
#'                         emiss_V =  list(rep(5^2, m), rep(0.5^2, m)),
#'                         emiss_nu = list(1, 1),
#'                         emiss_a0 = list(rep(1.5, m), rep(1, m)),
#'                         emiss_b0 = list(rep(20, m), rep(4, m)))
#'
#' # Run the model on the simulated data:
#' # Note that for reasons of running time, J is set at a ridiculous low value.
#' # One would typically use a number of iterations J of at least 1000,
#' # and a burn_in of 200.
#' out_3st_cont_sim <- mHMM(s_data = data_cont$obs,
#'                          data_distr = 'continuous',
#'                          gen = list(m = m, n_dep = n_dep),
#'                          start_val = c(list(gamma), emiss_distr),
#'                          emiss_hyp_prior = manual_prior_emiss,
#'                          mcmc = list(J = 11, burn_in = 5))
#'
#' plot_emiss(out_3st_cont_sim)
#' }
plot_emiss <- function(model,
                       type = "bar",
                       distr = "continuous",
                       subject_effects = TRUE,
                       cat_labels = NULL,
                       position = ggplot2::position_jitter(width = 0.2, height = 0),
                       alpha = 0.5,
                       line = FALSE) {
  check_model(model, classes = c("mHMM", "mHMM_vary"))
  m <- model$input$m
  n_subj <- model$input$n_subj
  state_labels <- paste("State", 1:m)
  if(inherits(model, "mHMM_vary")){
    cont_vrbs_ind <- which(model$input$data_distr == distr)
    n_dep <- length(cont_vrbs_ind)
  } else {
    cont_vrbs_ind <- 1:model$input$n_dep
    n_dep <- model$input$n_dep
  }
  if (type == "bar") {
    if(inherits(model, "mHMM_vary")){
      emiss_group <- obtain_emiss_new(object = model, level = "group")[cont_vrbs_ind]
    } else {
      emiss_group <- mHMMbayes::obtain_emiss(object = model, level = "group")[cont_vrbs_ind]
    }
    if(distr == "continuous"){
    vrb <- names(emiss_group)
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
      dplyr::mutate(State = factor(.data$State, labels = state_labels))
    } else {
      emiss_group <- emiss_group[[1]]
      rownames(emiss_group) <- paste0(1:m)
      q <- ncol(emiss_group)
      if(is.null(cat_labels)){
        vrb <- paste0("Category ", 1:q)
      } else {
        vrb <- cat_labels
      }
      emiss_group_melt <- as.data.frame(emiss_group) %>%
        tibble::rownames_to_column(var = "State") %>%
        tidyr::pivot_longer(cols = -.data$State, names_to = "Dep", values_to = "Mean") %>%
        dplyr::mutate(State = factor(.data$State, labels = state_labels))
    }
    if(distr == "categorical"){
      emiss_group_melt$Dep <- factor(emiss_group_melt$Dep, labels = vrb)
    }
    gg <- ggplot2::ggplot(data = emiss_group_melt,
                          mapping = ggplot2::aes(x = .data$State, y = .data$Mean, fill = .data$Dep)) +
      ggplot2::geom_col()
    if (subject_effects) {
      if(inherits(model, "mHMM_vary")){
        emiss_subj <- obtain_emiss_new(object = model, level = "subject")[cont_vrbs_ind]
      } else {
        emiss_subj <- mHMMbayes::obtain_emiss(object = model, level = "subject")[cont_vrbs_ind]
      }
      gg_emiss_subject <- data.frame(
        Subj = rep(rep(1:n_subj, each = m), n_dep),
        State = factor(rep(1:m, n_subj * n_dep), labels = state_labels),
        Dep = factor(c(rep(
          vrb, each = m * n_subj
        )), levels = vrb)
      )
      if(distr == "continuous"){
      gg_emiss_subject$Mean <- mapply(
        function(x, y, z) {
          emiss_subj[[x]][[y]][z, 1]
        },
        x = gg_emiss_subject$Dep,
        y = gg_emiss_subject$Subj,
        z = gg_emiss_subject$State
      )} else {
        gg_emiss_subject$Mean <- mapply(
          function(x, y, z) {
            emiss_subj[[1]][[y]][z, x]
          },
          x = gg_emiss_subject$Dep,
          y = gg_emiss_subject$Subj,
          z = gg_emiss_subject$State
        )
      }
      gg <- gg +
        ggplot2::geom_jitter(
          data = gg_emiss_subject,
          mapping = ggplot2::aes(x = .data$State, y = .data$Mean, fill = .data$Dep),
          alpha = alpha,
          color = "black",
          pch = 21,
          position = position
        )
      if(line){
        gg <- gg +
          ggplot2::geom_line(
            data = gg_emiss_subject,
            mapping = ggplot2::aes(x = .data$State, y = .data$Mean, group = .data$Subj),
            alpha = alpha
          )
      }

    }
  } else if(type == "boxplot"){
    emiss_subj <- mHMMbayes::obtain_emiss(object = model, level = "subject")[cont_vrbs_ind]
    vrb <- names(emiss_subj)
    gg_emiss_subject <- data.frame(
      Subj = rep(rep(1:n_subj, each = m), n_dep),
      State = factor(rep(1:m, n_subj * n_dep), labels = state_labels),
      Dep = factor(c(rep(
        vrb, each = m * n_subj
      )), levels = vrb)
    )
    gg_emiss_subject$Mean <- mapply(
      function(x, y, z) {
        emiss_subj[[x]][[y]][z, 1]
      },
      x = gg_emiss_subject$Dep,
      y = gg_emiss_subject$Subj,
      z = gg_emiss_subject$State
    )
    gg <- ggplot2::ggplot(data = gg_emiss_subject,
                          mapping = ggplot2::aes(x = .data$State, y = .data$Mean, fill = .data$Dep)) +
      ggplot2::geom_boxplot()
  }
  gg <- gg +
    ggplot2::facet_grid(cols = ggplot2::vars(.data$Dep)) +
    ggplot2::theme_minimal() +
    ggplot2::theme(legend.position = "none") +
    ggplot2::xlab("Mood State") +
    ggplot2::guides(fill = "none",
                    color = "none") +
    ggplot2::theme_bw()
  if(distr == "categorical"){
    gg <- gg + ggplot2::ylab("Probability")
  }
  return(gg)
}
