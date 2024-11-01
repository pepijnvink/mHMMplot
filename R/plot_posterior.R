#' Plot posterior distributions of a Bayesian Multilevel Hidden Markov Model
#'
#' @param model Object of type `mHMMbayes::mHMM` or `mHMMbayes::mHMM_vary` created using [mHMMbayes::mHMM()] or [mHMMbayes::mHMM_vary()].
#' @param component Character string specifying the component to plot. Takes "gamma" or "emiss".
#' @param vrb Character string specifying the dependent variable to plot when plotting emission distributions.
#' @param state_labels Optional character string specifying labels to use for the inferred states.
#' @param cat_labels Optional character string used to specify labels for categories when plotting emission distributions of categorical variables.
#' @param burnin Optional integer specifying number of burnin iterations. If unspecified, the number of burnin iterations specified when fitting the model is used.
#' @param alpha Transparency of densities representing subject-specific posterior densities.
#'
#' @return Object of type `ggplot2::gg` plotting posterior distributions.
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
#' plot_posterior(model = out_3st_cont_sim)
#' }
plot_posterior <- function(model,
                           component = "gamma",
                           vrb = NULL,
                           state_labels = NULL,
                           cat_labels = NULL,
                           burnin = NULL,
                           alpha = 0.1) {
  check_model(model,
              classes = c("mHMM",
                          "mHMM_vary"))
  if (component %nin% c("gamma", "emiss")) {
    comp <- cli::cli_vec(c("gamma", "emiss"), style = list("vec-sep2" = " or "))
    cli::cli_abort("Must provide {.val {comp}} to {.var component} to specify the component to plot.")
  }
  if (is.null(burnin)) {
    burnin <- model$input$burn_in
  }
  J <- model$input$J
  m <- model$input$m
  n_subj <- model$input$n_subj
  colnames_gamma <- paste0("S", rep(1:m, each = m), "toS", 1:m)
  if (is.null(state_labels)) {
    state_labels <- paste("State", 1:m)
  } else if(length(state_labels) != m){
    cli::cli_warn(
      c(
        "The number of names provided in {.var state_labels} must equal the number of states.",
        "i" = "The number of states is {m}.",
        "x" = "You provided {length(state_labels)} names.",
        "i" = "The state labels you provided will be ignored."
      )
    )
    state_labels <- paste("State", 1:m)
  }
  if (component == "gamma") {
    if (!is.null(cat_labels)) {
      cli::cli_warn(
        c("x" = "You provided category labels using {.var cat_labels} while plotting transition probabilities", "i" = "The category labels you provided will be ignored.")
      )
    }
    if(!is.null(vrb)){
      cli::cli_warn(
        c("x" = "You provided a variable to plot using {.var vrb} while plotting transition probabilities", "i" = "The variable name you provided will be ignored.")
      )
    }
    maxy <- max(sapply(1:m, function(i)
      max(sapply(1:m, function(q)
        max(stats::density(model$gamma_prob_bar[burnin:J, m * (i - 1) + q])$y)))))
    facet_labels <- paste("From", state_labels, "to...")
    names(facet_labels) <- 1:m
    data_group <- tibble::as_tibble(model$gamma_prob_bar[burnin:J, ],
                                    .name_repair = "minimal")
    data_subj <- lapply(model[["PD_subj"]], function(x) {
      x$trans_prob[burnin:J, ] %>%
        tibble::as_tibble(.name_repair = ~colnames_gamma) %>%
        tidyr::pivot_longer(
          cols = tidyselect::everything(),
          values_to = "Transition Probability",
          names_to = c("From", "To"),
          names_pattern = "S(\\d+)toS(\\d+)"
        )
    })
    data_group_long <- data_group %>%
      tidyr::pivot_longer(
        cols = tidyselect::everything(),
        values_to = "Transition Probability",
        names_to = c("From", "To"),
        names_pattern = "S(\\d+)toS(\\d+)"
      )
    gg <- ggplot2::ggplot(data = data_group_long)
    for (i in 1:n_subj) {
      gg <- gg +
        ggplot2::stat_density(
          data = data_subj[[i]],
          mapping = ggplot2::aes(x = .data$`Transition Probability`, color = .data$To),
          alpha = alpha,
          geom = "line",
          position = "identity"
        )
    }
    gg <- gg  +
      ggplot2::stat_density(
        mapping = ggplot2::aes(x = .data$`Transition Probability`, color = .data$To),
        linewidth = 1,
        geom = "line",
        position = "identity"
      ) +
      ggplot2::coord_cartesian(ylim = c(0, maxy)) +
      ggplot2::facet_wrap( ~ .data$From, labeller = ggplot2::labeller(From = facet_labels)) +
      ggplot2::scale_color_discrete(labels = state_labels)

  } else {
    vrb <- check_vrb(model,
                     vrb)
    if (inherits(model, "mHMM_vary")) {
      data_distr <- model$input$data_distr[which(model$input$dep_labels == vrb)]
    } else {
      data_distr <- model$input$data_distr
    }
    names(state_labels) <- as.character(1:m)
    dep_labels <- model$input$dep_labels
    vrb_ind <- which(dep_labels == vrb)
    if (data_distr == "categorical") {
      names_pattern <- paste0("dep", vrb_ind, "_S(\\d+)_emiss(\\d+)")
      q_emiss <- model$input$q_emiss[vrb_ind]
      if (is.null(cat_labels)) {
        cat_labels <- paste("Category", 1:q_emiss)
      } else if(length(cat_labels) != q_emiss){
        cli::cli_warn(
          c(
            "The number of names provided in {.var cat_labels} must equal the number of categories of {.val {vrb}}",
            "i" = "The number of categories is {q_emiss}.",
            "x" = "You provided {length(cat_labels)} names.",
            "i" = "The category labels you provided will be ignored."
          )
        )
        cat_labels <- paste("Category", 1:q_emiss)
      }
      maxy <- max(sapply(1:m, function(i)
        max(sapply(1:q_emiss, function(q)
          max(stats::density(model$emiss_prob_bar[[vrb]][burnin:J, q_emiss * (i -
                                                                                1) + q])$y)))))
      data_group <- tibble::as_tibble(model$emiss_prob_bar[[vrb]][burnin:J, ],
                                      .name_repair = "minimal")
      data_subj <- lapply(model[["PD_subj"]], function(x) {
        x$cat_emiss[burnin:J, ] %>%
          tibble::as_tibble(.name_repair = "minimal") %>%
          dplyr::select(tidyselect::starts_with(paste0("dep", vrb_ind, "_"))) %>%
          tidyr::pivot_longer(
            cols = tidyselect::everything(),
            values_to = "Conditional Probability",
            names_to = c("State", "Category"),
            names_pattern = names_pattern
          )
      })
      data_group_long <- data_group %>%
        tidyr::pivot_longer(
          cols = tidyselect::everything(),
          values_to = "Conditional Probability",
          names_to = c("Category", "State"),
          names_pattern = "Emiss(\\d+)_S(\\d+)"
        )
      gg <- ggplot2::ggplot(data = data_group_long)
      for (i in 1:n_subj) {
        gg <- gg +
          ggplot2::stat_density(
            data = data_subj[[i]],
            mapping = ggplot2::aes(x = .data$`Conditional Probability`, color = .data$Category),
            alpha = alpha,
            geom = "line",
            position = "identity"
          )
      }
      gg <- gg +
        ggplot2::stat_density(
          mapping = ggplot2::aes(x = .data$`Conditional Probability`, color = .data$Category),
          linewidth = 1,
          geom = "line",
          position = "identity"
        ) +
        ggplot2::coord_cartesian(ylim = c(0, maxy)) +
        ggplot2::facet_wrap( ~ .data$State, labeller = ggplot2::labeller(State = state_labels)) +
        ggplot2::scale_color_discrete(labels = cat_labels)
    } else if (data_distr == "continuous") {
      names_pattern <- paste0("dep", vrb_ind, "_S(\\d+)_([A-Za-z]+)")
      data_group_mu <- tibble::as_tibble(model$emiss_mu_bar[[vrb]][burnin:J, ],
                                         .name_repair = "minimal")
      data_group_var <- tibble::as_tibble(model$emiss_sd_bar[[vrb]][burnin:J, ],
                                          .name_repair = "minimal")
      data_group <- cbind(data_group_mu, data_group_var)
      data_group_long <- data_group %>%
        tidyr::pivot_longer(
          cols = tidyselect::everything(),
          values_to = "Value",
          names_to = c("Parameter", "State"),
          names_pattern = c("([A-Za-z]+)_(\\d+)")
        )
      data_subj <- lapply(model[["PD_subj"]], function(x) {
        x$cont_emiss[burnin:J, ] %>%
          tibble::as_tibble(.name_repair = "minimal") %>%
          dplyr::select(tidyselect::starts_with(paste0("dep", vrb_ind, "_"))) %>%
          tidyr::pivot_longer(
            cols = tidyselect::everything(),
            values_to = "Value",
            names_to = c("State", "Parameter"),
            names_pattern = names_pattern
          )
      })
      gg <- ggplot2::ggplot(data = data_group_long)
      for (i in 1:n_subj) {
        gg <- gg + ggplot2::stat_density(
          data = data_subj[[i]],
          mapping = ggplot2::aes(x = .data$Value, color = .data$State),
          alpha = alpha,
          geom = "line",
          position = "identity"
        )
      }
      gg <- gg +
        ggplot2::stat_density(
          mapping = ggplot2::aes(x = .data$Value, color = .data$State),
          linewidth = 1,
          geom = "line",
          position = "identity"
        ) +
        ggplot2::facet_wrap( ~ .data$Parameter, scales = "free") +
        ggplot2::scale_color_discrete(labels = state_labels)
    }
  }
  gg <- gg +
    ggplot2::theme_classic()
  return(gg)
}
