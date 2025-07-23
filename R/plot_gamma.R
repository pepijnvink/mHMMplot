#' Heat Plot for Transition Probability Matrix of a Bayesian Multilevel Hidden Markov Model
#'
#' @param model Object of type `mHMMbayes::mHMM` or `mHMMbayes::mHMM_gamma`, created using [mHMMbayes::mHMM()] or [mHMMbayes::obtain_gamma()].
#' @param level String specifying the level of transition distributions to plot. Options are "group" and "subject".
#' @param subject Optional integer or integer vector specifying the subject(s) to plot.
#' @param digits Integer specifying the number of digits to round to.
#' @param facet Logical specifying whether subjects should be faceted when plotting subject-specific transition probability matrices.
#' @param ncol_facet Integer specifying the number of columns in the facet grid when plotting subject-specific transition probability matrices.
#'
#' @return
#' Object of type `ggplot2::gg` with the visualized transition distributions.
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
#' plot_gamma(out_3st_cont_sim)
#' }
plot_gamma <- function(model = NULL,
                       level = "group",
                       subject = NULL,
                       digits = 2,
                       facet = TRUE,
                       ncol_facet = 2) {
  check_model(
    model,
    classes = c("mHMM", "mHMM_gamma"),
    fns = c("mHMM", "obtain_gamma")
  )
  if (inherits(model, "mHMM")) {
    m <- model$input$m
    nsubj <- model$input$n_subj
    if (is.null(level) | !(level %in% c("group", "subject"))) {
      cli::cli_abort(
        c(
          "{.var level} specifying the level to plot has not been specified.",
          "i" = "Please specify 'group' to plot the group-level parameters, or 'subject' to plot the subject level parameters."
        )
      )
    }
    gamma_matrix <- mHMMbayes::obtain_gamma(object = model, level = level)
  } else {
    gamma_matrix <- model
    if (inherits(gamma_matrix, "list")) {
      if(level == "group"){
        cli::cli_warn(c("The specified level is {.val group}, but you provided subject-specific transition probability matrices.",
                        "i" = "Subject-specific transition probabilities will be plotted.",
                        "i" = "To plot group level transition probabilities, provide an object of {.cls mHMMbayes::mHMM} or a group level transition probability matrix."))
      }
      level <- "subject"
      nsubj <- length(model)
      m <- nrow(model[[1]])
    } else {
      if(level == "subject"){
        cli::cli_warn(c("The specified level is {.val subject}, but you provided group-level transition probability matrices.",
                        "i" = "Group-level transition probabilities will be plotted.",
                        "i" = "To plot subject-specific transition probabilities, provide an object of {.cls mHMMbayes::mHMM} or subject-specific transition probability matrices."))
      }
      level <- "group"
      m <- nrow(model)
    }
  }
  state_labels <- paste("State", 1:m)
  if (level == "subject") {
    if (is.null(subject)) {
      subject <- 1:nsubj
    }
    if (length(subject) == 1) {
      gamma_matrix <- gamma_matrix[[subject]]
    } else {
      gamma_matrix <- gamma_matrix[subject]
      if (facet) {
        col_names <- paste("From", rep(state_labels, each = m), "to", state_labels)
        gamma_melt <- lapply(gamma_matrix, function(x){
          x %>%
            as.data.frame() %>%
            tibble::rownames_to_column(var = "From_state") %>%
            tidyr::pivot_longer(
              cols = tidyselect::starts_with("To"),
              names_to = "To_state",
              values_to = "prob"
            ) %>%
            dplyr::mutate(
              From_state = factor(.data$From_state, labels = state_labels),
              To_state = factor(.data$To_state, labels = state_labels)
            )
        }) %>%
          dplyr::bind_rows(.id = "Subject") %>%
          dplyr::mutate(Subject = factor(.data$Subject, levels = paste("Subject", subject)))
        gg <- ggplot2::ggplot(data = gamma_melt,
                              mapping = ggplot2::aes(x = .data$To_state, y = .data$From_state, fill = .data$prob)) +
          ggplot2::geom_tile(color = "white") +
          ggplot2::scale_fill_distiller(palette = "Spectral",
                                        limits = c(0, 1),
                                        name = "State switching\nprobability") +
          ggplot2::geom_text(
            mapping = ggplot2::aes(
              x = .data$To_state,
              y = .data$From_state,
              label = format(round(.data$prob, digits), nsmall = digits)
            ),
            color = "black",
            size = 4
          ) +
          ggplot2::labs(x = "To mood state", y = "From mood state") +
          ggplot2::coord_fixed() +
          ggplot2::facet_wrap(~Subject, ncol = ncol_facet, axes = "all") +
          ggplot2::scale_y_discrete(limits = rev) +
          ggplot2::theme_bw()
      } else {
        col_names <- paste("From", rep(state_labels, each = m), "to", state_labels)
        gamma_matrix <- sapply(gamma_matrix, function(x)
          c(t(x))) %>%
          t() %>%
          as.data.frame() %>%
          tibble::rownames_to_column(var = "Subject") %>%
          dplyr::rename_with( ~ c("Subject", col_names)) %>%
          tidyr::pivot_longer(
            cols = tidyselect::starts_with("From"),
            names_to = "From_To",
            values_to = "prob"
          ) %>%
          dplyr::mutate(Subject = factor(.data$Subject, levels = paste("Subject", subject)))
        gg <- gamma_matrix %>%
          ggplot2::ggplot(mapping = ggplot2::aes(
            x = .data$From_To,
            y = .data$Subject,
            fill = .data$prob
          )) +
          ggplot2::geom_tile(color = "white") +
          ggplot2::scale_fill_distiller(palette = "Spectral",
                                        limits = c(0, 1),
                                        name = "State switching\n probability") +
          ggplot2::geom_text(
            mapping = ggplot2::aes(
              x = .data$From_To,
              y = .data$Subject,
              label = format(round(.data$prob, digits), nsmall = digits)
            ),
            color = "darkslategrey",
            size = 3
          ) +
          ggplot2::labs(x = ggplot2::element_blank(), y = "Subject") +
          ggplot2::scale_y_discrete(limits = rev) +
          ggplot2::coord_fixed() +
          ggplot2::theme_bw() +
          ggplot2::theme(axis.text.x = ggplot2::element_text(
            angle = 45,
            vjust = 1,
            hjust = 1
          ))
      }
      return(gg)
    }
  }
  gamma_melt <- gamma_matrix %>%
    as.data.frame() %>%
    tibble::rownames_to_column(var = "From_state") %>%
    tidyr::pivot_longer(
      cols = tidyselect::starts_with("To"),
      names_to = "To_state",
      values_to = "prob"
    )
  gamma_melt <- gamma_melt %>%
    dplyr::mutate(
      From_state = factor(.data$From_state, labels = state_labels),
      To_state = factor(.data$To_state, labels = state_labels)
    )
  if (level == "group" | !is.null(subject)) {
    gg <- ggplot2::ggplot(data = gamma_melt,
                          mapping = ggplot2::aes(x = .data$To_state, y = .data$From_state, fill = .data$prob)) +
      ggplot2::geom_tile(color = "white") +
      ggplot2::scale_fill_distiller(palette = "Spectral",
                                    limits = c(0, 1),
                                    name = "State switching\nprobability") +
      ggplot2::geom_text(
        mapping = ggplot2::aes(
          x = .data$To_state,
          y = .data$From_state,
          label = format(round(.data$prob, digits), nsmall = digits)
        ),
        color = "black",
        size = 4
      ) +
      ggplot2::labs(x = "To mood state", y = "From mood state") +
      ggplot2::coord_fixed()
  }
  gg <- gg +
    ggplot2::scale_y_discrete(limits = rev) +
    ggplot2::theme_bw()
  return(gg)
}
