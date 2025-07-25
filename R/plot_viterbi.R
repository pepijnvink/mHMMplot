#' Plot inferred states of a Bayesian Multilevel Hidden Markov Model
#'
#' @param states Data Frame with inferred states
#' obtained using [mHMMbayes::vit_HMM()] or object of class `mHMMbayes:mHMM`.
#' @param s_data Data Frame with data used to infer states using the Viterbi
#' algorithm. Only required when the object given to `states`
#' is of class `mHMMbayes::mHMM`.
#' @param subject Optional numeric vector with indices of subjects to plot.
#'
#' @return
#' Object of type `ggplot2::gg` with the plotted inferred states over time.
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
#' states <- vit_HMM(
#'   s_data = data_cont$obs,
#'   object = out_3st_cont_sim
#' )
#' plot_viterbi(states)
#' }
plot_viterbi <- function(states, s_data, subject = NULL) {
  if (inherits(states, "mHMM")) {
    if (is.null(s_data)) {
      cli::cli_abort(
        "x" = "You provided an object of class {.cls mHMM}
                     without a data frame with data to obtain states for.",
        "i" = "Please use the {.var s_data} argument to provide
                     a data frame, or instead use {.var states}
                     to provide a data frame of inferred states."
      )
    } else {
      suppressMessages({states <- mHMMbayes::vit_mHMM(states, s_data = s_data)})
    }
  }
  if (!inherits(states, "data.frame")) {
    cli::cli_abort(
      c(
        "!" = "The input for the {.var states} must be
                     a matrix with inferred states,
                     built using {.fn mHMMbayes:vit_mHMM()}.",
        "x" = "You provided an object of class
                   {.cls {class(states)}}."
      )
    )
  }
  m <- max(states$state)
  if (!is.null(subject)) {
    states <- states[states$subj %in% subject, ] %>%
      dplyr::mutate(subj = factor(.data$subj, levels = subject))
  }
  state_labels <- paste("State", 1:m)
  states <- states %>%
    dplyr::mutate(state = factor(.data$state, labels = state_labels)) %>%
    dplyr::mutate(subj = factor(.data$subj)) %>%
    dplyr::group_by(.data$subj) %>%
    dplyr::mutate(beep = seq_len(dplyr::n())) %>%
    dplyr::ungroup()
  gg <- ggplot2::ggplot(data = states) +
    ggplot2::geom_rect(
      mapping = ggplot2::aes(
        xmin = .data$beep - 0.5,
        ymin = 0,
        ymax = 1,
        xmax = .data$beep + 0.5,
        fill = .data$state
      )
    ) +
    ggplot2::facet_grid(rows = ggplot2::vars(.data$subj)) +
    ggplot2::xlab("Beep")
  gg <- gg +
    theme_mhmm() +
    scale_color_mhmm(which = "fill") +
    ggplot2::theme(
      axis.title.y = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank()
    )
  return(gg)
}
