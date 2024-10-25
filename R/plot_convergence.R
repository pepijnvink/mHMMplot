#' Plot trace plots to assess convergence of a Bayesian Multilevel Hidden Markov Model
#'
#' @param model Object or a list of objects of type `mHMMbayes::mHMM` or `mHMMbayes:mHMM_vary`, created using [mHMMbayes::mHMM()] or [mHMMbayes::mHMM_vary()].
#' @param component Character string specifying the component to plot. Takes "gamma" or "emiss".
#' @param param Optional character string specifying the parameter to plot for the plotted component. If `NULL` (default), plots the means (or probabilities). Takes "var" for between-person variances, "sd" for standard deviations of normal emission distributions, and "beta" for regression coefficients.
#' @param level Character string specifying the level of parameter to plot. Takes "group" or "subject".
#' @param vrb Optional character string specifying the variable to plot when plotting categorical emission distributions.
#' @param prob Logical specifying whether converence of transitions or categorical emissions should be plotted on the probability scale, rather than the log scale.
#' @param ID Integer specifying the subject to plot subject specific parameters for.
#' @param state_labels Optional character string specifying labels to use for the inferred states.
#' @param cat_labels Optional character string used to specify labels for categories when plotting emission distributions of categorical variables.
#'
#' @return Object of type `ggplot2::gg`, plotting parameter distributions.
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
#' plot_convergence(model = out_3st_cont_sim,
#'               param = "gamma",
#'               level = "group",
#'               prob = TRUE)
#' }
plot_convergence <- function(model,
                             component = "gamma",
                             param = NULL,
                             level = "group",
                             vrb = NULL,
                             prob = FALSE,
                             ID = NULL,
                             state_labels = NULL,
                             cat_labels = NULL) {
  if (inherits(model, c("mHMM", "mHMM_vary"))) {
    model_1 <- model
    model <- list(model)
  }
  lapply(model, function(x) {
    check_model(
      x,
      classes = c("mHMM", "mHMM_vary"),
      fns = c("mHMM", "mHMM_vary")
    )
  })
  n_chains <- length(model)
  if (n_chains > 1) {
    list_input <- lapply(model, function(x)
      x$input)
    all_identical <- Reduce(function(x, y)
      identical(x, y), list_input)
    if (!all_identical) {
      cli::cli_abort(
        c("x" = "Not all models you provided are identical", "!" = "Please provide new models")
      )
    } else {
      model_1 <- model[[1]]
    }
  }
  is_mhmm_vary <- inherits(model_1, "mHMM_vary")
  if (is.null(level) | !(level %in% c("group", "subject"))) {
    cli::cli_abort(
      c("x" = "{.var level} specifying the level to plot has not been specified.", "!" = "Please specify 'group' to plot the group-level parameters, or 'subject' to plot the subject level parameters.")
    )
  }
  if (component %nin% c("gamma", "emiss")) {
    comp <- cli::cli_vec(c("gamma", "emiss"), style = list("vec-sep2" = " or "))
    cli::cli_abort("Must provide {.val {comp}} to {.var component} to specify the component to plot.")
  }
  if (level %nin% c("group", "subject")) {
    cli::cli_abort("x" = "{.val {level}} is not a valid input for {.var level}.", "i" = "Valid inputs are {.val group} or {.val subject}.")
  }
  if (is.null(ID) & level == "subject") {
    cli::cli_abort(
      c(
        "{.code plot_convergence} needs an ID specifying the subject to plot when plotting subject-specific parameters.",
        "i" = "Please provide an ID indicator using the {.var ID} argument, or specify {.code level = {.val group}}."
      )
    )
  }
  if (!is.null(vrb) & component == "gamma") {
    cli::cli_warn(
      c("x" = "You provided a variable to plot using {.var vrb} while plotting transition probabilities", "i" = "The variable name you provided will be ignored.")
    )
  }
  if (!is.null(cat_labels) & component == "gamma") {
    cli::cli_warn(
      c("x" = "You provided category labels using {.var cat_labels} while plotting transition probabilities", "i" = "The category labels you provided will be ignored.")
    )
  }
  m <- model_1$input$m
  vrb_ind <- NULL
  if (component == "emiss") {
    vrb <- check_vrb(model_1, vrb)
    vrb_ind <- which(model_1$input$dep_labels == vrb)
    if (inherits(model, "mHMM_vary")) {
      data_distr <- model_1$input$data_distr[which(model_1$input$dep_labels == vrb)]
    } else {
      data_distr <- model_1$input$data_distr
    }
    if (!is.null(cat_labels) & data_distr != "categorical") {
      cli::cli_warn(
        c(
          "You provided category labels using {.var cat_labels} while plotting a {data_distr} variable.",
          "i" = "The category labels you provided will be ignored."
        )
      )
    }
    if (is.null(param)) {
      param_comb <- component
    } else if (level == "group") {
      allowed <- list(
        "categorical" = c("var", "beta"),
        "continuous" = c("var", "beta", "sd"),
        "count" = c("var", "beta")
      )
      if (param %in% allowed[[data_distr]]) {
        param_comb <- paste0(component, "_", param)
      } else {
        allowed_vec <- cli::cli_vec(model_1$input$dep_labels,
                                    style = list("vec-last" = ", or ", "vec-sep2" = " or "))
        cli::cli_abort(
          c(
            "x" = "{.val {param}} is not a valid value for {.var param} for your type of data.",
            "i" = "You want to plot a {data_distr} variable at the group level.",
            "i" = "Allowed values for {.var param} are: {.val {allowed_vec}} (or NULL)."
          )
        )
      }
    } else {
      cli::cli_warn(
        c(
          "The function does not take a parameter specification when plotting subject specific distributions.",
          "i" = "Your input for {.var param} will be ignored."
        )
      )
      param_comb <- component
    }
    allparams <- list(
      "group" = list(
        "categorical" = c(
          "emiss" = c("emiss_int_bar", "emiss_prob_bar")[prob + 1],
          "emiss_var" = "emiss_V_int_bar",
          "emiss_beta" = "emiss_cov_bar"
        ),
        "continuous" = c(
          "emiss" = "emiss_mu_bar",
          "emiss_var" = "emiss_varmu_bar",
          "emiss_sd" = "emiss_sd_bar",
          "emiss_beta" = "emiss_cov_bar"
        ),
        "count" = c(
          "emiss" = "emiss_mu_bar",
          "emiss_var" = "emiss_varmu_bar",
          "emiss_beta" = "emiss_cov_bar"
        )
      ),
      "subject" = list(
        "categorical" = c("emiss" = c("emiss_int_subj", "cat_emiss")[prob + 1]),
        "continuous" = c("emiss" = "cont_emiss"),
        "count" = c("emiss" = "count_emiss")
      )
    )[[level]][[data_distr]]
  } else {
    if (is.null(param)) {
      param_comb <- component
    } else if (level == "group") {
      if (param %in% c("beta", "var")) {
        param_comb <- paste0(component, "_", param)
      } else {
        cli::cli_abort(
          c(
            "x" = "{.val {param}} is not a valid value for {.var param} when plotting the group level transition probability matrix.",
            "i" = "Allowed values for {.var param} are: {.val var} or {.val beta} (or NULL).",
            "!" = "Please provide a different value for {.var param}."
          )
        )
      }
    } else {
      cli::cli_warn(
        c(
          "The function does not take a parameter specification when plotting subject specific distributions.",
          "i" = "Your input for {.var param} will be ignored."
        )
      )
      param_comb <- component
    }
    allparams <- list(
      "group" = c(
        "gamma_beta" = "gamma_cov_bar",
        "gamma" = c("gamma_int_bar", "gamma_prob_bar")[prob +
                                                         1],
        "gamma_var" = "gamma_V_int_bar"
      ),
      "subject" = c("gamma" = c("gamma_int_subj", "trans_prob")[prob + 1])
    )[[level]]
  }
  param_name <- allparams[param_comb]
  if (param_name %in% c("trans_prob", "cat_emiss", "cont_emiss", "count_emiss")) {
    obj <- lapply(model, function(x) {
      x[["PD_subj"]][[ID]][[param_name]] %>%
        tibble::as_tibble(.name_repair = "minimal")
    })
    if (param_name %in% c("cat_emiss", "cont_emiss", "count_emiss")) {
      obj <- obj %>%
        lapply(function(x) {
          x %>%
            dplyr::select(tidyselect::starts_with(paste0("dep", vrb_ind, "_")) &
                            !tidyselect::ends_with(c("fixvar", "sd")))
        })
    } else {
      for (i in 1:n_chains) {
        colnames(obj[[i]]) <- paste0("S", rep(1:m, each = m), "toS", 1:m)
      }
    }
  } else if (param_name == "emiss_int_subj") {
    obj <- lapply(model, function(x) {
      x[[param_name]][[ID]][[vrb]] %>%
        tibble::as_tibble(.name_repair = "minimal")
    })
  } else if (param_name %in% c(
    "gamma_int_bar",
    "gamma_cov_bar",
    "gamma_V_int_bar",
    "gamma_prob_bar",
    "emiss_cont_cov_bar",
    "emiss_cat_cov_bar"
  )) {
    obj <- lapply(model, function(x) {
      x[[param_name]] %>%
        tibble::as_tibble(.name_repair = "minimal")
    })
  } else if (param_name == "gamma_int_subj") {
    obj <- lapply(model, function(x) {
      x[[param_name]][[ID]] %>%
        tibble::as_tibble(.name_repair = "minimal")
    })
  } else {
    obj <- lapply(model, function(x) {
      x[[param_name]][[vrb]] %>%
        tibble::as_tibble(.name_repair = "minimal")
    })
  }
  obj <- obj %>%
    dplyr::bind_rows(.id = "chain") %>%
    dplyr::mutate(chain = factor(chain, labels = paste("Chain", 1:n_chains)))
  clnm <- list(
    "emiss_int_bar" = c("State", "Category"),
    "emiss_prob_bar" = c("State", "Category"),
    "emiss_V_int_bar" = c("Category", "State"),
    "emiss_cov_bar",
    "emiss_mu_bar" = "State",
    "emiss_varmu_bar" = "State",
    "emiss_sd_bar" = "State",
    "emiss_int_subj" = c("Category", "State"),
    "cat_emiss" = c("State", "Category"),
    "cont_emiss" = "State",
    "count_emiss",
    "gamma_cov_bar",
    "gamma_int_bar" = c("From", "To"),
    "gamma_prob_bar" = c("From", "To"),
    "gamma_V_int_bar" = c("From", "To"),
    "gamma_int_subj" = c("From", "To"),
    "trans_prob" = c("From", "To")
  )
  facet <- list(
    "emiss_int_bar" = c("State", "Category"),
    "emiss_prob_bar" = c("State", "Category"),
    "emiss_V_int_bar" = c("State", "Category"),
    "emiss_int_subj" = c("State", "Category"),
    "cat_emiss" = c("State", "Category"),
    "gamma_int_bar" = c("From", "To"),
    "gamma_prob_bar" = c("From", "To"),
    "gamma_V_int_bar" = c("From", "To"),
    "gamma_int_subj" = c("From", "To"),
    "trans_prob" = c("From", "To")
  )
  colmns <- clnm[[param_name]]
  if (is.null(vrb_ind)) {
    vrb_ind <- 1
  }
  ptrns <- c(
    "emiss_int_bar" = "S(\\d+)_int_emiss(\\d+)",
    "emiss_prob_bar" = "S(\\d+)_emiss(\\d+)",
    "emiss_V_int_bar" = "var_int_Emiss(\\d+)_S(\\d+)",
    "emiss_cov_bar",
    "emiss_mu_bar" = "mu_(\\d+)",
    "emiss_varmu_bar" = "varmu_(\\d+)",
    "emiss_sd_bar" = "sd_(\\d+)",
    "emiss_int_subj" = "int_Emiss(\\d+)_S(\\d+)",
    "cat_emiss" = paste0("dep", vrb_ind, "_S", "(\\d+)", "_emiss", "(\\d+)"),
    "cont_emiss" = paste0("dep", vrb_ind, "_S", "(\\d+)", "_mu"),
    "count_emiss",
    "gamma_cov_bar",
    "gamma_int_bar" = "int_S(\\d+)toS(\\d+)",
    "gamma_prob_bar" = "S(\\d+)toS(\\d+)",
    "gamma_V_int_bar" = "int_S(\\d+)toS(\\d+)",
    "gamma_int_subj" = "S(\\d+)toS(\\d+)",
    "trans_prob" = "S(\\d+)toS(\\d+)"
  )
  pat <- ptrns[param_name]
  obj <- obj %>%
    dplyr::group_by(chain) %>%
    dplyr::mutate(iter = 1:dplyr::n()) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_longer(
      cols = -c(chain, iter),
      names_to = colmns,
      names_pattern = pat
    )
  if (!is.null(state_labels)) {
    if (length(state_labels) != m) {
      cli::cli_warn(
        c(
          "The number of names provided in {.var state_labels} must equal the number of states.",
          "i" = "The number of states is {m}.",
          "x" = "You provided {length(state_labels)} names.",
          "i" = "The state labels you provided will be ignored."
        )
      )
      obj <- obj %>%
        dplyr::mutate(dplyr::across(
          tidyselect::any_of(c("State", "From", "To")),
          ~ factor(
            .x,
            levels = 1:m,
            labels = paste("State", 1:m)
          )
        ))
    } else {
      obj <- obj %>%
        dplyr::mutate(dplyr::across(
          tidyselect::any_of(c("State", "From", "To")),
          ~ factor(.x, levels = 1:m, labels = state_labels)
        ))
    }
  } else {
    obj <- obj %>%
      dplyr::mutate(dplyr::across(
        tidyselect::any_of(c("State", "From", "To")),
        ~ factor(.x, levels = 1:m, labels = paste("State", 1:m))
      ))
  }
  if (!is.null(cat_labels) & ("Category" %in% colnames(obj))) {
    n_levels_cat <- model_1$input$q_emiss[vrb_ind]
    if (length(cat_labels) != n_levels_cat) {
      cli::cli_warn(
        c(
          "The number of names provided in {.var cat_labels} must equal the number of category values.",
          "i" = "The number of category values is {n_levels_cat}.",
          "x" = "You provided {length(cat_labels)} names.",
          "i" = "The category labels you provided will be ignored."
        )
      )
      n_levels_cat <- model_1$input$q_emiss[vrb_ind]
      obj <- obj %>%
        dplyr::mutate(Category = factor(
          Category,
          levels = 1:n_levels_cat,
          labels = paste("Category", 1:n_levels_cat)
        ))
    } else {
      obj <- obj %>%
        dplyr::mutate(Category = factor(
          Category,
          levels = 1:n_levels_cat,
          labels = cat_labels
        ))
    }
  } else if ("Category" %in% colnames(obj)) {
    n_levels_cat <- model_1$input$q_emiss[vrb_ind]
    obj <- obj %>%
      dplyr::mutate(Category = factor(
        Category,
        levels = 1:n_levels_cat,
        labels = paste("Category", 1:n_levels_cat)
      ))
  }
  if (n_chains > 1) {
    gg <- obj %>%
      ggplot2::ggplot(mapping = ggplot2::aes(x = iter, y = value, color = chain))
  } else {
    gg <- obj %>%
      ggplot2::ggplot(mapping = ggplot2::aes(x = iter, y = value))
  }
  gg <-  gg +
    ggplot2::geom_line()
  if (length(colmns) == 2) {
    gg <- gg +
      ggplot2::facet_grid(rows = ggplot2::vars(!!rlang::sym(facet[[param_name]][1])),
                          cols = ggplot2::vars(!!rlang::sym(facet[[param_name]][2])))
  } else {
    gg <- gg +
      ggplot2::facet_grid(rows = ggplot2::vars(!!rlang::sym(clnm[[param_name]])))
  }
  gg <- gg +
    ggplot2::theme_classic()
  return(gg)
}
