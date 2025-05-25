plot_density <- function(model,
                         range_plot,
                         level = "group",
                         subject = NULL,
                         steps = 0.5){
  # Check if the model is of class 'mHMMbayes'
  check_model(model, classes = "mHMM")
  if(model$input$data_distr == 'continuous'){
    cli::cli_abort(
      c("`plot_density()` can only be used to plot continuous emissions.",
        "x" = "The model you provided is built using count data.",
        "i" = "`plot_emiss()` may be a more appropriate function."
      )
    )
  }
  range_plot <- c(0, 100)
  steps <- 0.5
  model <- out_2st_emotion
  dep_labels <- model$input$dep_labels
  emiss <- mHMMbayes::obtain_emiss(model, level = level)
  if(level == 'subject'){
    emiss <- lapply(emiss, function(x) x[[subject]])
  }
  n_dep <- model$input$n_dep
  m <- model$input$m

  emiss_long <- lapply(emiss, function(x) {
    x <- as.data.frame(x)
    x$State <- rownames(x)
    rownames(x) <- NULL
    return(x)
  }) %>%
    dplyr::bind_rows(.id = "Dep") %>%
    dplyr::mutate(Dep = factor(Dep, levels = dep_labels))
  sequence <- seq(from = range_plot[1], to = range_plot[2], by = steps)
  curve_data <- emiss_long %>%
    dplyr::group_by(Dep, State) %>%
    dplyr::do({
      mean <- .data$Mean
      sd <- .data$SD
      y <- dnorm(sequence, mean, sd)
      data.frame(x = sequence, y = y)
    }) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(Dep = rep(emiss_long$Dep, each = length(sequence)),
                  State = rep(emiss_long$State, each = length(sequence)))
  gg <- ggplot2::ggplot(curve_data, aes(x = x, y = y, color = factor(State))) +
    ggplot2::geom_line() +
    ggplot2::geom_vline(data = emiss_long, aes(xintercept = Mean, colour = as.factor(State)), linetype = 'dashed', linewidth = 0.6) +
    ggplot2::facet_wrap(~ Dep) +
    ggplot2::labs(color = "State", x = "Value", y = "Prob") +
    ggplot2::theme_bw()
  return(gg)
}
