# Functions for Internal Use
`%nin%` <- Negate(`%in%`)

check_model <- function(model,
                        classes = "mHMM",
                        fns = classes) {
  if (is.null(model)) {
    cls_names <- cli::cli_vec(paste0("mHMMbayes::", classes),
                              style = list(
                                "vec-last" = ", or ",
                                "vec-sep2" = " or "
                              )
    )
    fns_names <- cli::cli_vec(paste0("mHMMbayes::", fns),
                              style = list(
                                "vec-last" = ", or ",
                                "vec-sep2" = " or "
                              )
    )
    cli::cli_abort(
      c(
        "x" = "The argument {.var model} has not been specified.",
        "i" = "Please specify an object of class {.cls {cls_names}},
        obtained using {.fn {fns_names}}."
      )
    )
  }
  if (!inherits(model, classes)) {
    cls_names <- cli::cli_vec(paste0("mHMMbayes::", classes),
                              style = list(
                                "vec-last" = ", or ",
                                "vec-sep2" = " or "
                              )
    )
    fns_names <- cli::cli_vec(paste0("mHMMbayes::", fns),
                              style = list(
                                "vec-last" = ", or ",
                                "vec-sep2" = " or "
                              )
    )
    cli::cli_abort(
      c(
        "Argument {.var model} should be an object of class
        {.cls {cls_names}}.",
        "x" = "You have provided an object of class {.cls {class(model)}}.",
        "i" = "Use the function {.fn {fns_names}} to obtain an object
        of class {.cls {cls_names}}."
      )
    )
  }
}

check_vrb <- function(model, vrb, vctr = TRUE) {
  if (is.null(vrb)) {
    vrb <- model$input$dep_labels[1]
    cli::cli_inform(
      c(
        "You did not provide a string to {.var vrb}, specifying
        the dependent variable to plot.",
        "i" = "The first variable in the dataset, {.val {vrb}}, will be used."
      )
    )
  } else if (!is.character(vrb)) {
    vrb <- model$input$dep_labels[1]
    cli::cli_inform(
      c(
        "{.var vrb} needs a character string,
        specifying the dependent variable to plot.",
        "x" = "You specified an object of class {.cls {class(vrb)}}.",
        "i" = "The first variable in the dataset,{.val{vrb}}, will be used."
      )
    )
  } else if (!vctr && length(vrb) > 1) {
    cli::cli_inform(
      c(
        "{.var vrb} needs a character string of length 1,
        specifying the dependent variable to plot.",
        "x" = "You specified an object with length {length(vrb)}.",
        "i" = "The first specified variable ({.val vrb[1]}) will be plotted."
      )
    )
    vrb <- vrb[1]
  }
  if (vrb %nin% model$input$dep_labels) {
    dep_labs <- cli::cli_vec(model$input$dep_labels,
                             style = list(
                               "vec-last" = ", or ",
                               "vec-sep2" = " or "
                             )
    )
    cli::cli_abort(
      c(
        "Argument {.var vrb} should be a character string corresponding to
        a variable name in the model.",
        "x" = "{.val {vrb}} does not correspond to
        a variable name used in the model.",
        "i" = "Possible values of {.var vrb} for your model are:
        {.val {dep_labs}}."
      )
    )
  }
  return(vrb)
}

theme_mhmm <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = 12, vjust = .5, hjust = 1),
      axis.text.x = ggplot2::element_text(angle = 45),
      axis.title = ggplot2::element_text(size = 14),
      plot.title = ggplot2::element_text(size = 16, face = "bold"),
      legend.text = ggplot2::element_text(size = 12),
      legend.title = ggplot2::element_text(size = 14),
      strip.text = ggplot2::element_text(size = 12),
      plot.caption = ggplot::element_text(size = 10)
    )
}

scale_color_mhmm <- function(which = "color") {
  clrs <- c("#4E79A7",
            "#F28E2B",
            "#E15759",
            "#499894",
            "#59A14F",
            "#EDC948",
            "#B07AA1",
            "#d37295",
            "#9C755F",
            "#BAB0AC")
  if(which == "color") {
    ggplot2::scale_color_manual(clrs)
  } else if (which == "fill") {
    ggplot2::scale_fill_manual(clrs)
  }
}

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @importFrom rlang .data
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL
