#' Create mHMM_list object with multiple chains
#'
#' @param models List of objects of type `[mHMM::mHMM()]` consisting of different chains.
#'
#' @returns An object of type [[mHMMplot::mHMM_list]]
#'
#' @export
#'
mHMM_list <- function(models) {
  all_mHMM <- all(sapply(models, inherits, 'mHMM'))
  all_mHMM_vary <- all(sapply(models, inherits, 'mHMM_vary'))
  if (!all_mHMM & !all_mHMM_vary) {
    cli::cli_abort(c(
      "The function `mHMM_list` takes a list of `mHMM` objects",
      "i" = "Please provide a different list."
    ))
  }
  nchains <- length(models)
  list_input <- lapply(models, function(x) x$input)
  all_identical <- length(unique(list_input)) == 1
  if (!all_identical) {
    cli::cli_abort(c(
      "x" = "The models you provided do not have identical input",
      "i" = "Please check and provide a different list"
    ))
  }
  if (all_mHMM) {
    if (inherits(models[[1]], 'cont')) {
      class(models) <- c('mHMM_list_cont', 'mHMM_list', 'list')
    } else if (inherits(models[[1]], 'cat')) {
      class(models) <- c('mHMM_list_cat', 'mHMM_list', 'list')
    }
  } else {
    class(models) <- c('mHMM_list_vary', 'mHMM_list', 'list')
  }
  return(models)
}
