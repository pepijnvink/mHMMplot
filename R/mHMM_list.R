#' Create mHMM_list object with multiple chains
#' 
#' @param ... Objects of type `mHMM`, where each object is a separate chain.
#' 
#' @returns An object of type [[mHMMplot::mHMM_list]]
#'
#' @export
#' 
mHMM_list <- function(...){
   models <- list(...)
  
  # If a single list of mHMM objects was passed, unwrap it
  if (length(models) == 1 && is.list(models[[1]]) && !inherits(models[[1]], c("mHMM", "mHMM_vary"))) {
    models <- models[[1]]
  }
  all_mHMM <- all(sapply(models, inherits, 'mHMM'))
  all_mHMM_vary <- all(sapply(models, inherits, 'mHMM_vary'))
  if (!all_mHMM & !all_mHMM_vary) {
    cli::cli_abort(c(
      "The function `mHMM_list` takes `mHMM` objects",
      "i" = "Please provide different objects."
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
