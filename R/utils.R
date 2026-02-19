#' Check if object is a birt model fit (internal)
#'
#' @param object Any object.
#' @return TRUE if it's a birt fit, otherwise throws an error.
#' @keywords internal
assert_birt_fit <- function(object) {
  valid <- c("birt_fit", "birt_2pl_fit", "birt_3pl_fit")
  if (!inherits(object, valid)) {
    cli::cli_abort(
      "{.arg object} must be a fitted model from {.fn rasch_fit},
       {.fn twopl_fit}, or {.fn threepl_fit}."
    )
  }
  invisible(TRUE)
}

#' Get model name (internal)
#'
#' @param object A birt fit object.
#' @return Character string: "Rasch", "2PL", or "3PL".
#' @keywords internal
get_model_name <- function(object) {
  if (inherits(object, "birt_3pl_fit")) return("3PL")
  if (inherits(object, "birt_2pl_fit")) return("2PL")
  "Rasch"
}
