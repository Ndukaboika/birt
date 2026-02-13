#' @export
print.bayesRasch <- function(x, ...) {
  cat("Bayesian Rasch Model\n")
  cat("Students:", x$J, "\n")
  cat("Items:", x$K, "\n")
}
