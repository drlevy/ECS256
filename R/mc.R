#' Markov chain definition.
#' @param Construct and initialize with some pijdef.
#' @slot pijdef is either a square finite transition matrix
#' or a function(i, j) defining a (potentially) infinite transition matrix.
#' @export
mc <- function(pijdef = NULL) {
  mc <- list(pijdef = pijdef)
  class(mc) <- "mc"
  return(mc)
}
