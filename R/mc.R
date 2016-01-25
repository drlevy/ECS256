#' @title Markov chain definition
#' @description Construct and initialize with some pijdef.
#' @param pijdef is either a square finite transition matrix
#' or a function(i, j) defining a (potentially) infinite transition matrix.
#' @export
mc <- function(pijdef = NULL) {
  mc <- list(pijdef = pijdef)
  class(mc) <- "mc"
  return(mc)
}
