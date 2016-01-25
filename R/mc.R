#' @title Markov chain definition
#' @description Construct and initialize with some pijdef.
#' @param pijdef is either a square finite transition matrix
#' or a function(i, j) defining a (potentially) infinite transition matrix.
#' @examples 
#' pijdef = matrix(rep(0,9), nrow=3)
#' pijdef[1,1] <- 0.5
#' pijdef[1,2] <- 0.5
#' pijdef[2,3] <- 0.5
#' pijdef[2,1] <- 0.5
#' pijdef[3,1] <- 1
#' markov_chain = mc(pijdef)
#' @export
mc <- function(pijdef = NULL) {
  mc <- list(pijdef = pijdef)
  class(mc) <- "mc"
  return(mc)
}
