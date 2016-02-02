#  TODO: if need be, add an explicit time category factor instead of sensing input.
#' @title Markov chain definition
#' @description Construct and initialize either a continuous or discrete markov chain.
#' Input will be assumed discrete unless qidef is defined.
#' Input will be assumed finite unless pijdef is a function.
#' @param pijdef is either a square finite transition matrix
#' or a function(i, j) defining a (potentially) infinite transition matrix.
#' @param qidef is a vector containing the holding-time of each state in a continuous markov chain
#' or a function(i, j) defining a (potentially) infinite holding-time vector.
#' @examples 
#' pijdef = matrix(rep(0,9), nrow=3)
#' pijdef[1,1] <- 0.5
#' pijdef[1,2] <- 0.5
#' pijdef[2,3] <- 0.5
#' pijdef[2,1] <- 0.5
#' pijdef[3,1] <- 1
#' markov_chain = mc(pijdef)
#' @export
mc <- function(pijdef = NULL, qidef = NULL) {
  mc <- list(pijdef = pijdef, qidef = qidef)
  class(mc) <- "mc"
  return(mc)
}

# TODO: Figure out how to add as member fcn of mc...
#  ...without allowing user to overwrite func. def'n.

qijdef <- function(mc, i = 1, j = 1) {
  validateInput(mc)
  
  # Compute and return finite matrix.
  # TODO: cache matrix.
  if (class(mc$qidef) != "function") {
    n <- NROW(mc$qidef)
    m <- matrix(rep(0, n*n), nrow=n)
    for (i in 1:n) {
      for (j in 1:n) {
        m[i, j] <- qijdefHelper(mc, i, j)
      }
    }
    return(m)
  } else {
    qijdefHelper(mc, i, j) 
  }
}

qijdefHelper <- function(mc, i, j) {
  if (i != j) {
    mc$qidef[j]*mc$pijdef[j, i]
  } else {
    -mc$qidef[i]
  }   
}