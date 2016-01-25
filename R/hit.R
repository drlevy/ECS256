#' @title Find expected hitting times
#' @description Input an mc to get a submatrix of hitting times where M[i,j] is the E(Tij).
#' @param mc of type mc containing a defined pijdef.
#' @param e floating point convergence criterion for infinite pijdef.
#' @return The submatrix of expected hitting times for mc$pijdef.
#' @examples 
#' pijdef = matrix(rep(0,9), nrow=3)
#' pijdef[1,1] <- 0.5
#' pijdef[1,2] <- 0.5
#' pijdef[2,3] <- 0.5
#' pijdef[2,1] <- 0.5
#' pijdef[3,1] <- 1
#' markov_chain = mc(pijdef)
#' hit(markov_chain, 0.1)
#' @export
hit <- function(mc, e = 0.01) {
  validateInput(mc)

  p <- mc$pijdef

  if (class(mc$pijdef) == "function") {
    statesInConverged <- NROW(stn (mc, e))
    convergedTransitionMatrix <- matrix(rep(0, statesInConverged*statesInConverged), nrow=statesInConverged)
    for (i in 1:statesInConverged) {
      for (j in 1:statesInConverged) {
        convergedTransitionMatrix[i, j] = mc$pijdef(i, j)
      }
    }
    p <- convergedTransitionMatrix
  }

  findEtas(p)
}

findEtas <- function(p) {
  n <- nrow(p)
  etas <- matrix(rep(0,n*n),nrow=n)
  for (j in 1:n) {
    etasForJ <- findEta(p, j)
    pJ <- p[, -j]
    etaJJ <- 1
    for (i in 1:(n-1)) {
      if (NCOL(pJ) == 1) {
        etaJJ <- etaJJ + pJ[j]*etasForJ[i]
      } else {
        etaJJ <- etaJJ + pJ[j, i]*etasForJ[i]
      }
    }
    etasForJ <- append(etasForJ, etaJJ, after=j-1)
    etas[, j] <- etasForJ
  }
  return(etas)
}

findEta <- function(p, j) {
  n <- nrow(p)
  q <- diag(n) - p
  q <- q[-j, -j]
  ones <- rep(1, n-1)
  if (length(q) == 1 && q == 0) {
    return(0)
  } else {
    solve(q, ones)
  }
}
