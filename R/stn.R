#' @title Find stationary distribution
#' @description Input an mc to get a vector of stationary probabilities for each markov state.
#' @param mc of type mc containing a defined pijdef.
#' @param e floating point convergence criterion for infinite pijdef.
#' @return The stationary distribution vector for mc$pijdef.
#' @examples 
#' pijdef = matrix(rep(0,9), nrow=3)
#' pijdef[1,1] <- 0.5
#' pijdef[1,2] <- 0.5
#' pijdef[2,3] <- 0.5
#' pijdef[2,1] <- 0.5
#' pijdef[3,1] <- 1
#' markov_chain = mc(pijdef)
#' stn(markov_chain, 0.1)
#' @export
stn <- function(mc, e = 0.01) {
  validateInput(mc)

  if (class(mc$pijdef) == "function") {
    infiniteStn(mc$pijdef, e)
  } else {
    findPi(mc$pijdef)
  }
}

infiniteStn <- function(pFunc, e) {
  n <- 10
  pi <- c(rep(0, n))
  repeat {
    p <- matrix(rep(0, n*n),nrow = n)
    for (i in 1:n) {
      for (j in 1:n) {
        p[i, j] <- pFunc(i, j)
      }
    }
    newPi <- findPi(p)
    if (dist(rbind(pi, newPi[1:NROW(pi)])) <= e) {
      return(newPi)
    }
    pi <- newPi
    n <- 2*n
  }
}

findPi <- function(p) {
  n <- nrow(p)
  imp <- diag(n) - t(p)
  imp[n,] <- rep(1, n)
  rhs <- c(rep(0, n-1), 1)
  solve(imp, rhs)
}
