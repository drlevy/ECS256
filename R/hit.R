#  TODO: Allow i,j as input for a specific sub-matrix.
#  TODO: Continuous example.
#' @title Find expected hitting times
#' @description Input an mc to get a submatrix of hitting times where M[i,j] is the E(Tij).
#' @param mc of type mc containing a defined markov chain
#' @param e floating point convergence criterion for infinite mc.
#' @return The submatrix of expected hitting times for mc.
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

  # Default to discrete markov chain.
  m <- mc$pijdef
  func <- findDiscreteEtas  

  # If qidef is present, switch to continuous markov chain.
  if (!is.null(mc$qidef)) {
    m <- qijdef(mc)
    func <- findContinuousEtas
  }   

  # Handle convergence if function input (infinite).
  if (class(m) == "function") {
    statesInConverged <- NROW(stn (mc, e))
    convergedTransitionMatrix <- matrix(rep(0, statesInConverged*statesInConverged), nrow=statesInConverged)
    for (i in 1:statesInConverged) {
      for (j in 1:statesInConverged) {
        convergedTransitionMatrix[i, j] <- m(i, j)
      }
    }
    m <- convergedTransitionMatrix
  } 
      
  func(m)
}

findDiscreteEtas <- function(p) {
  n <- nrow(p)
  etas <- matrix(rep(0, n*n), nrow=n)
  for (j in 1:n) {
    etasForJ <- findDiscreteEta(p, j)
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

findDiscreteEta <- function(p, j) {
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

findContinuousEtas <- function(q) {
  n <- nrow(q)
  etas <- matrix(rep(0, n*n), nrow=n)
  
  for (k in 1:n) {
    etas[, k] <- findContinuousEta(q, k)
  }
  
  return(etas)
}

findContinuousEta <- function(q, k) {
  n <- nrow(q)
  qk <- matrix(rep(0, n*n), nrow=n)

  for (i in 1:n) {
    for (j in 1:n) {
      if (i == k) {
        if (j == k) {
          qk[i, j] <- 1
        } else {
          qk[i, j] <- 0
        }
      } else {
        qk[i, j] <- -q[i, j]
      }
    }
  }
  
  rhs <- c(rep(1, n))
  rhs[k] <- 0
  
  solve(qk, rhs)
}