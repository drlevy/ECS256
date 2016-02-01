#' TODO: Continuous example.
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

  # Default to discrete markov chain.
  m <- mc$pijdef
  func <- findDiscretePi
  
  # If qidef is present, switch to continuous mc.
  if (!is.null(mc$qidef)) {
    m <- qijdef(mc)
    func <- findContinuousPi
  } 
  
  # Handle convergence if function input (infinite)
  if (class(m) == "function") {
    infiniteHelper(m, func, e)
  } else {
    func(m)
  }
}

findDiscretePi <- function(p) {
  n <- nrow(p)
  imp <- diag(n) - t(p)
  imp[n,] <- rep(1, n)
  rhs <- c(rep(0, n-1), 1)
  solve(imp, rhs)
}

findContinuousPi <- function(q) {
  n <- nrow(q)
  q[n,] <- rep(1, n)
  rhs <- c(rep(0, n-1), 1)
  solve(q, rhs)
}
