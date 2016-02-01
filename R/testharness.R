# TODO: add expected test case values (i.e., unit tests)
#' @title Create an example markov chain and calculate the stationary distribution or expected hitting times
#' @description 
#' The example matrix that is created reflects the following problems:\cr\cr
#' * For the discrete case:\cr
#' ** For the finite case, we create a matrix that symbolizes flipping a coin and the probabilties of getting heads in a row. State 0 means we just got a tails. State 1 means we have 1 head. State 2 means we have gotten 2 heads in a row. This requires a 3x3 matrix since we have three states.\cr
#' ** The infinite case creates a function that represents an infinite matrix and a birth/death process of having the probability of 0.4 for birth and 0.6 for death.\cr\cr
#' * For the continuous case:\cr
#' ** For the finite case, we simulate a model with two machines s.t. mean wait when one machine is working is 1/25, 1/20 with both, and a mean repair time of 1/8.\cr
#' ** TODO: infinite, continuous case\cr\cr
#' All infinite case calculations are done with a convergence criterion of 0.1.
#' @param use_discrete_markov_chain boolean that indicates if you want to use a discrete(TRUE) or continuous(FALSE) markov chain.
#' @param use_finite_state boolean that indicates if you want to use a finite state(TRUE) matrix or infinite(FALSE)
#' @param calculate_stationary_dist boolean that indicates if you want to calculate the stationary distribution(TRUE) or expected hitting times(FALSE)
#' @return The stationary distribution or expected hitting times of the indicated matrix.
#' @examples 
#' testharness(FALSE,FALSE,FALSE)
#' Get expected hitting times for continuous, infinite markov chain.
#' @export
testharness <- function(use_discrete_markov_chain = TRUE, use_finite_state = TRUE, calculate_stationary_dist = TRUE)
{
  qidef = NULL
  
  if(use_discrete_markov_chain) {
    if(use_finite_state) {
      # three heads up game
      pijdef <- matrix(rep(0,9), nrow=3)
      pijdef[1,1] <- 0.5
      pijdef[1,2] <- 0.5
      pijdef[2,3] <- 0.5
      pijdef[2,1] <- 0.5
      pijdef[3,1] <- 1
    } else {
      # birth/death model
      pijdef <- function(i,j) {
        if(abs(i - j) == 1) {
          if( i > j) {
            return(0.6)
          } else {
            return(0.4)
          }
        }
        return(0)
      }
    }    
  } else {
    if(use_finite_state) {
      # machine repair
      pijdef <- matrix(rep(0,9), nrow=3)
      pijdef[1,1] <- 0.0
      pijdef[1,2] <- 1.0
      pijdef[1,3] <- 0.0
      pijdef[2,1] <- (1/20.0)/((1/20.0)+(1/8.0))
      pijdef[2,2] <- 0.0
      pijdef[2,3] <- (1/8.0)/((1/20.0)+(1/8.0))
      pijdef[3,2] <- 1.0
      
      qidef <- c(rep(0,3))
      qidef[1] = 0.25
      qidef[2] = 0.175
      qidef[3] = 0.08
    } else {
      # TODO: infinite, continuous case
    }    
  }
  
  markov_chain = mc(pijdef, qidef)
  if(calculate_stationary_dist) {
    stn(markov_chain, 0.1)
  } else {
    hit(markov_chain, 0.1)
  }
}