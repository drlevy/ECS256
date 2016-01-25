#' @title Create an example Pijdef matrix and calculate the stationary distribution or expected hitting times
#' @description The example matrix that is created reflects the following problems. For the finite case, we create a matrix that symbolizes flipping a coin and the probabilties of getting heads in a row. State 0 means we just got a tails. State 1 means we have 1 head. State 2 means we have gotten 2 heads in a row. This requires a 3x3 matrix since we have three states.
#'  The infinite case creates a function that represents an infinite matrix and a birth/death process of having the probability of 0.4 for birth and 0.6 for death.
#' All infinite case calculations are done with a convergence criterion of 0.1.
#' @param boolean that indicates if you want to use a finite state(TRUE) matrix or infinite(FALSE)
#' @param boolean that indicates if you want to calculate the stationary distrubtion(TRUE) or expected hitting times(FALSE)
#' @return THe staionary distribution or expected hitting times of the indicated matrix.
#' @export

testharness <- function(use_finite_state = TRUE, calculate_staionary_dist = TRUE)
{
  
  if(use_finite_state)
  {
    #three heads up game
    pijdef = matrix(rep(0,9), nrow=3)
    pijdef[1,1] <- 0.5
    pijdef[1,2] <- 0.5
    pijdef[2,3] <- 0.5
    pijdef[2,1] <- 0.5
    pijdef[3,1] <- 1
  }
  else
  {
    #birth/death model
    pijdef <- function(i,j)
    {
      if(abs(i - j) == 1)
      {
        if( i > j)
          return(0.6)
        else
          return(0.4)
      }
        
      
      return(0)
    }
  }
  
  
  markov_chain = mc(pijdef)
  if(calculate_staionary_dist == TRUE)
  {
    stn(markov_chain, 0.1)
  }
  else
  {
    hit(markov_chain, 0.1)
  }
}

