validateInput <- function(mc) {
  if (class(mc) != "mc") {
    stop("Input not of class type \"mc\".")
  } else if (is.null(mc$pijdef)) {
    stop("mc contains undefined pijdef.")
  } else if (!is.null(mc$qidef)) {
    if (class(mc$qidef) != "function") {
      if (NROW(mc$pijdef) != NROW(mc$qidef)) {
        stop("pijdef and qidef do not have an equal number of states.")
      } 
    }
  }
}

# TODO: Extra arguments for mFunc (i, j,...)?
infiniteHelper <- function(mFunc, cFunc, e) {
  n <- 10
  val <- c(rep(0, n))
  repeat {
    m <- matrix(rep(0, n*n), nrow = n)
    for (i in 1:n) {
      for (j in 1:n) {
        m[i, j] <- mFunc(i, j)
      }
    }
    newVal <- cFunc(m)
    if (dist(rbind(val, newVal[1:NROW(val)])) <= e) {
      return(newVal)
    }
    val <- newVal
    n <- 2*n
  }
}