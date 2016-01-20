validateInput <- function(mc) {
  if (class(mc) != "mc") {
    stop("Input not of class type \"mc\".")
  } else if (is.null(mc$pijdef)) {
    stop("mc contains undefined pijdef.")
  }
}
