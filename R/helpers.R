#' Slightly faster implementation of diagional elements of matrix product
#'
#' @param x Matrix
#' @param y Matrix
#'
`%diag%` <- function (x, y) {
  l = nrow(x)
  z = vector("numeric", l)
  for (i in 1:l) {
    z[i] = x[i,] %*% y[,i]
  }
  z
}

#' Predict convergence ETA
#'
#' @param change Vector of relative change in variable of interest
#' @param time Vector of time, relative or absolute
#' @param tolerance Tolerance for which convergence is assumed to be achieved
#' @param steps Steps to take into account for estimation
#' @importFrom utils tail
#'
predict_convergence_eta <- function(time, change, tolerance, steps = 5) {
  y <- log(tail(change,steps))
  time <- tail(time,steps)
  time <- (time - max(time))
  X <- matrix(c(rep(1,steps), time), ncol = 2)
  b <- solve(t(X) %*% X) %*% t(X) %*% y
  x <- max(0, round((log(tolerance) - b[1]) / b[2]))
  return(x)
}
