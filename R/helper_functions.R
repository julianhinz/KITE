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

#' Cast variable from data.tables to vectors, matrices and arrays
#'
#' @description
#' `cast_variable()` updates the equilibrium to a counterfactual situation with new trade costs and/or other changes.
#'
#' @return Vector/matrix/array
#'
#' @param x Data.table to be reshaped into vector/matrix/array with named dimensions if it is indeed a data.frame (i.e. data.table)
#'
cast_variable = function (x) {
  if (is.data.table(x)) {
    value = NULL
    setorderv(x, rev(names(x)[-length(x)])) # reverse sort for correct array
    array(data = x[, value],
          dim = dim.data.table(x),
          dimnames = dimnames.data.table(x))
  } else {
    x = x[order(x)]
  }
}

#' Dimensions of a data.table
#'
#' @param d data.table
#'
dim.data.table = function (d) {
  r = integer()
  for (n in names(d)[-length(d)]) r = c(r, d[, uniqueN(get(n))])
  r
}

#' Dimension names of a data.table
#'
#' @param d data.table
#'
dimnames.data.table = function (d) {
  r = list()
  for (n in names(d)[-length(d)]) r[[n]] = d[, unique(get(n))]
  r
}

#' Generate inputs from initial and counterfactual conditions
#'
#' @param initial_conditions List of initial conditions, i.e. named data.tables
#' @param model_scenario List of counterfactual conditions, i.e. named data.tables
#' @param settings List of settings
#'

generate_input = function (initial_conditions, model_scenario, settings) {
  input = initial_conditions
  for (n in names(model_scenario)) {
    if (n %in% names(initial_conditions)) {
      if (settings$verbose >= 1L) cli_alert_warning(paste0(n, " is changed."))
      input[[n]] = model_scenario[[n]]
    } else {
      if (settings$verbose >= 1L) cli_alert_warning(paste0(n, " is set."))
      input[[n]] = model_scenario[[n]]
    }
  }
  input
}

#' Get all dimensions of model by inspecting initial conditions
#'
#' @param x List of data.tables of initial conditions
#'
get_model_dimensions <- function (x) {
  names(x) = NULL
  x = lapply(x, c)
  x = unlist(x, recursive = F)
  x = lapply(x, unique)
  x = x[names(x) != "value"]
  x = x[!duplicated(names(x))]
  x
}

#' Initialize variable as array
#'
#' @description
#' `initialize_variable()` Initializes a variable.
#'
#' @return Array
#'
#' @param dims Dimensions of variable
#' @param value Default value
#'
initialize_variable = function (dims, value = 1) {
  array(data = value,
        dim = unlist(lapply(dims, length)),
        dimnames = dims)
}

#' Melt variable from vectors, matrices and arrays, to data.tables
#'
#' @description
#' `melt_variable()`
#'
#' @return Data.table
#'
#' @param x vector/matrix/array to data.table
#'
melt_variable = function (x) {
  if (is.array(x)) {
    if (length(dim(x)) == 2 & dim(x)[2] == 1) { # fix problem with one-dimensional arrays
        x = array(x, dim = dim(x)[1], dimnames = dimnames(x)[1])
    }
    return (setDT(as.data.frame.table(x, responseName = "value", stringsAsFactors = F)))
  } else {
    return (x)
  }
}

#' Predict convergence ETA
#'
#' @param change Vector of relative change in variable of interest
#' @param time Vector of time, relative or absolute
#' @param tolerance Tolerance for which convergence is assumed to be achieved
#' @param steps Steps to take into account for estimation
#' @importFrom utils tail
#'
predict_convergence_eta <- function (time, change, tolerance, steps = 5) {
  y <- log(tail(change,steps))
  time <- tail(time,steps)
  time <- (time - max(time))
  X <- matrix(c(rep(1,steps), time), ncol = 2)
  b <- solve(t(X) %*% X) %*% t(X) %*% y
  x <- max(0, round((log(tolerance) - b[1]) / b[2]))
  return(x)
}

#' Create one-dimensional array from vector
#'
#' @param x vector
#' @param dimname dimname
#'
vector_to_array = function (x, dimname = NULL) {
  x = as.array(x)
  dimnames_name = dimnames(x)
  names(dimnames_name) = dimname
  dimnames(x) = dimnames_name
  x
}
