#' Slightly faster implementation of diagonal elements of matrix product
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
#' `cast_variable()` converts data.tables to arrays and recursively processes nested lists.
#'
#' @return Vector/matrix/array
#'
#' @param x Data.table to be reshaped into vector/matrix/array with named dimensions if it is indeed a data.frame (i.e. data.table)
#' @param variable_order A character vector specifying the desired order of the variables (columns) before casting.
#'   Defaults to country x origin x destination x sector x input x output.
#'
cast_variable = function(
    x,
    variable_order = c("country","origin","destination","sector","input","output")
) {
  # 0) Atomic (character/numeric/logical/factor) or NULL → preserve as-is
  if (is.null(x) || is.atomic(x)) return(x)

  # 1) data.table → cast to array
  if (data.table::is.data.table(x)) {
    value <- NULL
    keep <- if (is.null(variable_order)) character(0) else intersect(variable_order, names(x))
    # Put requested variables first, then the rest
    x <- x[, c(keep, setdiff(names(x), keep)), with = FALSE]
    # Reverse sort by all index columns except the last (= value column) for correct array layout
    data.table::setorderv(x, rev(names(x)[-length(names(x) )]))
    return(array(
      data = x[, value],
      dim = dim.data.table(x),
      dimnames = dimnames.data.table(x)
    ))
  }

  # 2) Character vectors → reorder by variable_order without dropping others
  if (is.character(x) && length(x) > 1) {
    if (!is.null(variable_order)) {
      idx <- match(x, variable_order)                    # position in variable_order or NA
      ord <- order(is.na(idx), idx, x)                   # matched first (their order), then unmatched (alphabetical)
      x <- x[ord]
    } else {
      x <- x[order(x)]
    }
    return(x)
  }

  # 3) Plain lists → recurse (preserves any inner atomic vectors)
  if (is.list(x) && !is.data.frame(x)) {
    nms <- names(x)
    res <- lapply(x, function(el) cast_variable(el, variable_order))
    if (!is.null(nms)) names(res) <- nms
    return(res)
  }

  # 4) Anything else → return as-is
  x
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
      if (settings[['verbose']] >= 1L) cli_alert_warning(paste0(n, " is changed."))
      input[[n]] = model_scenario[[n]]
    } else {
      if (settings[['verbose']] >= 1L) cli_alert_warning(paste0(n, " is set."))
      input[[n]] = model_scenario[[n]]
    }
  }
  input
}

#' Get all dimensions of a model by inspecting initial conditions
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

#' Nest elasticity-related initial conditions under a dedicated list
#'
#' @param x List of initial conditions or scenario inputs
#'
nest_elasticity_variables <- function(x) {
  if (!is.list(x)) return(x)

  elasticity_fields <- c("trade_elasticity", "inputs", "factors", "intermediates", "consumption")
  present_fields <- elasticity_fields[elasticity_fields %in% names(x)]

  if (length(present_fields) > 0) {
    if (is.null(x[['elasticities']]) || !is.list(x[['elasticities']])) x[['elasticities']] <- list()

    for (field in present_fields) {
      if (is.null(x[['elasticities']][[field]])) {
        x[['elasticities']][[field]] <- x[[field]]
      }
      x[[field]] <- NULL
    }
  }

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
  if (is.list(x) && !is.data.frame(x)) {
    return(lapply(x, melt_variable))
  }
  if (is.array(x)) {
    if (length(dim(x)) == 2 & dim(x)[2] == 1) { # fix problem with one-dimensional arrays
        x = array(x, dim = dim(x)[1], dimnames = dimnames(x)[1])
    }
    return (setDT(as.data.frame.table(x, responseName = "value", stringsAsFactors = F)))
  } else {
    return (x)
  }
}

#' Extract selected output variables from input list
#'
#' @description
#' `output_variables()` returns a named list of selected elements from an input list,
#' where each element is accessed by name via `input[[var]]`.
#'
#' @return Named list
#'
#' @param input List containing model output elements
#' @param vars Character vector of variable names to extract
#'
output_variables <- function(input, vars) {
  setNames(lapply(vars, function(v) input[[v]]), vars)
}

#' Predict convergence ETA
#'
#' @param change_list A matrix with two columns:
#'   - First column: Time (relative or absolute).
#'   - Second column: Convergence criterion values.
#' @param tolerance Tolerance for which convergence is assumed to be achieved.
#'
#' @return Estimated time (in the same units as `change_list` time) to reach the `tolerance`,
#' or `"-"` if insufficient data.
#'
predict_convergence_eta <- function(change_list, tolerance) {
  if (nrow(change_list) < 3) return("-")
  model <- lm(log(change_list[-1, 2]) ~ I(change_list[-1, 1] - change_list[2, 1]))
  max(0, (log(tolerance) - coef(model)[1]) / coef(model)[2] - max(change_list[-1, 1] - change_list[2, 1]))
}

#' Format Time Difference
#'
#' @param start_time POSIXct object representing the start time.
#' @param end_time POSIXct object representing the end time.
#' @importFrom lubridate as.duration
#' @importFrom stringr str_pad str_c
#' @return A character string in the format "X:YY minutes" or "Z seconds".
#' @export
format_time_diff <- function(start_time, end_time) {
  time_diff <- as.duration(end_time - start_time)
  minutes <- as.integer(time_diff) %/% 60
  seconds <- as.integer(time_diff) %% 60

  if (minutes == 0) {
    return(str_c(seconds, " seconds"))
  } else {
    return(str_c(minutes, ":", str_pad(seconds, width = 2, pad = "0"), " minutes"))
  }
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

#' Check Convergence of Iterative Algorithm
#'
#' This function computes convergence criteria for iterative algorithms using
#' different methods that balance speed and accuracy.
#'
#' @param new_values A numeric vector or array containing the new iteration values.
#' @param old_values A numeric vector or array containing the previous iteration values.
#' @param method Character string specifying the convergence method. Options are:
#'   - `"root_mean_square"` (default): RMS of relative changes, good balance of speed and accuracy.
#'   - `"aggregate"`: Fast sum-based relative change, fastest but least accurate.
#'   - `"element_wise"`: Maximum relative change across all elements, most accurate but slowest.
#'   - `"sample"`: Maximum relative change on a random sample, faster than element-wise.
#' @param sample_fraction Numeric value between 0 and 1 specifying the fraction of elements
#'   to sample when `method = "sample"`. Default is 0.1 (ten percent).
#' @param min_sample Integer specifying minimum number of elements to sample when
#'   `method = "sample"`. Default is 10.
#'
#' @return A numeric value representing the convergence criterion.
#'
#' @examples
#' old_vals <- c(1.0, 2.0, 3.0, 4.0)
#' new_vals <- c(1.01, 2.02, 2.99, 4.01)
#'
#' # Different methods
#' check_convergence(new_vals, old_vals, method = "root_mean_square")
#' check_convergence(new_vals, old_vals, method = "aggregate")
#' check_convergence(new_vals, old_vals, method = "element_wise")
#' check_convergence(new_vals, old_vals, method = "sample", sample_fraction = 0.2)
#'
#' @export
check_convergence <- function(new_values, old_values,
                              method = "root_mean_square",
                              sample_fraction = 0.1,
                              min_sample = 10) {

  # Input validation
  if (length(new_values) != length(old_values)) {
    stop("new_values and old_values must have the same length")
  }

  if (!method %in% c("root_mean_square", "aggregate", "element_wise", "sample")) {
    stop("method must be one of: 'root_mean_square', 'aggregate', 'element_wise', 'sample'")
  }

  if (sample_fraction <= 0 || sample_fraction > 1) {
    stop("sample_fraction must be between 0 and 1")
  }

  # Convert to vectors for consistent handling
  new_values <- as.vector(new_values)
  old_values <- as.vector(old_values)

  # Handle different methods
  switch(method,

         "aggregate" = {
           # Fast sum-based convergence
           sum_new <- sum(new_values, na.rm = TRUE)
           sum_old <- sum(old_values, na.rm = TRUE)
           # Handle case where sums are both zero or both NA
           if (sum_old == 0 && sum_new == 0) {
             return(0)
           }
           if (is.na(sum_old) && is.na(sum_new)) {
             return(0)
           }
           abs((sum_new - sum_old) / max(abs(sum_old), .Machine$double.eps))
         },

         "root_mean_square" = {
           # RMS of relative changes
           rel_changes <- (new_values - old_values) / pmax(abs(old_values), .Machine$double.eps)
           # Handle case where all values are NA
           if (all(is.na(rel_changes))) {
             return(0)
           }
           result <- sqrt(mean(rel_changes^2, na.rm = TRUE))
           # Handle case where result is NaN (e.g., all rel_changes are NA after na.rm)
           if (is.nan(result)) {
             return(0)
           }
           result
         },

         "element_wise" = {
           # Maximum relative change across all elements
           rel_changes <- abs(new_values - old_values) / pmax(abs(old_values), .Machine$double.eps)
           # Handle case where all values are NA
           if (all(is.na(rel_changes))) {
             return(0)
           }
           result <- max(rel_changes, na.rm = TRUE)
           # Handle case where result is -Inf (all NA values)
           if (is.infinite(result) && result < 0) {
             return(0)
           }
           result
         },

         "sample" = {
           # Maximum relative change on a random sample
           n_total <- length(new_values)
           n_sample <- max(min_sample, round(n_total * sample_fraction))

           if (n_sample >= n_total) {
             # If sample size exceeds total, use all elements
             rel_changes <- abs(new_values - old_values) / pmax(abs(old_values), .Machine$double.eps)
             if (all(is.na(rel_changes))) {
               return(0)
             }
             result <- max(rel_changes, na.rm = TRUE)
             if (is.infinite(result) && result < 0) {
               return(0)
             }
             result
           } else {
             # Random sample
             idx <- sample(n_total, n_sample)
             rel_changes <- abs(new_values[idx] - old_values[idx]) / pmax(abs(old_values[idx]), .Machine$double.eps)
             if (all(is.na(rel_changes))) {
               return(0)
             }
             result <- max(rel_changes, na.rm = TRUE)
             if (is.infinite(result) && result < 0) {
               return(0)
             }
             result
           }
         }
  )
}

#' Sum Over Specified Dimensions of an Array
#'
#' Sums over the specified dimensions of an n-dimensional array and reshapes
#' the result back into an array with the correct dimensions.
#'
#' @param arr An n-dimensional array.
#' @param keep_dims An integer vector specifying the dimensions to keep after the summation.
#' @return An array with dimensions corresponding to `keep_dims`, containing the summed values.
#' @export
#' @examples
#' arr <- array(1:24, dim = c(2, 3, 4))
#' result <- array_sum(arr, c(1, 2))
array_sum <- function(arr, keep_dims) {
  if (!is.array(arr) && !is.matrix(arr)) {
    stop("arr must be an array.")
  }
  keep_dims <- as.integer(keep_dims)
  dims <- dim(arr)
  if (anyNA(keep_dims) || any(keep_dims < 1L) || any(keep_dims > length(dims))) {
    stop("keep_dims out of bounds.")
  }
  result <- apply(arr, keep_dims, sum)
  array(result, dim = dims[keep_dims], dimnames = dimnames(arr)[keep_dims])
}

#' Sweep Over Array Margins
#'
#' Applies an element-wise operation between an array and the supplied
#' statistics broadcast over the requested margins while preserving dimension
#' names. Matches the semantics of [base::sweep()].
#'
#' @param x Numeric array.
#' @param MARGIN Integer or character vector identifying the margins over which
#'   `STATS` should be swept. Character margins are matched against the names of
#'   `dimnames(x)`.
#' @param STATS Numeric array or vector that is broadcastable over `MARGIN`.
#' @param FUN Character scalar choosing the operation. Supported values are
#'   `"+"`, `"-"`, `"*"`, and `"/"`.
#'
#' @return An array with the same shape (and dimnames) as `x` containing the
#'   swept result.
#' @export
#'
#' @examples
#' x <- array(1:12, dim = c(3, 4))
#' dimnames(x) <- list(country = c("A", "B", "C"), sector = c("X", "Y", "Z", "W"))
#' stats <- c(1, 2, 3)
#' array_sweep(x, MARGIN = "country", STATS = stats, FUN = "-")
array_sweep <- function(x, MARGIN, STATS, FUN = "*") {
  if (!is.array(x)) {
    stop("x must be an array with dim().")
  }

  margins <- MARGIN
  if (is.character(margins)) {
    dn <- dimnames(x)
    if (is.null(dn) || is.null(names(dn))) {
      stop("Character MARGIN requires named dimnames(x).")
    }
    idx <- match(margins, names(dn))
    if (any(is.na(idx))) {
      stop("Failed to match all margin names in dimnames(x).")
    }
    margins <- idx
  }
  margins <- as.integer(margins)

  if (length(margins) == 0L || anyNA(margins)) {
    stop("MARGIN must contain valid dimensions.")
  }
  if (any(margins < 1L) || any(margins > length(dim(x)))) {
    stop("MARGIN out of bounds.")
  }
  if (anyDuplicated(margins)) {
    stop("MARGIN must not repeat.")
  }

  op <- FUN
  if (is.function(op)) {
    stop("FUN must be one of '+', '-', '*', '/' (function objects are not supported).")
  }
  if (length(op) != 1L) {
    stop("FUN must be a length-1 character vector.")
  }
  op <- as.character(op)
  if (!op %in% c("+", "-", "*", "/")) {
    stop("FUN must be one of '+', '-', '*', '/'.")
  }

  expected_dims <- dim(x)[margins]
  if (is.array(STATS)) {
    if (!identical(as.integer(dim(STATS)), as.integer(expected_dims))) {
      stop("dim(STATS) does not match the margins of x.")
    }
  } else if (length(STATS) != 1L && length(STATS) != prod(expected_dims)) {
    if (!(length(margins) == 1L && length(STATS) == expected_dims[1])) {
      stop("dim(STATS) does not match the margins of x.")
    }
  }

  if (!is.numeric(STATS)) {
    storage.mode(STATS) <- "double"
  }

  base::sweep(x, MARGIN = margins, STATS = STATS, FUN = match.fun(op))
}
