#' @title Inherit Documentation for Regression Metrics
#' @name params_regression
#' @description This object provides the documentation for the parameters of functions
#'              that provide regression metrics
#' @param actual The ground truth numeric vector.
#' @param predicted The predicted numeric vector, where each element in the vector
#'                  is a prediction for the corresponding element in \code{actual}.
#' @param na.rm  Whether to remove NA/NaN values; if those are not removed, output will usually be \code{NA}
NULL

#' Bias
#'
#' \code{bias} computes the average amount by which \code{actual} is greater than
#' \code{predicted}.
#'
#' If a model is unbiased \code{bias(actual, predicted)} should be close to zero.
#' Bias is calculated by taking the average of (\code{actual} - \code{predicted}).
#'
#' @inheritParams params_regression
#' @export
#' @seealso \code{\link{percent_bias}}
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' bias(actual, predicted)
bias <- function(actual, predicted, na.rm = F) {
  ind <- rep(T, length(actual))
  if (na.rm) {
    ind <- !(is.na(actual) | is.na(predicted))
  }
  return(mean(actual[ind] - predicted[ind], na.rm = na.rm))
}

#' Percent Bias
#'
#' \code{percent_bias} computes the average amount that \code{actual} is greater
#' than \code{predicted} as a percentage of the absolute value of \code{actual}.
#'
#' If a model is unbiased \code{percent_bias(actual, predicted)} should be close
#' to zero. Percent Bias is calculated by taking the average of
#' (\code{actual} - \code{predicted}) / \code{abs(actual)} across all observations.
#'
#' \code{percent_bias} will give \code{-Inf}, \code{Inf}, or \code{NaN}, if any
#' elements of \code{actual} are \code{0}.
#'
#' @inheritParams params_regression
#' @param zero.rm  If set to \code{True}, will remove \code{actual} values which are 0.
#' Might be useful if at least some approximation is required when \code{actual} values contain 0
#' @export
#' @seealso \code{\link{bias}}
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' percent_bias(actual, predicted)
percent_bias <- function(actual, predicted, na.rm = F, zero.rm = F) {
  ind <- rep(T, length(actual))
  if (na.rm) {
    ind <- !(is.na(actual) | is.na(predicted))
  }
  if (zero.rm) {
    ind <- ind & actual != 0
  }
  return(mean((actual[ind] - predicted[ind]) / abs(actual[ind]), na.rm = na.rm))
}

#' Squared Error
#'
#' \code{se} computes the elementwise squared difference between two numeric vectors.
#'
#' @inheritParams params_regression
#' @export
#' @seealso \code{\link{mse}} \code{\link{rmse}}
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' se(actual, predicted)
se <- function(actual, predicted, na.rm = F) {
  ind <- rep(T, length(actual))
  if (na.rm) {
    ind <- !(is.na(actual) | is.na(predicted))
  }
  return((actual[ind] - predicted[ind]) ^ 2)
}

#' Sum of Squared Errors
#'
#' \code{sse} computes the sum of the squared differences between two numeric vectors.
#'
#' @inheritParams params_regression
#' @export
#' @seealso \code{\link{mse}}
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' sse(actual, predicted)
sse <- function(actual, predicted, na.rm = F) {
    return(sum(se(actual, predicted, na.rm = na.rm)))
}

#' Mean Squared Error
#'
#' \code{mse} computes the average squared difference between two numeric vectors.
#'
#' @inheritParams params_regression
#' @export
#' @seealso \code{\link{rmse}} \code{\link{mae}}
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' mse(actual, predicted)
mse <- function(actual, predicted, na.rm = F) {
    return(mean(se(actual, predicted, na.rm = na.rm)))
}

#' Root Mean Squared Error
#'
#' \code{rmse} computes the root mean squared error between two numeric vectors
#'
#' @inheritParams params_regression
#' @export
#' @seealso \code{\link{mse}}
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' rmse(actual, predicted)
rmse <- function(actual, predicted, na.rm = F) {
    return(sqrt(mse(actual, predicted, na.rm = na.rm)))
}

#' Absolute Error
#'
#' \code{ae} computes the elementwise absolute difference between two numeric vectors.
#'
#' @inheritParams params_regression
#' @export
#' @seealso \code{\link{mae}} \code{\link{mdae}} \code{\link{mape}}
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' ae(actual, predicted)
ae <- function(actual, predicted, na.rm = F) {
  ind <- rep(T, length(actual))
  if (na.rm) {
    ind <- !(is.na(actual) | is.na(predicted))
  }
  return(abs(actual[ind] - predicted[ind]))
}

#' Mean Absolute Error
#'
#' \code{mae} computes the average absolute difference between two numeric vectors.
#'
#' @inheritParams params_regression
#' @export
#' @seealso \code{\link{mdae}} \code{\link{mape}}
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' mae(actual, predicted)
mae <- function(actual, predicted, na.rm = F) {
    return(mean(ae(actual, predicted, na.rm = na.rm)))
}

#' Median Absolute Error
#'
#' \code{mdae} computes the median absolute difference between two numeric vectors.
#'
#' @importFrom stats median
#' @inheritParams params_regression
#' @export
#' @seealso \code{\link{mae}} \code{\link{mape}}
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' mdae(actual, predicted)
mdae <- function(actual, predicted, na.rm = F) {
    return(stats::median(ae(actual, predicted, na.rm = na.rm)))
}

#' Absolute Percent Error
#'
#' \code{ape} computes the elementwise absolute percent difference between two numeric
#' vectors
#'
#' \code{ape} is calculated as (\code{actual} - \code{predicted}) / \code{abs(actual)}.
#' This means that the function will return \code{-Inf}, \code{Inf}, or \code{NaN}
#' if \code{actual} is zero.
#'
#' @inheritParams params_regression
#' @param zero.rm  If set to \code{True}, will remove \code{actual} values which are 0.
#' Might be useful if at least some approximation is required when \code{actual} values contain 0
#' @export
#' @seealso \code{\link{mape}} \code{smape}
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' ape(actual, predicted)
ape <- function(actual, predicted, na.rm = F, zero.rm = F) {
  ind <- rep(T, length(actual))
  if (na.rm) {
    ind <- !(is.na(actual) | is.na(predicted))
  }
  if (zero.rm) {
    ind <- ind & actual != 0
  }
  return(ae(actual[ind], predicted[ind], na.rm = F) / abs(actual[ind]))
}

#' Mean Absolute Percent Error
#'
#' \code{mape} computes the average absolute percent difference between two numeric vectors.
#'
#' \code{mape} is calculated as the average of (\code{actual} - \code{predicted}) / \code{abs(actual)}.
#' This means that the function will return \code{-Inf}, \code{Inf}, or \code{NaN}
#' if \code{actual} is zero. Due to the instability at or near zero, \code{smape} or
#' \code{mase} are often used as alternatives.
#'
#' @inheritParams params_regression
#' @param zero.rm  If set to \code{True}, will remove actual values which are 0.
#' Might be useful if at least some approximation is required when \code{actual} values contain 0
#' @export
#' @seealso \code{\link{mae}} \code{\link{smape}} \code{\link{mase}}
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' mape(actual, predicted)
mape <- function(actual, predicted, na.rm = F, zero.rm = F) {
    return(mean(ape(actual, predicted, na.rm = na.rm, zero.rm = zero.rm)))
}

#' Symmetric Mean Absolute Percentage Error
#'
#' \code{smape} computes the symmetric mean absolute percentage error between
#' two numeric vectors.
#'
#' \code{smape} is defined as two times the average of \code{abs(actual - predicted) / (abs(actual) + abs(predicted))}.
#' Therefore, at the elementwise level, it will provide \code{NaN} only if \code{actual} and \code{predicted}
#' are both zero. It has an upper bound of \code{2}, when either \code{actual} or
#' \code{predicted} are zero or when \code{actual} and \code{predicted} are opposite
#' signs.
#'
#' \code{smape} is symmetric in the sense that \code{smape(x, y) = smape(y, x)}.
#'
#' @inheritParams params_regression
#' @param zero.rm  If set to \code{True}, will remove zero values of both \code{actual} and \code{predicted}.
#' Might be useful if at least some approximation is required when \code{actual} values contain 0
#' @export
#' @seealso \code{\link{mape}} \code{\link{mase}}
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' smape(actual, predicted)
smape <- function(actual, predicted, na.rm = F, zero.rm = F) {
  ind <- rep(T, length(actual))
  if (na.rm) {
    ind <- !(is.na(actual) | is.na(predicted))
  }
  if (zero.rm) {
    ind <- ind & !(actual == 0 & predicted == 0)
  }
  return(2 * mean(ae(actual[ind], predicted[ind]) / (abs(actual[ind]) + abs(predicted[ind]))))
}

#' Squared Log Error
#'
#' \code{sle} computes the elementwise squares of the differences in the logs of two numeric vectors.
#'
#' \code{sle} adds one to both \code{actual} and \code{predicted} before taking
#' the natural logarithm of each to avoid taking the natural log of zero. As a result,
#' the function can be used if \code{actual} or \code{predicted} have zero-valued
#' elements. But this function is not appropriate if either are negative valued.
#'
#' @param actual The ground truth non-negative vector
#' @param predicted The predicted non-negative vector, where each element in the vector
#'                  is a prediction for the corresponding element in \code{actual}.
#' @export
#' @seealso \code{\link{msle}} \code{\link{rmsle}}
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' sle(actual, predicted)
sle <- function(actual, predicted, na.rm = F) {
  ind <- rep(T, length(actual))
  if (na.rm) {
    ind <- !(is.na(actual) | is.na(predicted))
  }
  return((log(1 + actual[ind]) - log(1 + predicted[ind])) ^ 2)
}

#' Mean Squared Log Error
#'
#' \code{msle} computes the average of squared log error between two numeric vectors.
#'
#' \code{msle} adds one to both \code{actual} and \code{predicted} before taking
#' the natural logarithm to avoid taking the natural log of zero. As a result,
#' the function can be used if \code{actual} or \code{predicted} have zero-valued
#' elements. But this function is not appropriate if either are negative valued.
#'
#' @inheritParams sle
#' @export
#' @seealso \code{\link{rmsle}} \code{\link{sle}}
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' msle(actual, predicted)
msle <- function(actual, predicted, na.rm = F) {
    mean(sle(actual, predicted, na.rm = na.rm))
}

#' Root Mean Squared Log Error
#'
#' \code{rmsle} computes the root mean squared log error between two numeric vectors.
#'
#' \code{rmsle} adds one to both \code{actual} and \code{predicted} before taking
#' the natural logarithm to avoid taking the natural log of zero. As a result,
#' the function can be used if \code{actual} or \code{predicted} have zero-valued
#' elements. But this function is not appropriate if either are negative valued.
#'
#' @inheritParams sle
#' @export
#' @seealso \code{\link{msle}} \code{\link{sle}}
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' rmsle(actual, predicted)
rmsle <- function(actual, predicted, na.rm = F) {
    sqrt(msle(actual, predicted, na.rm = na.rm))
}

#' Relative Squared Error
#'
#' \code{rse} computes the relative squared error between two numeric vectors.
#'
#' \code{rse} divides \code{sse(actual, predicted)} by \code{sse(actual, mean(actual))},
#' meaning that it provides the squared error of the predictions relative to a naive model that
#' predicted the mean for every data point.
#'
#' @inheritParams params_regression
#' @export
#' @seealso \code{\link{rrse}} \code{\link{rae}}
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' rse(actual, predicted)
rse <- function (actual, predicted, na.rm = F) {
  ind <- rep(T, length(actual))
  if (na.rm) {
    ind <- !(is.na(actual) | is.na(predicted))
  }
  return(sse(actual[ind], predicted[ind]) / sse(actual[ind], rep(mean(actual[ind]), length(actual[ind]))))
}

#' Root Relative Squared Error
#'
#' \code{rrse} computes the root relative squared error between two numeric vectors.
#'
#' \code{rrse} takes the square root of \code{sse(actual, predicted)} divided by
#' \code{sse(actual, mean(actual))}, meaning that it provides the squared error of the
#' predictions relative to a naive model that predicted the mean for every data point.
#'
#' @inheritParams params_regression
#' @export
#' @seealso \code{\link{rse}} \code{\link{rae}}
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' rrse(actual, predicted)
rrse <- function(actual, predicted, na.rm = F) {
    return(sqrt(rse(actual, predicted, na.rm = na.rm)))
}

#' Relative Absolute Error
#'
#' \code{rae} computes the relative absolute error between two numeric vectors.
#'
#' \code{rae} divides \code{sum(ae(actual, predicted))} by \code{sum(ae(actual, mean(actual)))},
#' meaning that it provides the absolute error of the predictions relative to a naive model that
#' predicted the mean for every data point.
#'
#' @inheritParams params_regression
#' @export
#' @seealso \code{\link{rse}} \code{\link{rrse}}
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' rae(predicted, actual)
rae <- function(actual, predicted, na.rm = F) {
  ind <- rep(T, length(actual))
  if (na.rm) {
    ind <- !(is.na(actual) | is.na(predicted))
  }
  return(sum(ae(actual[ind], predicted[ind])) / sum(ae(actual[ind], rep(mean(actual[ind]), length(actual[ind])))))
}

#' Explained Variation
#'
#' \code{explained_variation} computes the percentage of variation in one numeric vector
#' explained by another, also known as the coefficient of determination.
#'
#' \code{explained_variation} subtracts the relative squared error, \code{rse(actual, predicted)},
#' from 1, meaning it can return negative values if the predictions are on average further from
#' the actual values than predictions from a naive model that predicts the mean for every data
#' point.
#'
#' @inheritParams params_regression
#' @export
#' @seealso \code{\link{rse}}
#' @examples
#' actual <- c(1.1, 1.9, 3.0, 4.4, 5.0, 5.6)
#' predicted <- c(0.9, 1.8, 2.5, 4.5, 5.0, 6.2)
#' explained_variation(actual, predicted)
explained_variation <- function(actual, predicted, na.rm = F) {
    1 - rse(actual, predicted, na.rm = na.rm)
}
