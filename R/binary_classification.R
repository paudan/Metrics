#' @title Inherit Documentation for Binary Classification Metrics
#' @name params_binary
#' @description This object provides the documentation for the parameters of functions
#'              that provide binary classification metrics
#' @param actual The ground truth binary numeric vector containing 1 for the positive
#'               class and 0 for the negative class.
#' @param predicted The predicted binary numeric vector containing 1 for the positive
#'                  class and 0 for the negative class. Each element represents the
#'                  prediction for the corresponding element in \code{actual}.
#' @param na.rm  Whether to remove NA/NaN values; if those are not removed, output will usually be \code{NA}
NULL

#' Area under the ROC curve (AUC)
#'
#' \code{auc} computes the area under the receiver-operator characteristic curve (AUC).
#'
#' \code{auc} uses the fact that the area under the ROC curve is equal to the probability
#' that a randomly chosen positive observation has a higher predicted value than a
#' randomly chosen negative value. In order to compute this probability, we can
#' calculate the Mann-Whitney U statistic. This method is very fast, since we
#' do not need to compute the ROC curve first.
#'
#' @inheritParams params_binary
#' @param predicted A numeric vector of predicted values, where the smallest values correspond
#'                  to the observations most believed to be in the negative class
#'                  and the largest values indicate the observations most believed
#'                  to be in the positive class. Each element represents the
#'                  prediction for the corresponding element in \code{actual}.
#' @export
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(0.9, 0.8, 0.4, 0.5, 0.3, 0.2)
#' auc(actual, predicted)
auc <- function(actual, predicted, na.rm = F) {
    if (length(actual) != length(predicted)) {
        msg <- "longer object length is not a multiple of shorter object length"
        warning(msg)
    }
    r <- rank(predicted)
    n_pos <- as.numeric(sum(actual == 1, na.rm = na.rm))
    n_neg <- length(actual) - n_pos
    return((sum(r[actual == 1], na.rm = na.rm) - n_pos * (n_pos + 1) / 2) / (n_pos * n_neg))
}

#' Log Loss
#'
#' \code{ll} computes the elementwise log loss between two numeric vectors.
#'
#' @inheritParams params_binary
#' @param predicted A numeric vector of predicted values, where the values correspond
#'                  to the probabilities that each observation in \code{actual}
#'                  belongs to the positive class
#' @export
#' @seealso \code{\link{logLoss}}
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(0.9, 0.8, 0.4, 0.5, 0.3, 0.2)
#' ll(actual, predicted)
ll <- function(actual, predicted, na.rm=F) {
  ind <- rep(T, length(actual))
  if (na.rm) {
    ind <- !(is.na(actual) | is.na(predicted))
  }
  actual <- actual[ind]
  predicted <- predicted[ind]
  score <- -(actual * log(predicted) + (1 - actual) * log(1 - predicted))
  score[actual == predicted] <- 0
  score[is.nan(score)] <- Inf
    return(score)
}

#' Mean Log Loss
#'
#' \code{logLoss} computes the average log loss between two numeric vectors.
#'
#' @inheritParams ll
#' @export
#' @seealso \code{\link{ll}}
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(0.9, 0.8, 0.4, 0.5, 0.3, 0.2)
#' logLoss(actual, predicted)
logLoss <- function(actual, predicted, na.rm = F) {
    return(mean(ll(actual, predicted), na.rm = na.rm))
}



#' Precision
#'
#' \code{precision} computes proportion of observations predicted to be in the
#'                  positive class (i.e. the element in \code{predicted} equals 1)
#'                  that actually belong to the positive class (i.e. the element
#'                  in \code{actual} equals 1)
#'
#' @inheritParams params_binary
#' @export
#' @seealso \code{\link{recall}} \code{\link{fbeta_score}}
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(1, 1, 1, 1, 1, 1)
#' precision(actual, predicted)
precision <- function(actual, predicted, na.rm = F) {
    return(mean(actual[predicted == 1], na.rm = na.rm))
}

#' Recall
#'
#' \code{recall} computes proportion of observations in the positive class
#'               (i.e. the element in \code{actual} equals 1) that are predicted
#'               to be in the positive class (i.e. the element in \code{predicted}
#'               equals 1)
#'
#' @inheritParams params_binary
#' @export
#' @seealso \code{\link{precision}} \code{\link{fbeta_score}}
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(1, 0, 1, 1, 1, 1)
#' recall(actual, predicted)
recall <- function(actual, predicted, na.rm = F) {
    return(mean(predicted[actual == 1], na.rm = na.rm))
}

#' F-beta Score
#'
#' \code{fbeta_score} computes a weighted harmonic mean of Precision and Recall.
#'                    The \code{beta} parameter controls the weighting.
#'
#' @inheritParams params_binary
#' @param beta A non-negative real number controlling how close the F-beta score is to
#'             either Precision or Recall. When \code{beta} is at the default of 1,
#'             the F-beta Score is exactly an equally weighted harmonic mean.
#'             The F-beta score will weight toward Precision when \code{beta} is less
#'             than one.  The F-beta score will weight toward Recall when \code{beta} is
#'             greater than one.
#' @export
#' @seealso \code{\link{precision}} \code{\link{recall}}
#' @examples
#' actual <- c(1, 1, 1, 0, 0, 0)
#' predicted <- c(1, 0, 1, 1, 1, 1)
#' recall(actual, predicted)
fbeta_score <- function(actual, predicted, beta = 1, na.rm = F) {
    prec <- precision(actual, predicted, na.rm = na.rm)
    rec <- recall(actual, predicted, na.rm = na.rm)
    return((1 + beta^2) * prec * rec / ((beta^2 * prec) + rec))
}
