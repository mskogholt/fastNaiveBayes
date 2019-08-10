#' @title Fast Naive Bayes Classifier with a Gaussian event model
#' @description Extremely fast implementation of a Naive Bayes Classifier. This instance only uses the
#' Gaussian event model for all columns.
#'
#' @param x a numeric matrix. A sparse dgcMatrix is also accepted
#' @param y a factor of classes
#' @param sparse Use a sparse matrix? If true a sparse matrix will be constructed from x.
#'     It's possible to directly feed a sparse dgcMatrix as x, which will set this parameter to TRUE
#' @param ... Not used.
#'
#' @details A Naive Bayes classifier that assumes independence between the feature variables. The Gaussian distribution
#'     should be used with numerical variables.
#'
#'     By setting sparse = TRUE the numeric matrix x will be converted to a sparse dgcMatrix. This can be considerably faster
#'     in case few observations have a value different than 0.
#'
#'     It's also possible to directly supply a sparse dgcMatrix, which can be a lot faster in case a fastNaiveBayes model
#'     is trained multiple times on the same matrix or a subset of this. See examples for more details. Bear in mind that
#'     converting to a sparse matrix can actually be slower depending on the data.
#'
#' @return A fitted object of class "fastNaiveBayes.bernoulli". It has four components:
#'
#'     \describe{
#'         \item{probability_table}{Posterior probabilities}
#'         \item{priors}{calculated prior probabilities for each class}
#'         \item{names}{names of features used to train this fastNaiveBayes}
#'     }
#'
#' @export
#' @import Matrix
#' @examples
#' rm(list = ls())
#' library(fastNaiveBayes)
#' cars <- mtcars
#' y <- as.factor(ifelse(cars$mpg > 25, "High", "Low"))
#' x <- cars[, 2:ncol(cars)]
#'
#' dist <- fastNaiveBayes::fastNaiveBayes.detect_distribution(x, nrows = nrow(x))
#'
#' # Gaussian only
#' vars <- c("hp", dist$gaussian)
#' newx <- x[, vars]
#'
#' mod <- fastNaiveBayes.gaussian(newx, y)
#' pred <- predict(mod, newdata = newx)
#' mean(pred != y)
#' @seealso \code{\link{predict.fastNaiveBayes.gaussian}} for the predict function for the fastNaiveBayes.gaussian class,
#' \code{\link{fastNaiveBayes.mixed}} for the general fastNaiveBayes model, \code{\link{fastNaiveBayes.bernoulli}} for a Bernoulli
#' distribution only model, and finally, \code{\link{fastNaiveBayes.multinomial}} for a multinomial only distribution model.
#' @rdname fastNaiveBayes.gaussian
fastNaiveBayes.gaussian <- function(x, y, sparse = FALSE, ...) {
  UseMethod("fastNaiveBayes.gaussian")
}
