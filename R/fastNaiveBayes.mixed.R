#' @title Fast Naive Bayes Classifier for mixed Distributions
#' @description Extremely fast implementation of a Naive Bayes Classifier.
#'
#' @param x a numeric matrix, or a dgcMatrix
#' @param y a factor of classes
#' @param laplace A number used for Laplace smoothing. Default is 0
#' @param sparse Use a sparse matrix? If true a sparse matrix will be constructed from x.
#'     It's possible to directly feed a sparse dgcMatrix as x, which will set this parameter to TRUE
#' @param distribution A list with distribution names and column names to for which to use the distribution, see examples.
#' @param ... Not used.
#'
#' @details A Naive Bayes classifier that assumes independence between the feature variables. Currently, either a Bernoulli,
#'     multinomial, or Gaussian distribution can be used. The bernoulli distribution should be used when the features are 0 or 1 to
#'     indicate the presence or absence of the feature in each document. The multinomial distribution should be used when the
#'     features are the frequency that the feature occurs in each document. NA's are simply treated as 0. Finally, the Gaussian distribution
#'     should be used with numerical variables. By setting the distribution parameter a mix of different distributions can be used
#'     for different columns in the input matrix
#'
#'     By setting sparse = TRUE the numeric matrix x will be converted to a sparse dgcMatrix. This can be considerably faster
#'     in case few observations have a value different than 0.
#'
#'     It's also possible to directly supply a sparse dgcMatrix, which can be a lot faster in case a fastNaiveBayes model
#'     is trained multiple times on the same matrix or a subset of this. See examples for more details. Bear in mind that
#'     converting to a sparse matrix can actually be slower depending on the data.
#'
#' @return A fitted object of class "fastNaiveBayes". It has four components:
#'
#'     \describe{
#'         \item{models}{The fitted models, one for each distribution specified}
#'         \item{priors}{calculated prior probabilities for each class}
#'         \item{names}{names of features used to train this fastNaiveBayes}
#'         \item{distribution}{the distribution assumed for probability calculations and predictions}
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
#' # Mixed event models
#' dist <- fastNaiveBayes::fastNaiveBayes.detect_distribution(x, nrows = nrow(x))
#' print(dist)
#' mod <- fastNaiveBayes.mixed(x, y, laplace = 1)
#' pred <- predict(mod, newdata = x)
#' mean(pred != y)
#' @seealso \code{\link{predict.fastNaiveBayes.mixed}} for the predict function for a fastNaiveBayes class,
#' \code{\link{fastNaiveBayes.bernoulli}} for a Bernoulli only model, \code{\link{fastNaiveBayes.gaussian}} for a Gaussian
#' distribution only model, and finally, \code{\link{fastNaiveBayes.multinomial}} for a multinomial only distribution model.
#' @rdname fastNaiveBayes.mixed
fastNaiveBayes.mixed <- function(x,
                                 y,
                                 laplace = 0,
                                 sparse = FALSE,
                                 distribution = NULL, ...) {
  UseMethod("fastNaiveBayes.mixed")
}
