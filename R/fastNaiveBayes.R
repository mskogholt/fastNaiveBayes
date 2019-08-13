#' @title Fast Naive Bayes Classifier for different Distributions
#' @description Extremely fast implementation of a Naive Bayes Classifier.
#'
#' @param x a numeric matrix, or a dgcMatrix
#' @param y a factor of classes
#' @param laplace A number used for Laplace smoothing. Default is 0
#' @param sparse Use a sparse matrix? If true a sparse matrix will be constructed from x.
#'     It's possible to directly feed a sparse dgcMatrix as x, which will set this parameter to TRUE
#' @param distribution A list with distribution names and column names for which to use the distribution, see examples.
#' @param ... Not used.
#'
#' @details A Naive Bayes classifier that assumes independence between the feature variables. Currently, either a Bernoulli,
#'     multinomial, or Gaussian distribution can be used. The bernoulli distribution should be used when the features are 0 or 1 to
#'     indicate the presence or absence of the feature in each document. The multinomial distribution should be used when the
#'     features are the frequency that the feature occurs in each document. Finally, the Gaussian distribution
#'     should be used with numerical variables. fnb.mixed uses the distribution parameter to mix different distributions
#'     for different columns in the input matrix
#'
#'     By setting sparse = TRUE the numeric matrix x will be converted to a sparse dgcMatrix. This can be considerably faster
#'     in case few observations have a value different than 0.
#'
#'     It's also possible to directly supply a sparse dgcMatrix, which can be a lot faster in case a fastNaiveBayes model
#'     is trained multiple times on the same matrix or a subset of this. See examples for more details. Bear in mind that
#'     converting to a sparse matrix can actually be slower depending on the data.
#'
#' @return A fitted object of class "fastNaiveBayes". It has three components:
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
#' x <- cars[,2:ncol(cars)]
#'
#' dist <- fnb.detect_distribution(x, nrows = nrow(x))
#'
#'
#' @seealso \code{\link{predict.fastNaiveBayes}} for the predict function for the fastNaiveBayes model.
#' @rdname fastNaiveBayesF
fastNaiveBayes <- function(x, y, laplace = 0, sparse = FALSE, distribution = NULL, ...){
  UseMethod("fastNaiveBayes")
}

#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fastNaiveBayes.default <- function(x, y, laplace = 0, sparse = FALSE, distribution = NULL, ...){

  if (class(x)[1] != "dgCMatrix") {
    if (!is.matrix(x)) {
      x <- as.data.frame(x)

      classes <- sapply(x, class)
      cols <- colnames(x)[which(classes!='numeric' & classes!='factor')]
      x[cols] <- lapply(x[cols] , factor)

      x <- stats::model.matrix(y ~ . -1, cbind(y, x))
      x <- as.matrix(x)
    }
    if (sparse) {
      x <- Matrix(x, sparse = TRUE)
    }
  } else {
    sparse <- TRUE
  }
  if(!is.factor(y)){
    y <- as.factor(y)
  }
  if(nlevels(y)<=1){
    stop('y does not have enough levels to classify.')
  }
  if(nrow(x)!=length(y)){
    stop('Rows of x not equal to length of y')
  }

  if(ncol(x)<1){
    stop('x seems to be empty')
  }
  if(any(is.na(x))){
    warning("x contains na's. These will be set to 0")
    x[is.na(x)] <- 0
  }
  if(any(is.na(y))){
    warning("y contains na's. These observations will be removed")
    x <- x[!is.na(y),]
    y <- y[!is.na(y)]
  }
  if(any(rowsum(rep(1,times = length(y)), y)<2)){
    stop('Not enough rows. Should be at least 2 rows or more for each class')
  }

  if(is.null(distribution)){
    distribution <- fnb.detect_distribution(x)
  }

  mod <- fnb.mixed(x, y, laplace, sparse, distribution = distribution)
  structure(list(
    model = mod,
    names = colnames(x),
    distribution = distribution,
    levels = levels(y)),

    class = "fastNaiveBayes"
  )
}

