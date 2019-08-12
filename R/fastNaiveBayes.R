#' @description  This is an extremely fast implementation of a Naive Bayes classifier. This package is
#' currently the only package that supports a Bernoulli distribution, a Multinomial distribution,
#' and a Gaussian distribution, making it suitable for both binary features, frequency counts,
#' and numerical features. Another feature is the support of a mix of different event models.
#' Only numerical variables are allowed, however, categorical variables can be transformed into
#' dummies and used with the Bernoulli distribution.
#'
#' This implementation offers a huge performance gain compared to other implementations in R.
#' The execution times were compared on a data set of tweets and this package was found to be
#' around 283 to 34,841 times faster for the Bernoulli event models and 17 to 60 times faster
#' for the Multinomial model. For the Gaussian distribution this package was found to be
#' between 2.8 and 1679 times faster. See the vignette for more details.
#'
#' The implementation is largely based on the paper
#' "A comparison of event models for Naive Bayes anti-spam e-mail filtering" written by
#' K.M. Schneider (2003) <doi:10.3115/1067807>.
#'
#' Any issues can be submitted to: <https://github.com/mskogholt/fastNaiveBayes/issues>.
#'
#' For a complete list of functions, use library(help = "fastNaiveBayes").
#'
#' The easiest starting point is to use the fnb.train function. This will convert the data to a suitable format
#' and determine the distributions to use for each feature.
#'
#' @keywords internal
#' @rdname fastNaiveBayes-package
"_PACKAGE"

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
#' x <- cars[, 2:ncol(cars)]
#'
#' dist <- fnb.detect_distribution(x, nrows = nrow(x))
#'
#'
#' @seealso \code{\link{predict.fastNaiveBayes}} for the predict function for the fastNaiveBayes model.
#' @rdname fastNaiveBayes
fastNaiveBayes <- function(x, y, laplace = 0, sparse = FALSE, distribution = NULL, ...){
  UseMethod("fastNaiveBayes")
}

#' @export
#' @import Matrix
#' @rdname fastNaiveBayes
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
    stop('ý does not have enough levels to classify.')
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

