#' @title Fast Naive Bayes Classifier for different Distributions
#' @description Extremely fast implementation of a Naive Bayes Classifier.
#'
#' A Naive Bayes classifier that assumes independence between the feature variables. Currently, either a Bernoulli,
#' multinomial, or Gaussian distribution can be used. The bernoulli distribution should be used when the features are 0 or 1 to
#' indicate the presence or absence of the feature in each document. The multinomial distribution should be used when the
#' features are the frequency that the feature occurs in each document. Finally, the Gaussian distribution
#' should be used with numerical variables. The distribution parameter is used to mix different distributions
#' for different columns in the input matrix
#'
#' Use fastNaiveBayes(...) or fnb.train(...) for a mixed event distribution model. fnb.bernoulli, fnb.multinomial, fnb.gaussian and
#' for the specific distributions
#'
#' @param x a numeric matrix, or a dgcMatrix. For bernoulli should only contain 0's and 1's. For multinomial should only
#' contain integers.
#' @param y a factor of classes to classify
#' @param priors a numeric vector with the priors. If left empty the priors will be determined by the relative frequency of the classes in the data
#' @param laplace A number used for Laplace smoothing. Default is 0
#' @param sparse Use a sparse matrix? If true a sparse matrix will be constructed from x.
#'     It's possible to directly feed a sparse dgcMatrix as x, which will set this parameter to TRUE
#' @param check Whether to enable formal checks on input. Recommended to set to TRUE. Set to FALSE will make it faster, but at your own risk.
#' @param distribution A list with distribution names and column names for which to use the distribution, see examples.
#'
#' @details
#' fastNaiveBayes(...) will convert non numeric columns to one hot encoded features to use with the Bernoulli event
#' model. NA's in x will be set to 0 by default and observations with NA in y will be removed.
#'
#' The distribution that is used for each feature is determined by a set of rules:
#' - if the column only contains 0's and 1's a Bernoulli event model will be used
#' - if the column only contains whole numbers a Multinomial event model will be used
#' - if none of the above a Gaussian event model will be used.
#'
#' By setting sparse = TRUE the numeric matrix x will be converted to a sparse dgcMatrix. This can be considerably faster
#' in case few observations have a value different than 0.
#'
#' It's also possible to directly supply a sparse dgcMatrix, which can be a lot faster in case a fastNaiveBayes model
#' is trained multiple times on the same matrix or a subset of this. See examples for more details. Bear in mind that
#' converting to a sparse matrix can actually be slower depending on the data.
#'
#' @return A fitted object of class "fastNaiveBayes". It has four components:
#'
#'     \describe{
#'         \item{model}{Fitted fastNaiveBayes model}
#'         \item{names}{Names of features used to train this fastNaiveBayes model}
#'         \item{distribution}{Distribution used for each column of x}
#'         \item{levels}{Levels of y}
#'     }
#' @export
#' @import Matrix
#' @examples
#' rm(list = ls())
#' library(fastNaiveBayes)
#' cars <- mtcars
#' y <- as.factor(ifelse(cars$mpg > 25, "High", "Low"))
#' x <- cars[,2:ncol(cars)]
#'
#' mod <- fastNaiveBayes(x, y, laplace = 1)
#'
#' pred <- predict(mod, newdata = x)
#' mean(y!=pred)
#'
#' mod <- fnb.train(x, y, laplace = 1)
#'
#' pred <- predict(mod, newdata = x)
#' mean(y!=pred)
#'
#' dist <- fnb.detect_distribution(x)
#'
#' bern <- fnb.bernoulli(x[,dist$bernoulli], y, laplace = 1)
#' pred <- predict(bern, x[,dist$bernoulli])
#' mean(y!=pred)
#'
#' mult <- fnb.multinomial(x[,dist$multinomial], y, laplace = 1)
#' pred <- predict(mult, x[,dist$multinomial])
#' mean(y!=pred)
#'
#' gauss <- fnb.gaussian(x[,dist$gaussian], y)
#' pred <- predict(gauss, x[,dist$gaussian])
#' mean(y!=pred)
#'
#' @seealso \code{\link{predict.fastNaiveBayes}} for the predict function for the fastNaiveBayes model.
#' @rdname fastNaiveBayesF
fnb.train <- function(x, y, priors = NULL, laplace = 0, sparse = FALSE, check = TRUE, distribution = fnb.detect_distribution(x)) {
  UseMethod("fnb.train")
}

#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.train.default <- function(x, y, priors = NULL, laplace = 0, sparse = FALSE, check = TRUE, distribution = fnb.detect_distribution(x)) {
  if(check){
    args <- fnb.check.args.model(x, y, priors, sparse)
    x <- args$x
    y <- args$y
    priors <- args$priors
    sparse <- args$sparse

    # distribution
    if (!is.null(distribution)) {
      if(!is.list(distribution)){
        stop('distribution should be a list with distribution names and column names corresponding to x, see details.')
      }

      distribution <- distribution[lengths(distribution) != 0]
      if(!any(c("bernoulli","multinomial","gaussian") %in% names(distribution))){
        stop('Not a single accepted distribution was specified or all were empty')
      }

      if(any(!names(distribution) %in% c("bernoulli","multinomial","gaussian"))){
        warning('Redundant distribution specified, will be removed')
        distribution <- distribution[names(distribution) %in% c("bernoulli","multinomial","gaussian")]
      }
    }
  }

  models <- lapply(names(distribution), function(dist) {
    switch(dist,
           bernoulli = {
             newx <- x[, distribution[[dist]], drop=FALSE]
             fnb.bernoulli(newx, y, priors, laplace, sparse)
           },
           multinomial = {
             newx <- x[, distribution[[dist]], drop=FALSE]
             fnb.multinomial(newx, y, priors, laplace, sparse)
           },
           gaussian = {
             newx <- x[, distribution[[dist]], drop=FALSE]
             fnb.gaussian(newx, y, priors, sparse)
           }
    )
  })

  if(is.null(priors)){
    priors <- tabulate(y) / nrow(x)
  }

  structure(list(
    models = models,
    priors = priors,
    names = colnames(x),
    distribution = distribution,
    levels = levels(y)),

    class = "fastNaiveBayes"
  )
}

#' @title Predict Method for fastNaiveBayes fits
#' @description Uses a fastNaiveBayes model and a new data set to create the classifications.
#'     This can either be the raw probabilities generated by the fastNaiveBayes model or the classes themselves.
#' @param object A fitted object of class "fnb.bernoulli", "fnb.gaussian", "fnb.multinomial" or "fastNaiveBayes".
#' @param newdata A numeric matrix. A Sparse dgcMatrix is also accepted.
#'     Note: if fnb.naiveBayes or fnb.gaussian or fnb.train was used to create the model, then if newdata contains features that were not encountered in the training data, these are omitted from the prediction.
#'     Furthermore, newdata can contain fewer features than encountered in the training data. In this case, newdata will be padded with
#'     extra columns all filled with 0's.
#' @param type If "raw", the conditional a-posterior probabilities for each class are returned, and the class with maximal probability else.
#' @param sparse Use a sparse matrix? If true a sparse matrix will be constructed from x.
#'     It's possible to directly feed a sparse dgcMatrix as x, which will set this parameter to TRUE
#' @param threshold A threshold for the minimum probability. For Bernoulli and Multinomial event models Laplace smoothing should solve this,
#' but in the case of Gaussian event models, this ensures numerical probabilities.
#' @param check Whether to perform formal checks on the input. Set to false, if this is not necessary and speed is of the essence
#' @param ... not used
#'
#' @return If type = 'class', a factor with classified class levels. If type = 'raw', a matrix with the predicted probabilities of
#'     each class, where each column in the matrix corresponds to a class level.
#' @seealso \code{\link{fastNaiveBayes}} for the fastNaiveBayes model
#' @export
#' @import Matrix
#' @rdname predict.fastNaiveBayes
predict.fastNaiveBayes <- function(object, newdata, type = c("class", "raw"), sparse = FALSE,
                                   threshold = .Machine$double.eps, check = TRUE, ...) {

  type <- match.arg(type)
  if(check){
    args <- fnb.check.args.predict(object, newdata, type, sparse, threshold)
    object <- args$object
    newdata <- args$newdata
    type <- args$type
    sparse <- args$sparse
    threshold <- args$threshold


  }

  probs <- NULL
  for (i in 1:length(object$models)) {
    model <- object$models[[i]]
    if (is.null(probs)) {
      probs <- stats::predict(model, newdata, type = "rawprob", sparse, threshold, silent = TRUE)
    } else {
      probs <- probs + stats::predict(model, newdata, type = "rawprob", sparse, threshold, silent = TRUE)
    }
  }
  probs <- exp(probs)

  priors <- object$priors
  for(i in 1:length(priors)){
    probs[,i] <- probs[,i]*priors[i]
  }

  denom <- rowSums(probs)
  denom[denom==0] <- 1
  probs <- probs / denom

  if (type == "class") {
    class <- as.factor(object$levels[max.col(probs, ties.method = "first")])
    levels(class) <- object$levels
    return(class)
  }
  return(probs)
}
