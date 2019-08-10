#' fastNaiveBayes
#'
#' This is an extremely fast implementation of a Naive Bayes classifier. This package is
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
#' @docType package
#' @name fastNaiveBayes
#' @import Matrix
NULL
