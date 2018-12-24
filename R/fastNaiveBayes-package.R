#' fastNaiveBayes
#'
#' Extremely fast and highly scalable implementation of a Naive Bayes Classifier. The package currently supports
#' both a Bernoulli distribution and a multinomial distribution, making it suitable for both binary features and
#' frequency counts. Only numerical variables are allowed, however, categorical variables can be transformed into dummies
#' and used with the Bernoulli distribution.
#'
#' This implementation most importantly offers a huge performance gain compared to the 'e1071' implementation in R.
#' The execution times were compared on a data set of tweets and was found to be 331 times faster. See the vignette for more
#' details
#'
#' @docType package
#' @name fastNaiveBayes
#' @import Matrix
NULL
