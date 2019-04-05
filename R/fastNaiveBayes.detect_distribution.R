#' @title Distribution Detection Function
#' @description Determines which distribution to use for which columns in the matrix based
#' on a set of rules.
#'
#' @param x a numeric matrix, or a dgcMatrix
#' @param nrows number of rows to use to detect distributions
#' @param ... Not used.
#'
#' @details A simple utility function to detect the distribution to use for each columns
#'
#' @return A list of distribution names mapped to column names
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
#' # Uses default of top 10 rows to determine distributions
#' dist <- fastNaiveBayes.detect_distribution(x)
#' print(dist)
#' 
#' # Uses all rows of x to determine distributions
#' dist <- fastNaiveBayes.detect_distribution(x, nrows = nrow(x))
#' print(dist)
#' @rdname fastNaiveBayes.detect_distribution
fastNaiveBayes.detect_distribution <- function(x, nrows = 10) {
  UseMethod("fastNaiveBayes.detect_distribution")
}
