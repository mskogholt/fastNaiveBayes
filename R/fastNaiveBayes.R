#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fastNaiveBayes <- function(x, y, priors = NULL, laplace = 0, sparse = FALSE, check = TRUE, distribution = fnb.detect_distribution(x)){
  UseMethod("fastNaiveBayes")
}

#' @import Matrix
#' @export
#' @rdname fastNaiveBayesF
fastNaiveBayes.default <- function(x, y, priors = NULL, laplace = 0, sparse = FALSE, check = TRUE, distribution = fnb.detect_distribution(x)){
  return(fnb.train(x, y, priors, laplace, sparse, check, distribution))
}
