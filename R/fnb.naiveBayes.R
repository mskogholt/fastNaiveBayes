#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.naiveBayes <- function(x, y, laplace = 0, sparse = FALSE, distribution = NULL, ...){
  UseMethod("fastNaiveBayes")
}
