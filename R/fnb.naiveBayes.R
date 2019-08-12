#' @export
#' @import Matrix
#' @rdname fastNaiveBayes
fnb.naiveBayes <- function(x, y, laplace = 0, sparse = FALSE, distribution = NULL, ...){
  UseMethod("fastNaiveBayes")
}
