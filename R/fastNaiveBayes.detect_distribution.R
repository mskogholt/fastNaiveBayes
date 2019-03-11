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
#' require(mlbench)
#' require(Matrix)
#' 
#' # Load BreastCancer data
#' data(BreastCancer)
#' dim(BreastCancer)
#' levels(BreastCancer$Class)
#' head(BreastCancer)
#' 
#' # Select couple of columns
#' data_mat <- BreastCancer[, c("Class", "Cl.thickness", "Cell.size", "Cell.shape", "Marg.adhesion")]
#' 
#' y <- data_mat[, "Class"]
#' data_mat <- data_mat[, setdiff(colnames(data_mat), c("Class"))]
#' for (i in 1:ncol(data_mat)) {
#'   data_mat[[i]] <- as.numeric(data_mat[[i]])
#' }
#' 
#' # Example using only Gaussian distribution
#' model <- fastNaiveBayes.mixed(data_mat[1:400, ], y[1:400],
#'   laplace = 1, sparse = TRUE,
#'   distribution = fastNaiveBayes.detect_distribution(data_mat)
#' )
#' preds <- predict(model, newdata = data_mat[401:nrow(data_mat), ], type = "class")
#' 
#' mean(preds != y[401:length(y)])
#' @rdname fastNaiveBayes.detect_distribution
fastNaiveBayes.detect_distribution <- function(x, nrows = 10) {
  UseMethod("fastNaiveBayes.detect_distribution")
}
