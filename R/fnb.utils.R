#' @import Matrix
fnb.utils.cast <- function(x, sparse){
  # x
  if (class(x)[1] != "dgCMatrix") {
    if (!is.matrix(x)) {
      x <- as.matrix(x)
    }
    if (sparse) {
      x <- Matrix(x, sparse = TRUE)
    }
  } else {
    sparse <- TRUE
  }
  return(list(x=x,sparse=sparse))
}
