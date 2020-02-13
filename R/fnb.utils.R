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

#' @import Matrix
fnb.utils.pad_with_zeros <- function(x, sparse, names){
  if(length(names)!=length(colnames(x))){
    other_names <- setdiff(names, colnames(x))
    if(length(other_names)>0){
      if (sparse) {
        other_mat <- Matrix(0L, nrow = nrow(x), ncol = length(other_names), sparse = TRUE)
      } else {
        other_mat <- matrix(0L, nrow = nrow(x), ncol = length(other_names))
      }
      colnames(other_mat) <- other_names

      x <- cbind(x, other_mat)
    }
    x <- x[, names, drop=FALSE]
  }

  return(x)
}

#' @import Matrix
fnb.utils.rowsum <- function(x, y, sparse){
  if (sparse) {
    present <- lapply(levels(y), function(level) {
      Matrix::colSums(x[y == level, ,drop=FALSE])
    })
    present <- do.call(rbind, present)
    rownames(present) <- as.factor(levels(y))
  } else {
    present <- rowsum(x, y)
  }
  return(present)
}
