#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.gaussian <- function(x, y, sparse = FALSE, ...) {
  UseMethod("fnb.gaussian")
}

#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.gaussian.default <- function(x, y, sparse = FALSE, ...) {
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

  n <- tabulate(y)
  if (sparse) {
    probability_table <- lapply(levels(y), function(level) {
      x_level <- x[y == level, ]
      if (ncol(x) == 1) {
        x_level <- as.matrix(x_level)
      }

      means <- Matrix::colMeans(x_level, na.rm = TRUE)
      mat_means <- matrix(means, nrow = nrow(x_level), ncol = ncol(x_level), byrow = TRUE)
      stddev <- sqrt(Matrix::colSums((x_level - mat_means)^2) / (nrow(x_level) - 1))
      return(list(level = level, means = means, stddev = stddev))
    })
  }else{
    rs <- rowsum(x,y)
    means <- rs/n
    stddev <- sqrt((rowsum(x^2,y)-2*means*rs+n*means^2)/(n-1))
    probability_table <- lapply(levels(y), function(level){
      return(list(level = level, means = means[level,],
                  stddev = stddev[level,]))
    })
  }

  priors <- n/nrow(x)
  structure(list(
    probability_table = probability_table,
    priors = priors,
    names = colnames(x),
    levels = levels(y)),

    class = "fnb.gaussian")
}

