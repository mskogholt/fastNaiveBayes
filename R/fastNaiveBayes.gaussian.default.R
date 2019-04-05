#' @export
#' @import Matrix
#' @rdname fastNaiveBayes.gaussian
fastNaiveBayes.gaussian.default <- function(x, y, sparse = FALSE, ...) {
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

  if (nrow(x) != length(y)) {
    stop("X and Y must be equal length")
  }

  probability_table <- lapply(levels(y), function(level) {
    x_level <- x[y == level, ]
    if (ncol(x) == 1) {
      x_level <- as.matrix(x_level)
    }

    if (sparse) {
      means <- Matrix::colMeans(x_level, na.rm = TRUE)
      mat_means <- matrix(means, nrow = nrow(x_level), ncol = ncol(x_level), byrow = TRUE)
      stddev <- sqrt(Matrix::colSums((x_level - mat_means)^2) / (nrow(x_level) - 1))
    } else {
      means <- base::colMeans(x_level, na.rm = TRUE)
      mat_means <- matrix(means, nrow = nrow(x_level), ncol = ncol(x_level), byrow = TRUE)
      stddev <- sqrt(colSums((x_level - mat_means)^2) / (nrow(x_level) - 1))
    }
    return(list(level = level, means = means, stddev = stddev))
  })

  priors <- table(y) / nrow(x)
  structure(list(
    probability_table = probability_table,
    priors = priors,
    names = colnames(x)
  ), class = "fastNaiveBayes.gaussian")
}
