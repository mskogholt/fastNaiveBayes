#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.multinomial <- function(x, y, priors = NULL, laplace = 0, sparse = FALSE, check = TRUE) {
  UseMethod("fnb.multinomial")
}

#' @export
#' @import Matrix
#' @rdname fastNaiveBayesF
fnb.multinomial.default <- function(x, y, priors = NULL, laplace = 0, sparse = FALSE, check = TRUE) {
  if(check){
    args <- fnb.check.args.model(x, y, priors, sparse)
    x <- args$x
    y <- args$y
    priors <- args$priors
    sparse <- args$sparse

    # laplace
    if(laplace < 0){
      stop('Laplace smoothing must a positive number.')
    }
  }

  if (sparse) {
    present <- lapply(levels(y), function(level) {
      Matrix::colSums(x[y == level, , drop=FALSE])
    })
    present <- do.call(rbind, present)
  } else {
    present <- rowsum(x, y)
  }

  if(is.null(priors)){
    priors <- tabulate(y) / nrow(x)
  }

  structure(list(
    present = present,
    laplace = laplace,
    priors = priors,
    names = colnames(x),
    levels = levels(y)),

    class = "fnb.multinomial"
  )
}

#' @export
#' @import Matrix
#' @rdname predict.fastNaiveBayes
predict.fnb.multinomial <- function(object, newdata, type = c("class", "raw", "rawprob"), sparse = FALSE,
                                    threshold = .Machine$double.eps, check = TRUE, ...) {

  type <- match.arg(type)
  if(check){
    args <- fnb.check.args.predict(object, newdata, type, sparse, threshold, ...)
    object <- args$object
    newdata <- args$newdata
    type <- args$type
    sparse <- args$sparse
    threshold <- args$threshold

    if(length(object$names)!=length(colnames(newdata))){
      other_names <- setdiff(object$names, colnames(newdata))
      if(length(other_names)>0){
        if (sparse) {
          other_mat <- Matrix(0L, nrow = nrow(newdata), ncol = length(other_names), sparse = TRUE)
        } else {
          other_mat <- matrix(0L, nrow = nrow(newdata), ncol = length(other_names))
        }
        colnames(other_mat) <- other_names

        newdata <- cbind(newdata, other_mat)
      }
      newdata <- newdata[, object$names, drop=FALSE]
    }
  }

  present <- object$present + object$laplace
  total <- rowSums(present)

  present <- present / total

  present <- log(present)
  present[is.infinite(present)] <- max(-100000, log(threshold))

  presence_prob <- newdata %*% t(present)

  if (type == "rawprob") {
    return(presence_prob)
  }
  probs <- exp(presence_prob)
  priors <- object$priors
  for(i in 1:length(priors)){
    probs[,i] <- probs[,i]*priors[i]
  }

  denom <- rowSums(probs)
  denom[denom==0] <- 1
  probs <- probs / denom

  if (type == "class") {
    class <- as.factor(object$levels[max.col(probs, ties.method = "first")])
    levels(class) <- object$levels
    return(class)
  }
  return(probs)
}
