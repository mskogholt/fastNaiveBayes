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

  n <- tabulate(y, nbins = nlevels(y))

  present <- fnb.utils.rowsum(x, y, sparse)

  structure(
    list(
      present = present,
      laplace = laplace,
      n = n,
      obs = nrow(x),
      priors = priors,
      names = colnames(x),
      levels = levels(y)
    ),
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

    newdata <- fnb.utils.pad_with_zeros(newdata, sparse, object$names)
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
  if(is.null(priors)){
    priors <- object$n / object$obs
  }

  for(i in 1:length(priors)){
    probs[,i] <- probs[,i]*priors[i]
  }

  denom <- rowSums(probs)
  denom[denom==0] <- 1
  probs <- probs / denom

  if (type == "class") {
    class <- factor(colnames(probs)[max.col(probs, ties.method = "first")],
                    levels = object$levels)
    return(class)
  }
  return(probs)
}
