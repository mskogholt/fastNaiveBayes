#' @import Matrix
fnb.check.args.predict <- function(object, newdata, type, sparse, threshold, silent = FALSE){
  if(threshold<0){
    stop('Threshold must be a positive number')
  }
  if (class(newdata)[1] != "dgCMatrix") {
    if (!is.matrix(newdata)) {
      newdata <- as.matrix(newdata)
    }
    if (sparse) {
      newdata <- Matrix(newdata, sparse = TRUE)
    }
  } else {
    sparse <- TRUE
  }

  cl <- class(object)
  if(cl=="fnb.gaussian"){
    names <- intersect(object$names, colnames(newdata))
    if(!silent){
      if(length(object$names)!=length(names)){
        warning('Columns in test and train set not equal! Only the intersect of the two is used for prediction')
      }
    }
    newdata <- newdata[, names]
    if (length(names) == 1) {
      newdata <- as.matrix(newdata)
      colnames(newdata) <- names
    }
  }
  if(cl=="fnb.bernoulli" || cl=="fnb.multinomial"){
    names <- intersect(object$names, colnames(newdata))
    newdata <- newdata[, names]
    if (length(names) == 1) {
      newdata <- as.matrix(newdata)
      colnames(newdata) <- names
    }

    if(length(object$names)!=length(names)){
      if(!silent){
        warning('Columns in test and train set not equal! Newdata is padded with zeros')
      }

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
      newdata <- newdata[, object$names]
    }
    if (length(object$names) == 1) {
      newdata <- as.matrix(newdata)
      colnames(newdata) <- object$names
    }

  }

  return(list(object=object, newdata=newdata, type=type, sparse=sparse, threshold = threshold))
}

#' @import Matrix
fnb.check.args.model <- function(x, y, priors, laplace, sparse, distribution=NULL){
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

  if(ncol(x)<1){
    stop('x seems to be empty')
  }

  if(any(is.na(x))){
    warning("x contains na's. These will be set to 0")
    x[is.na(x)] <- 0
  }

  # y
  if(!is.factor(y)){
    y <- as.factor(y)
  }

  if(nlevels(y)<=1){
    stop('y does not have enough levels to classify.')
  }

  if(any(is.na(y))){
    warning("y contains na's. These observations will be removed")
    x <- x[!is.na(y),]
    y <- y[!is.na(y)]
  }

  # y with x
  if(nrow(x)!=length(y)){
    stop('Rows of x not equal to length of y')
  }

  if(any(rowsum(rep(1,times = length(y)), y)<2)){
    stop('Not enough rows. Should be at least 2 rows or more for each class')
  }

  # laplace
  if(laplace < 0){
    stop('Laplace smoothing must a positive number.')
  }

  # priors
  if(!is.null(priors)){
    if(!is.vector(priors, mode = "numeric")){
      stop(paste0("Priors should be a numeric vector with ",
                  nlevels(y), " prior probabilities"))
    }

    if (length(priors) != nlevels(y)){
      stop(paste0("Priors should be a vector with ",
                  nlevels(y), " prior probabilities"))
    }

    if(abs(sum(priors)-1) > .Machine$double.eps){
      stop(paste0('Sum of the priors should equal 1, not ', sum(priors)))
    }
  }

  # distribution
  if (is.null(distribution)) {
    distribution <- fnb.detect_distribution(x)
  }
  if(!is.list(distribution)){
    stop('distribution should be a list with distribution names and column names corresponding to x, see details.')
  }

  distribution <- distribution[lengths(distribution) != 0]
  if(!any(c("bernoulli","multinomial","gaussian") %in% names(distribution))){
    stop('Not a single accepted distribution was specified or all were empty')
  }
  if(any(!names(distribution) %in% c("bernoulli","multinomial","gaussian"))){
    warning('Redundant distribution specified, will be removed')
    distribution <- distribution[names(distribution) %in% c("bernoulli","multinomial","gaussian")]
  }
  return(list(x=x, y=y, priors = priors, laplace=laplace, sparse=sparse, distribution=distribution))
}

#' @import Matrix
fnb.check.args.dist <- function(x, nrows){
  # x
  if (class(x)[1] != "dgCMatrix") {
    if (!is.matrix(x)) {
      x <- as.matrix(x)
    }
  }

  if(nrows<1){
    stop('nrows must be a positive number larger than 1. To use all rows, leave the argument empty')
  }

  nrows <- min(nrow(x), nrows)
  if(ncol(x)==1){
    name <- colnames(x)
    x <- as.matrix(x[1:nrows, ])
    colnames(x) <- name
  }else{
    x <- x[1:nrows, ]
  }
  x[is.na(x)] <- 0

  return(x)
}
