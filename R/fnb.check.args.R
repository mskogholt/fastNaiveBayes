fnb.check.args.save <- function(model, filename, overwrite){
  if(!is.null(model)){
    if(!class(model)[1] %in% c("fnb.bernoulli","fnb.gaussian","fnb.multinomial",
                            "fastNaiveBayes")){
      stop('Cannot save objects that are not a fastNaiveBayes model.')
    }
  }

  if(file.exists(filename) && !overwrite){
    stop(paste0("There's already a model saved under ", filename))
  }
}

fnb.check.args.load <- function(filename){
  if(!file.exists(filename)){
    stop(paste0("There's not any model saved under ", filename))
  }
}

#' @import Matrix
fnb.check.args.dist <- function(x, nrows){
  cast <- fnb.utils.cast(x, FALSE)
  x <- cast$x

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

#' @import Matrix
fnb.check.args.model <- function(x, y, priors, sparse){
  cast <- fnb.utils.cast(x, sparse)
  x <- cast$x
  sparse <- cast$sparse

  if(is.null(colnames(x))){
    stop("x must have column names!")
  }

  if(any(is.na(x))){
    warning("x contains na's. These will be set to 0")
    x[is.na(x)] <- 0
  }

  # y
  if(!is.factor(y)){
    y <- as.factor(y)
  }

  # if(nlevels(y)<=1){
  #   stop('y does not have enough levels to classify.')
  # }

  if(any(is.na(y))){
    warning("y contains na's. These observations will be removed")
    x <- x[!is.na(y),]
    y <- y[!is.na(y)]
  }

  # y with x
  if(nrow(x)!=length(y)){
    stop('Rows of x not equal to length of y')
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
  return(list(x=x, y=y, priors = priors, sparse=sparse))
}

#' @import Matrix
fnb.check.args.predict <- function(object, newdata, type, sparse, threshold, silent = FALSE){

  cast <- fnb.utils.cast(newdata, sparse)
  newdata <- cast$x
  sparse <- cast$sparse

  if(threshold<0){
    stop('Threshold must be a positive number')
  }

  if(is.null(colnames(newdata))){
    stop("newdata does not have any column names!")
  }

  if(!silent){
    if(length(object$names)!=length(colnames(newdata))){
      warning('Columns in test and train set not equal! Only the intersect of the two is used for prediction')
    }
  }

  names <- intersect(object$names, colnames(newdata))
  newdata <- newdata[, names, drop=FALSE]

  if(any(is.na(newdata))){
    warning("newdata contains na's. These will be set to 0")
    newdata[is.na(newdata)] <- 0
  }

  return(list(object=object, newdata=newdata, type=type, sparse=sparse, threshold = threshold))
}

