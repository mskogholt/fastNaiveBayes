fnb.check.args.save <- function(model, filename, overwrite){
  if(!is.null(model)){
    if(!class(model) %in% c("fnb.bernoulli","fnb.gaussian","fnb.multinomial",
                            "fastNaiveBayes")){
      stop('Cannot save objects that are not a fastNaiveBayes model.')
    }
  }

  if(file.exists(paste0("./cache/",filename)) && !overwrite){
    stop(paste0("There's already a model saved under ", filename))
  }
}

fnb.check.args.load <- function(filename){
  if(!file.exists(paste0("./cache/",filename))){
    stop(paste0("There's not any model saved under ", filename))
  }
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
