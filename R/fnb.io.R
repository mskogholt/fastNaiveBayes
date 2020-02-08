#' @title Save & Load Function for Fast Naive Bayes Models
#' @description Loads and saves fitted Naive Bayes models.
#' @param model the fitted Naive Bayes model to save.
#' @param overwrite Whether to allow overwriting of previously saved models.
#' @param filename the file name to use to save or load the model.
#'
#' @return fnb.save returns the filename that was used, and fnb.load returns the saved object.
#' @import Matrix
#' @export
#' @examples
#' rm(list = ls())
#' library(fastNaiveBayes)
#' cars <- mtcars
#' y <- as.factor(ifelse(cars$mpg > 25, "High", "Low"))
#' x <- cars[,2:ncol(cars)]
#'
#' mod <- fastNaiveBayes(x, y, laplace = 1)
#' fnb.save(mod, "fastNaiveBayesModel")
#' mod2 <- fnb.load("fastNaiveBayesModel")
#' identical(mod, mod2)
#'
#' @rdname fnb.io
fnb.save <- function(model, filename, overwrite=FALSE){
  UseMethod("fnb.save")
}

#' @import Matrix
#' @export
#' @rdname fnb.io
fnb.save.default <- function(model, filename, overwrite=FALSE){
  fnb.check.args.save(model, filename, overwrite)
  folder <- "./cache/"
  if(!dir.exists(folder)){
    dir.create(folder)
  }
  saveRDS(model, paste0(folder,filename))
  return(filename)
}

#' @import Matrix
#' @export
#' @rdname fnb.io
fnb.load <- function(filename){
  UseMethod("fnb.load")
}

#' @import Matrix
#' @export
#' @rdname fnb.io
fnb.load.default <- function(filename){
  fnb.check.args.load(filename)
  folder <- "./cache/"
  return(readRDS(paste0(folder,filename)))
}

