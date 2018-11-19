# By: James Tan

# Date: 3/7/2017

# Functions to save and load data and models

#' @title Save Model
#'
#' @description
#' \code{saveModel} saves an Robject to a modelpath specified in user's Rprofile with filename name
#' 
#' @param ... R object to be saved
#' @param name filename of the object
#' @param overwrite if file exists, overwrites the model if true otherwise stops
#' @param save.fun save function, defaults to save
#' 
#' @export
saveModel <- function(..., name = stop("'name' must be specified"), overwrite = FALSE, save.fun = save) {
    file <- paste(modelpath, "/", name, ".RData", sep = "")
    print(file)
    if (file.exists(file) & !overwrite) 
        stop("'file' already exists")
    save.fun(..., file = file)
}

#' @title Save Data
#'
#' @description
#' \code{saveData} saves an Robject to a datapath specified in user's Rprofile with filename name
#' 
#' @param ... R object to be saved
#' @param name filename of the object
#' @param overwrite if file exists, overwrites the data if true otherwise stops
#' @param save.fun save function, defaults to save
#' 
#' @export
saveData <- function(..., name = stop("'name' must be specified"), overwrite = FALSE, save.fun = save) {
    file <- paste(datapath, "/", name, ".RData", sep = "")
    print(file)
    if (file.exists(file) & !overwrite) 
        stop("'file' already exists")
    save.fun(..., file = file)
}

#' @title Load Model
#'
#' @description
#' \code{loadModel} loads an Robject in modelpath specified in user's Rprofile with filename name
#' 
#' @param name filename of the object
#' 
#' @return the R object
#' 
#' @export
loadModel <- function(name = stop("'name' must be specified")) {
    retModel <- get(load(paste0(modelpath, "/", name, ".RData")))
    return(retModel)
}

#' @title Load Data
#'
#' @description
#' \code{loadData} loads an Robject in datapath specified in user's Rprofile with filename name
#' 
#' @param name filename of the object
#' 
#' @return the R object
#' 
#' @export
loadData <- function(name = stop("'name' must be specified")) {
    retData <- get(load(paste0(datapath, "/", name, ".RData")))
    return(retData)
}
