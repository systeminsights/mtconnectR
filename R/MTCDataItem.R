
## MTCDataItem Class methods
setClass("MTCDataItem",  representation(data = "data.frame", metadata = "list"))

#' Get data from the object in a data frame form
#' @param .Object A MTC Object
#' @examples 
#' data("example_mtc_data_item")
#' getData(example_mtc_data_item)
setGeneric("getData", function(.Object){standardGeneric("getData")})

#' Get MetaData from the Object as a list
#' 
#' @param .Object Object of MTCDataItem Class
#' @examples 
#' data("example_mtc_data_item")
#' getMetaData(example_mtc_data_item)
setGeneric("getMetaData", function(.Object){standardGeneric("getMetaData")})

setValidity("MTCDataItem", function(object)
{
  if(!is(object@data$timestamp, "POSIXct")) {	
    stop("timestamp objects not of class POSIXct", structure = str(object))
  }
  return(TRUE)
})

setMethod("initialize", "MTCDataItem", function(.Object, data, metadata){
  .Object@metadata = metadata
  rownames(data) <- NULL
  .Object@data = as.data.frame(data)

  if ((metadata$category == "SAMPLE") && ("value" %in% names(.Object@data))) {
    .Object@data$value[.Object@data$value == "UNAVAILABLE"] = NA_real_
    .Object@data$value = as.numeric(.Object@data$value)
  }
  
  if(metadata$category == "EVENT"  && ("value" %in% names(.Object@data)))  .Object@data$value = as.character(.Object@data$value) 
  
  return(.Object)
})


#' Get data from the object in a data frame form
#' @param .Object A MTC Object
#' @examples 
#' data("example_mtc_data_item")
#' getData(example_mtc_data_item)
#' @export
setMethod("getData", "MTCDataItem", function(.Object){
  return(.Object@data)
})

#' Get MetaData from the Object as a list
#' 
#' @param .Object Object of MTCDataItem Class
#' @examples 
#' data("example_mtc_data_item")
#' getMetaData(example_mtc_data_item)
#' @export
setMethod("getMetaData", "MTCDataItem", function(.Object){
  return(.Object@metadata)
})

