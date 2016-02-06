
## DataItem Class methods
setClass("DataItem",  representation(data = "data.frame", is.sample = "character", path = "character", dataSource="character", xmlID="character"), prototype("is.sample" = "Event", "path" = ""))
setGeneric("getData", function(.Object, pattern,...){standardGeneric("getData")})


setValidity("DataItem", function(object)
{
  if(!is(object@data$timestamp, "POSIXct")) {	
    flog.stop("timestamp objects not of class POSIXct", structure = str(object))
  }
  if(object@is.sample == "Sample") {
    if(!is(object@data$value, "numeric")) {
      flog.stop("Sample objects in DataItem not of class numeric", structure = str(object))
    }
  } else if(!is(object@data$value, "character")) {
    flog.stop("Non - Sample objects in DataItem not of class character", structure = str(object))
  }
  return(TRUE)
})

setMethod("initialize", "DataItem", function(.Object, data, is.sample="Event", path=NULL, dataSource="JSON", xmlID = "No XML ID found"){
  
  .Object@is.sample = is.sample
  .Object@path = path
  rownames(data) <- NULL
  .Object@data = as.data.frame(data)
  .Object@dataSource = dataSource
  
  if ((is.sample == "Sample") && ("value" %in% names(.Object@data))) {
    .Object@data$value = as.numeric(.Object@data$value)
  }
  
  if(is.sample == "Event")  .Object@data$value = as.character(.Object@data$value) 
  
  .Object@xmlID <- xmlID
  
  return(.Object)
})

setMethod("fix", "DataItem", function(x){
  flog.info(paste("The path is", x@path))
  flog.info(paste("is Sample =", x@is.sample))
  return(NULL)
})

setMethod("summary", "DataItem", function(object){
  output = data.frame((object@path), nrow(object@data), object@data$timestamp[1], tail(object@data$timestamp, 1), object@is.sample)
  names(output) = c("path", "Records", "start", "end", "is.sample")
  return(output)
})


setMethod("getData", "DataItem", function( .Object){
  return(.Object@data)
})
