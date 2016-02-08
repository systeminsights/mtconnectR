
## DataItem Class methods
setClass("DataItem",  representation(data = "data.frame", data_type = "character", path = "character", dataSource="character", xmlID="character"), prototype("data_type" = "Event", "path" = ""))
setGeneric("getData", function(.Object, pattern,...){standardGeneric("getData")})


setValidity("DataItem", function(object)
{
  if(!is(object@data$timestamp, "POSIXct")) {	
    flog.stop("timestamp objects not of class POSIXct", structure = str(object))
  }
  if(object@data_type == "Sample") {
    if(!is(object@data$value, "numeric")) {
      flog.stop("Sample objects in DataItem not of class numeric", structure = str(object))
    }
  } else if(!is(object@data$value, "character")) {
    flog.stop("Non - Sample objects in DataItem not of class character", structure = str(object))
  }
  return(TRUE)
})

setMethod("initialize", "DataItem", function(.Object, data, data_type="Event", path=NULL, dataSource="JSON", xmlID = "No XML ID found"){
  
  .Object@data_type = data_type
  .Object@path = path
  rownames(data) <- NULL
  .Object@data = as.data.frame(data)
  .Object@dataSource = dataSource
  
  if ((data_type == "Sample") && ("value" %in% names(.Object@data))) {
    .Object@data$value = as.numeric(.Object@data$value)
  }
  
  if(data_type == "Event")  .Object@data$value = as.character(.Object@data$value) 
  
  .Object@xmlID <- xmlID
  
  return(.Object)
})

setMethod("summary", "DataItem", function(object){
  output = data.frame((object@path), nrow(object@data), object@data$timestamp[1], tail(object@data$timestamp, 1), object@data_type)
  names(output) = c("path", "Records", "start", "end", "data_type")
  return(output)
})


setMethod("getData", "DataItem", function( .Object){
  return(.Object@data)
})
