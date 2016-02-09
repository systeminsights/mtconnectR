
setClass("MTCCycle", representation(data_item_list = "list", device_uuid ="character", "VIRTUAL"))
setGeneric("getDataItem", function(.Object, pattern,...){standardGeneric("getDataItem")})

#' Show a quick summary of the MTCCyle or MTCDevice Object
#' 
#' @param object The MTCDevice object
#' @export
setMethod("summary", "MTCCycle", function(object){
  output = ldply((object@data_item_list), function(x) summary(x))
  output$'.id' = NULL
  return(output)
})


#' Merge all data items from the MTCCycle or MTCDevice
#' 
#' @param .Object object of MTCCycle or MTCDevice Class
#' @examples 
#' data("example_mtc_device")
#' merge(example_mtc_device)
#' @export 
setMethod("merge", "MTCCycle", function(x){
  dataList = lapply(x@data_item_list, function(y) y@data)
  mergeddata = (mergeTS(dataList))
  names(mergeddata) = c("timestamp", names(dataList))
  mergeddata
})


#' Merge on or more data items from the MTCCycle or MTCDevice using a character pattern
#' 
#' @param .Object object of MTCCycle or MTCDevice Class
#' @examples 
#' data("example_mtc_device")
#' merge(example_mtc_device, "POSIT")
#' @export 
setMethod("merge", c("MTCCycle", "character"), function(x, y){
  data_item_list = x@data_item_list[grep(y, names(x@data_item_list))]
  if(length(data_item_list) == 0) 
  {
    flog.info(paste("No DataItems match the pattern you provided"))
    return(data.frame(timestamp = NA))
  }
  dataList = lapply(data_item_list, function(y) y@data)
  mergeddata = (mergeTS(dataList))
  names(mergeddata) = c("timestamp", names(dataList))
  mergeddata
})

#' Merge on or more data items from the MTCCycle or MTCDevice using an index
#' 
#' @param .Object object of MTCCycle or MTCDevice Class
#' @examples 
#' data("example_mtc_device")
#' merge(example_mtc_device, 1)
#' @export 
setMethod("merge", c("MTCCycle", "numeric"), function(x, y){
  data_item_list = x@data_item_list[y]
  dataList = lapply(data_item_list, function(y) y@data)
  mergeddata = (mergeTS(dataList))
  names(mergeddata) = c("timestamp", names(dataList))
  mergeddata
})

#' Get the first dataitem
#' 
#' @param .Object object of MTCCycle or MTCDevice Class
#' @examples 
#' data("example_mtc_device")
#' getDataItem(example_mtc_device)
#' @export 
setMethod("getDataItem", "MTCCycle", function(.Object){
  .Object@data_item_list[[1]]
})

#' Get one or more data items from the MTCCycle or MTCDevice using a character pattern
#' 
#' @param .Object object of MTCCycle or MTCDevice Class
#' @examples 
#' data("example_mtc_device")
#' getDataItem(example_mtc_device, "POSIT")
#' @export 
setMethod("getDataItem", c("MTCCycle", "character"), function(.Object, pattern){
  if(length(grep(pattern, names(.Object@data_item_list)))>1)
    return((.Object@data_item_list[grep(pattern, names(.Object@data_item_list))])) else
      (.Object@data_item_list[[grep(pattern, names(.Object@data_item_list))]])
})

#' Get one or more data items from the MTCCycle or MTCDevice using a numeric index
#' 
#' @param .Object object of MTCCycle or MTCDevice Class
#' @examples 
#' data("example_mtc_device")
#' getDataItem(example_mtc_device, 1:2)
#' @export
setMethod("getDataItem", c("MTCCycle", "numeric"), function(.Object, pattern){
  if(length(pattern)>1) (.Object@data_item_list[pattern]) else
    (.Object@data_item_list[[pattern]])
})

#' Get Data from the Object as a data.frame
#' 
#' @param .Object object of MTCCycle or MTCDevice Class
#' @export
#' @examples 
#' data("example_mtc_device")
#' getData(example_mtc_device)
setMethod("getData", "MTCCycle", function(.Object){
  output = (ldply((.Object@data_item_list), function(x) getData(x)))
  names(output)[1] = "data_item_name"
  return(output)
})