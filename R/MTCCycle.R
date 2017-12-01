#' An S4 class to represent the different data items of a device
#'
#' @slot data_item_list List of data items with data(data.frame of timestamp,value),
#' data_type,path,dataSource,xmlID
#' @slot device_uuid UUID of the device

setClass("MTCCycle", representation(data_item_list = "list", device_uuid ="character", "VIRTUAL"))

#' Get data on one or more data items from the class
#' @param .Object object of MTCCycle or MTCDevice Class
#' @param pattern OPTIONAL can be used to query specific data items
#' @examples
#' data("example_mtc_device")
#' getDataItem(example_mtc_device)
setGeneric("getDataItem", function(.Object, pattern){standardGeneric("getDataItem")})

#' Merge all data items from the MTCCycle or MTCDevice
#'
#' @param x Object of MTCCycle or MTCDevice Class
#' @examples
#' data("example_mtc_device")
#' merge(example_mtc_device)
#' @export
setMethod("merge", "MTCCycle", function(x){
  dataList = lapply(x@data_item_list, function(y) y@data)
  mergeddata = (mergeTS(dataList))
  if(is.null(mergeddata)) return(NULL)
  names(mergeddata) = c("timestamp", names(dataList))
  mergeddata
})


#' Merge one or more data items from the MTCCycle or MTCDevice using a character pattern
#'
#' @param x Object of MTCCycle or MTCDevice Class
#' @param y Regex for picking data items which has to be merged
#' @examples
#' data("example_mtc_device")
#' merge(example_mtc_device, "POSIT")
#' @export
setMethod("merge", c("MTCCycle", "character"), function(x, y){
  data_item_list = x@data_item_list[grep(y, names(x@data_item_list))]
  if(length(data_item_list) == 0)
  {
    message(paste("No DataItems match the pattern you provided"))
    return(data.frame(timestamp = NA))
  }
  dataList = lapply(data_item_list, function(y) y@data)
  mergeddata = (mergeTS(dataList))
  names(mergeddata) = c("timestamp", names(dataList))
  mergeddata
})

#' Merge one or more data items from the MTCCycle or MTCDevice using an index
#'
#' @param x Object of MTCCycle or MTCDevice Class
#' @param y Numeric index/indices of the data items to be merged
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
#' @param .Object Object of MTCCycle or MTCDevice Class
#' @examples
#' data("example_mtc_device")
#' getDataItem(example_mtc_device)
#' @export
setMethod("getDataItem", "MTCCycle", function(.Object){
  .Object@data_item_list[[1]]
})

#' Get one or more data items from the MTCCycle or MTCDevice using a character pattern
#'
#' @param .Object Object of MTCCycle or MTCDevice Class
#' @param pattern Regex of the pattern by which the data is queried
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
#' @param .Object Object of MTCCycle or MTCDevice Class
#' @param pattern Numeric index/indices of the data item to be queried
#' @examples
#' data("example_mtc_device")
#' getDataItem(example_mtc_device, 1:2)
#' @export
setMethod("getDataItem", c("MTCCycle", "numeric"), function(.Object, pattern){
  if(length(pattern)>1) (.Object@data_item_list[pattern]) else
    (.Object@data_item_list[[pattern]])
})

#' Get Data from MTCDevice/MTCCycle Object as a data.frame
#'
#' @param .Object Object of MTCCycle or MTCDevice Class
#' @export
#' @examples
#' data("example_mtc_device")
#' getData(example_mtc_device)
setMethod("getData", "MTCCycle", function(.Object){
  output = ldply((.Object@data_item_list), function(x) getData(x))
  names(output)[1] = "data_item_name"
  return(output)
})
