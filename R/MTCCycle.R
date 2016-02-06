
setClass("MTCCycle", representation(data_item_list = "list", device_uuid ="character", "VIRTUAL"))
setGeneric("getDataItem", function(.Object, pattern,...){standardGeneric("getDataItem")})
setGeneric("getData", function(.Object, pattern,...){standardGeneric("getData")})

setMethod("fix", "MTCCycle", function(x){
  View(ldply((x@data_item_list), function(y) headtail(y@data)))
  return(invisible(ldply((x@data_item_list), function(y) headtail(y@data))) )
})

setMethod("summary", "MTCCycle", function(object){
  output = ldply((object@data_item_list), function(x) summary(x))
  output$'.id' = NULL
  return(output)
})

setMethod("merge", "MTCCycle", function(x){
  dataList = lapply(x@data_item_list, function(y) y@data)
  mergeddata = (mergeTS(dataList))
  names(mergeddata) = c("timestamp", names(dataList))
  mergeddata
})

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

setMethod("merge", c("MTCCycle", "numeric"), function(x, y){
  data_item_list = x@data_item_list[y]
  dataList = lapply(data_item_list, function(y) y@data)
  mergeddata = (mergeTS(dataList))
  names(mergeddata) = c("timestamp", names(dataList))
  mergeddata
})

setMethod("getDataItem", "MTCCycle", function(.Object){
  .Object@data_item_list[[1]]
})

setMethod("getDataItem", c("MTCCycle", "character"), function(.Object, pattern){
  if(length(grep(pattern, names(.Object@data_item_list)))>1)
    return((.Object@data_item_list[grep(pattern, names(.Object@data_item_list))])) else
      (.Object@data_item_list[[grep(pattern, names(.Object@data_item_list))]])
})

setMethod("getDataItem", c("MTCCycle", "numeric"), function(.Object, pattern){
  if(length(pattern)>1) (.Object@data_item_list[pattern]) else
    (.Object@data_item_list[[pattern]])
})

setMethod("getData", "MTCCycle", function(.Object){
  output = (ldply((.Object@data_item_list), function(x) getData(x)))
  names(output)[1] = "DataItemName"
  return(output)
})