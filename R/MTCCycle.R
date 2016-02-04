
setClass("MTCCycle", representation(data_item_list = "list", device ="character", "VIRTUAL"))
setGeneric("getDataItem", function(.Object, pattern,...){standardGeneric("getDataItem")})
setGeneric("getData", function(.Object, pattern,...){standardGeneric("getData")})

setMethod("fix", "MTCCycle", function(x){
  View(ldply((x@DataItemlist), function(y) headtail(y@data)))
  return(invisible(ldply((x@DataItemlist), function(y) headtail(y@data))) )
})

setMethod("summary", "MTCCycle", function(object){
  output = ldply((object@DataItemlist), function(x) summary(x))
  output$'.id' = NULL
  return(output)
})

setMethod("merge", "MTCCycle", function(x){
  dataList = lapply(x@DataItemlist, function(y) y@data)
  mergeddata = (mergeTS(dataList))
  names(mergeddata) = c("timestamp", names(dataList))
  mergeddata
})

setMethod("merge", c("MTCCycle", "character"), function(x, y){
  dataItemList = x@DataItemlist[grep(y, names(x@DataItemlist))]
  if(length(dataItemList) == 0) 
  {
    flog.info(paste("No DataItems match the pattern you provided"))
    return(data.frame(timestamp = NA))
  }
  dataList = lapply(dataItemList, function(y) y@data)
  mergeddata = (mergeTS(dataList))
  names(mergeddata) = c("timestamp", names(dataList))
  mergeddata
})

setMethod("merge", c("MTCCycle", "numeric"), function(x, y){
  dataItemList = x@DataItemlist[y]
  dataList = lapply(dataItemList, function(y) y@data)
  mergeddata = (mergeTS(dataList))
  names(mergeddata) = c("timestamp", names(dataList))
  mergeddata
})

setMethod("getDataItem", "MTCCycle", function(.Object){
  .Object@DataItemlist[[1]]
})

setMethod("getDataItem", c("MTCCycle", "character"), function(.Object, pattern){
  if(length(grep(pattern, names(.Object@DataItemlist)))>1)
    return((.Object@DataItemlist[grep(pattern, names(.Object@DataItemlist))])) else
      (.Object@DataItemlist[[grep(pattern, names(.Object@DataItemlist))]])
})

setMethod("getDataItem", c("MTCCycle", "numeric"), function(.Object, pattern){
  if(length(pattern)>1) (.Object@DataItemlist[pattern]) else
    (.Object@DataItemlist[[pattern]])
})

setMethod("getData", "MTCCycle", function(.Object){
  output = (ldply((.Object@DataItemlist), function(x) getData(x)))
  names(output)[1] = "DataItemName"
  return(output)
})