setClass("MTCDevice", representation(rawdata = "list", metadata = "list", data_type = "character"), contains = "MTCCycle")

setMethod("initialize", "MTCDevice", function(.Object, device_data, type = "default", device_name = NULL){
  if(type=="Agent")
  {
    .Object@device = device_data$metadata$device
    .Object@rawdata = device_data$rawdata
    
    dataList = dlply(device_data$rawdata, 2, function(x){
      a = data.frame(timestamp=x$timestamp,value=x$value)
    })
    paths = names(dataList)
    isSample = rep("Event", length(paths))
    isSample[paths%in%HSPMSampleClasses] = "HSPMSample"
    .Object@DataItemlist = lapply(1:length(dataList), function(i) new("DataItem", dataList[[i]], isSample[i], paths[i] , "Agent"))
    names(.Object@DataItemlist) = paths
    .Object@type = "Agent"
    
  }
  if(type=="default")
  {
    .Object@DataItemlist = device_data
    if(!is.null(device_name))
      .Object@device = device_name
  }
  return(.Object)
})
