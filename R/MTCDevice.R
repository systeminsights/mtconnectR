setClass("MTCDevice", representation(rawdata = "list", metadata = "list"), contains = "MTCCycle")

setMethod("initialize", "MTCDevice", function(.Object, data_item_list = list(), rawdata = list(), 
                                              metadata = list(), device_uuid = NULL){
 
    .Object@rawdata = rawdata
    .Object@metadata = metadata
    .Object@data_item_list = data_item_list
    .Object@device_uuid = device_uuid
  return(.Object)
})
