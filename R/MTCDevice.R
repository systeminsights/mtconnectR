#' An S4 class to represent a device.
#'
#' @slot rawdata Delimited MTC data (parsed from the file using which the data was created)
#' @slot metadata Metadata (if any about) the device
#' @slot data_item_list List of data items with data(data.frame of timestamp,value),data_type,path,dataSource,xmlID
#' @slot device_uuid UUID of the device
#' @examples
#' data('example_mtc_data_item')
#' mtc_data_item <- example_mtc_data_item
#' device_name = extract_param_from_xpath(mtc_data_item@path,"Device")
#' mtc_device_object = list(rawdata = list(),metadata = list(),data_item_list = list(mtc_data_item),device_uuid = device_name)
#' 
setClass("MTCDevice", representation(rawdata = "list", metadata = "list"), contains = "MTCCycle")

setMethod("initialize", "MTCDevice", function(.Object, data_item_list = list(), rawdata = list(), 
                                              metadata = list(), device_uuid = NULL){
 
    .Object@rawdata = rawdata
    .Object@metadata = metadata
    .Object@data_item_list = data_item_list
    .Object@device_uuid = device_uuid
  return(.Object)
})


#' Create a MTC device object from a merged time series data frame
#' 
#' @param merged_device An existing object of MTCDevice Class
#' @param device_uuid UUID to be given to the device
#' @examples 
#' data("example_mtc_device")
#' merged_device = merge(example_mtc_device)
#' create_mtc_device_from_ts(merged_device)
#' 
#' @export
create_mtc_device_from_ts <- function(merged_device, device_uuid = "unmerged_device"){
  data_item_names = setdiff(names(merged_device), "timestamp")
  data_item_list = lapply(data_item_names, function(x){
    temp_df = data.frame(timestamp = merged_device$timestamp, value = merged_device[[x]]) %>% 
      clean_reduntant_rows()
    new("MTCDataItem", temp_df,
        data_type = ifelse(test = is.numeric(temp_df$value), yes = 'Sample', no = 'Event'),
        path = x, dataSource = "Unmerged", xmlID = "") 
  })
  names(data_item_list) = data_item_names
  new('MTCDevice', rawdata = list(), data_item_list = data_item_list, device_uuid = device_uuid)
  
}