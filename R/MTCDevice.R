#' An S4 class to represent a device. It contains MTCCycle class and the slots below.
#'
#' @slot rawdata Delimited MTC data (parsed from the file using which the data was created)
#' @slot metadata Metadata (if any about) the device
#' @examples
#' data('example_mtc_data_item')
#' example_mtc_data_item
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
        list(category = ifelse(test = is.numeric(temp_df$value), yes = 'SAMPLE', no = 'EVENT'),
             path = x)
    )
  })
  names(data_item_list) = data_item_names
  new('MTCDevice', rawdata = list(), data_item_list = data_item_list, device_uuid = device_uuid)
  
}


#' Calculate feed rate from the path position data items
#'
#' Returns a data.frame which contains the calculated feed rates and the corresponding time stamps
#' @param mtc_device is the MTCDevice object
#' @param pattern is the pattern of the path postion data items in the device object
#' @examples
#' data("example_mtc_device_3")
#' calculated_feed_from_postion = calculated_feed_from_position(example_mtc_device_3)
#' @export
calculated_feed_from_position <- function(mtc_device, pattern = "PATH_POSITION"){
  value = NULL
  # TODO: Quickfix function. Needs to be made generic
  # Calculate moving average
  ma <- function(x,n=5){stats::filter(x,rep(1/n,n), sides=2)}
  
  posParams = extract_param_from_xpath(names(mtc_device@data_item_list[grep(pattern, names(mtc_device@data_item_list))]))
  message("Using Datatems ", paste0(posParams, collapse = ","), " for calculating PFR")
  positions_merged = convert_ts_to_interval(merge(mtc_device, pattern))
  positions_merged_diff = apply(positions_merged[,4:ncol(positions_merged)], 2, diff)
  positions_merged_diff_sum = c(0, round(sqrt(apply(positions_merged_diff, 1, function(x)  sum(x*x) )),4))
  calculated_feed = data.frame(timestamp = positions_merged$start, value = positions_merged_diff_sum * 60 / positions_merged$duration) %>%
    mutate(value = ma(value, 3)) %>% # Not correct, but I don't care now
    stats::na.omit() %>% clean_reduntant_rows()
}

#' Filter MTCDevice object based on time range
#'
#' Helper function to quickly filter based on time range
#' @param mtc_device is the MTCDevice objectcc
#' @param start_time is the Start time
#' @param end_time is the End time
#' @examples 
#' data("example_mtc_device_3")
#' start_time = as.POSIXct("2016-03-22 12:45:00.000")
#' end_time = as.POSIXct("2016-03-22 12:45:10.000")
#' filtered_data = filter_timestamps_mtc_device(example_mtc_device_3,start_time,end_time)
#' @export
filter_timestamps_mtc_device <- function(mtc_device, start_time, end_time){
  timestamp = NULL
  mtc_device@data_item_list = lapply(mtc_device@data_item_list, function(x){
    x@data = x@data %>%  filter(timestamp > start_time & timestamp < end_time)
    x
  })
  # mtc_device@rawdata = mtc_device@rawdata %>% as.data.frame %>%  
  #   filter(timestamp > start_time & timestamp < end_time) %>% as.list
  mtc_device
}