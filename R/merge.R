
#' Merges all the data.frames in the list into single data.frame
#' 
#' @param DF_list is a list of data.frames. Each data.frame should be of type timestamp|value1|value2...
#' @param output_DF if TRUE, then returns output in the form of data.frame instead of data.table
#' @param use_list_names if TRUE, the names of the list are assigned the columns names
#' @param additional_ts an POSIXct vector of timestamps which needs to be added into the table.
#'  The values are repeated from the previous timestamp
#' 
mergeTS <- function(DF_list, output_DF = T, use_list_names = F, additional_ts = integer()){
  
  if (length(dataList) == 0) {
    warn(paste("You gave me a list with zero elements!"))
    return(NULL)
  }
  
  all_timestamps = NULL
  for (i in setdiff(1:length(dataList), missTimes))
    all_timestamps = append(all_timestamps, dataList[[i]]$timestamp)
  
  merged_data = data.table::data.table(timestamp = sort(unique(a), method = "quick"),key = "timestamp")
  
  for (i in length(dataList):1){
    data = dataList[[i]]
    data = data.table::data.table(data, key = "timestamp")
    merged_data = data[merged_data, mult = "first", roll = T]
    setnames(merged_data ,c("timestamp", paste0("V", 2:ncol(merged_data)))) 
  }
  
  if (!useListNames)  valNames = unlist(sapply(dataList, names)) else
    valNames = unlist(names(dataList))
  
  valNames = valNames[valNames!="timestamp"]
  setnames(merged_data, c("timestamp", valNames))
  if (outputDF == T) merged_data = data.frame(merged_data)
  merged_data
}