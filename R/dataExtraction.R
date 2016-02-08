
.is_ts_data <- function (lineRead){
  str_detect(lineRead, "^[0-9]{4}-[0-9]{2}-[0-9]{2}")
}

.is_asset_data <- function (lineRead){
  str_detect(lineRead, "@ASSET@")
}

.is_command <- function (lineRead){
  str_detect(lineRead, "^\\*")
}

#' Returns the tine Type of s single line
#' 
#' @param lineRead a single line from the adapter data
#' @return The type of data either, ASSET or TS or COMMAND or UNKNOWN 
#' @export
find_line_type = function (lineRead){
  
  if (.is_asset_data(lineRead)) return("ASSET")
  if (.is_ts_data(lineRead)) return("TS")
  if (.is_command(lineRead)) return("COMMAND")
  return("UNKNOWN")
}

# Function to load the log data into R as a data.frame
read_adapter_log_file <- function (file_path, conditionNames = CONDITION_DATAITEM_NAMES) {
  linesRead <- scan(file = file_path, what = "character", sep = '\n', quiet = T)
  line_types <- vapply(linesRead, find_line_type, "", USE.NAMES = F)
  
  ts_data = lapply(linesRead[line_types == "TS"], read_adapter_log_line_ts, conditionNames) %>%
    rbindlist(use.names = T, fill = F) %>%
    arrange(timestamp) %>% 
    as.data.frame()
}

# Function to read one line of adapter log data
read_adapter_log_line_ts = function (lineRead, conditionNames = CONDITION_DATAITEM_NAMES) {

  line_split <- str_split(lineRead, pattern = "\\|" )[[1]]
  
  full_length <- length(line_split)
  empty_result = data.frame(timestamp = as.POSIXct(1, origin='1970-01-01')[0], dataItemName = character(0), value = character(0))
  
  if (full_length < 3L) return(empty_result)
  
  timestamp <- line_split[1] ; variables <- values <- character(1000)
  count <- 1L ; current_position <- 2L
  
  while(current_position<full_length) {
    if (str_detect(line_split[current_position], conditionNames)) {
      # TODO Handle conditions. Not returning any value as of now
      current_position <- current_position+6L
    } else {
      variables[count] <- line_split[current_position]
      values[count] <- line_split[current_position+1L]
      current_position <- current_position+2L
      count <- count + 1L
    }}
  
  # TODO Handle conditions. Returning NULL as of now
  if (count == 1) return(empty_result)
  
  sub_df_log_data <- data.frame(timestamp = as.POSIXct(timestamp, format="%Y-%m-%dT%H:%M:%OSZ"),
                                dataItemName = variables[1:(count-1L)],
                                value = values[1:(count-1L)])
  return(sub_df_log_data)
}

#' Check if a device XML has been configured correctly
#' 
#' If a device XML is configured correctly, there should be a one-to-one mapping
#' between the names of the data items from the log data and the names of 
#' data items configured in the devices XML. Only the ones that are mapped correctly
#' can be analysed further.
#' 
#' This function checks if the mapping is correct, and issues a warning if there are 
#' incorrect mappings
#' 
#' @param data_from_log Device Adapter log in data frame form created using the 
#'   \code{\link{read_adapter_log_file}} function
#' @param xpaths_map Device XML for this devicein data frame form created using the
#'   \code{\link{get_xpaths_from_xml}} function
#' 
#' @examples 
#' 
#'   
#' @export
NULL

#' Create MTCDevice class from adapter data and log file
#' 
#' @param file_path_adapter_log Path to adapter log file
#' @param file_path_xml Path to the Device XML file
#' @param device_xml_name  Name of the Device in the Device XML file
#' @examples 
#' file_path_adapter_log = "extdata/tft-405-pfh.log"
#' file_path_xml = "extdata/Devices.xml.txt"
#' device_xml_name = "TFT-405-PFH"
#' mtc_device = create_mtc_device_from_adapter_data(
#'   system.file(file_path_adapter_log, package = "mtconnectR"),
#'   system.file(file_path_xml, package = "mtconnectR"),
#'   device_xml_name)
#' print(summary(mtc_device))
#' @export
create_mtc_device_from_adapter_data <- function(file_path_adapter_log, file_path_xml, device_xml_name) {
  
  xpaths_map <- get_xpaths_from_xml(file_path_xml, device_xml_name = device_xml_name, mtconnectVersion = '1.3')
  browser()
  CONDITION_DATAITEM_NAMES = paste0(":", paste0(subset(xpaths_map, category == "CONDITION")$name, collapse = "<|:"), "<") 
  SAMPLE_DATAITEM_NAMES =  paste0(":", paste0(subset(xpaths_map, category == "SAMPLE")$name, collapse = "<|:"), "<")
  
  # Get log data into R data frames
  data_from_log <- read_adapter_log_file(file_path = file_path_adapter_log, conditionNames = CONDITION_DATAITEM_NAMES)
  
  # check_xml_configuration(data_from_log, xpaths_map)

  mergedData <- subset(merge(data_from_log, xpaths_map, by.x = "dataItemName", by.y = "name", all = T)) %>%
    select(timestamp, xpath, value) %>% arrange(timestamp)
  
  data_item_list <- plyr::dlply(.data = mergedData, .variables = 'xpath', .fun = function(x){
    new('DataItem', x %>% data.frame %>% select(timestamp, value),
        ifelse(test = str_detect(x$xpath[1], SAMPLE_DATAITEM_NAMES), yes = 'Sample', no = 'Event'),
        x$xpath[1], 'logData')
    }
  )
  
  attr(data_item_list, 'split_type') = attr(data_item_list, 'split_labels') = NULL
  
  result <- new('MTCDevice', rawdata = list(data_from_log), data_item_list = data_item_list, device_uuid = device_xml_name)
}

#' Create Device from different data sourcers
#' 
#' This is a wrapper over the individual functions
#' @param data_source Defines what the data source is
#' @param ...  Other arguments passed on to specific data creation functions
#' @export 
create_mtc_device <- function(data_source = 'adapter', ...) {
  switch(data_source,
         # 'agent' = create_mtc_device_from_agent_data(...), 
         'adapter'  = create_mtc_device_from_adapter_data(...)
  )
}
