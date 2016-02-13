
.is_ts_data <- function (lineRead){
  str_detect(lineRead, "^[0-9]{4}-[0-9]{2}-[0-9]{2}")
}

.is_asset_data <- function (lineRead){
  str_detect(lineRead, "@ASSET@")
}

.is_command <- function (lineRead){
  str_detect(lineRead, "^\\*")
}

find_line_type = function (lineRead){
  
  if (.is_asset_data(lineRead)) return("ASSET")
  if (.is_ts_data(lineRead)) return("TS")
  if (.is_command(lineRead)) return("COMMAND")
  return("UNKNOWN")
}

#' Function to load the log data into R as a data.frame
#' 
#' @param file_path_adapter_log Path to the file containing log data
#' @param condition_names A character string with the names of the data items that
#'  represents the conditions in the log data
#' @export
#' @examples 
#' device_name = "test_device"
#' file_path_xml = "testdata/dataExtraction/test_devices.xml"
#' xpath_info = get_xpaths_from_xml(system.file(file_path_xml, package = "mtconnectR"), device_name)
read_adapter_log_file <- function (file_path_adapter_log, condition_names = c()) {
  linesRead <- scan(file = file_path_adapter_log, what = "character", sep = '\n', quiet = T)
  line_types <- vapply(linesRead, find_line_type, "", USE.NAMES = F)
  
  message("Reading Adapter Log data...")
  ts_data = plyr::llply(.progress = "text", linesRead[line_types == "TS"], read_adapter_log_line_ts, condition_names) %>%
    rbindlist(use.names = T, fill = F) %>%
    arrange_("timestamp") %>% 
    as.data.frame()
}

# Function to read one line of adapter log data
read_adapter_log_line_ts = function (lineRead, condition_names = c()) {

  line_split <- str_split(lineRead, pattern = "\\|" )[[1]]
  
  full_length <- length(line_split)
  empty_result = data.frame(timestamp = as.POSIXct(1, origin='1970-01-01')[0], data_item_name = character(0), value = character(0))
  
  if (full_length < 3L) return(empty_result)
  
  timestamp <- line_split[1] ; variables <- values <- character(1000)
  count <- 1L ; current_position <- 2L
  
  while(current_position<full_length) {
    if (line_split[current_position] %in% condition_names) {
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
  
  sub_df_log_data <- data.frame(timestamp = as.POSIXct(timestamp, format="%Y-%m-%dT%H:%M:%OSZ", tz = "UTC"),
                                data_item_name = variables[1:(count-1L)],
                                value = values[1:(count-1L)])
  return(sub_df_log_data)
}

#' Create MTCDevice class from adapter data and log file
#' 
#' @param file_path_adapter_log Path to adapter log file
#' @param file_path_xml Path to the XML file
#' @param device_name name of the device in the xml. List of all the devices and their
#'  names can be got using the \code{\link{get_device_info_from_xml}} function
#' @param mtconnect_version Specify MTConnect Version manually. If not specified, it is inferred automatically from the data.
#' @examples 
#' file_path_adapter_log = "testdata/dataExtraction/test_log_data.log"
#' file_path_xml = "testdata/dataExtraction/test_devices.xml"
#' device_name = "test_device"
#' mtc_device = create_mtc_device_from_adapter_data(
#'   system.file(file_path_adapter_log, package = "mtconnectR"),
#'   system.file(file_path_xml, package = "mtconnectR"),
#'   device_name)
#' print(summary(mtc_device))
#' @export
create_mtc_device_from_adapter_data <- function(file_path_adapter_log, file_path_xml, device_name, mtconnect_version = NULL) {
  
  xpaths_map <- get_xpaths_from_xml(file_path_xml, device_name = device_name, mtconnect_version = mtconnect_version)
  CONDITION_DATAITEM_NAMES = xpaths_map$name[xpaths_map$category == "CONDITION"] %>% unique()
  SAMPLE_DATAITEM_REGEXP =  paste0(":", paste0(xpaths_map$name[xpaths_map$category == "SAMPLE"] %>% unique(), collapse = "<|:"), "<")
  
  # Get log data into R data frames
  data_from_log <- read_adapter_log_file(file_path_adapter_log = file_path_adapter_log, condition_names = CONDITION_DATAITEM_NAMES)
  
  # check_xml_configuration(data_from_log, xpaths_map)

  mergedData <- merge(data_from_log, xpaths_map, by.x = "data_item_name", by.y = "name", all = F) %>%
    select_("timestamp", "xpath", "value") %>% arrange_("timestamp")
  
  data_item_list <- plyr::dlply(.data = mergedData, .variables = 'xpath', .fun = function(x){
    new('MTCDataItem', x %>% data.frame %>% select_("timestamp", "value"),
        ifelse(test = str_detect(x$xpath[1], SAMPLE_DATAITEM_REGEXP), yes = 'Sample', no = 'Event'),
        x$xpath[1], 'logData')
    }
  )
  
  attr(data_item_list, 'split_type') = attr(data_item_list, 'split_labels') = NULL
  result <- new('MTCDevice', rawdata = list(data_from_log), data_item_list = data_item_list, device_uuid = attr(xpaths_map, "details")[['uuid']])
}
