
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


#' Extract different parts of a xpath
#' 
#' Returns a single parameter extracted from the xpath vector. It could be Data Item Name
#' Data Item type of the Device. If the character vector is not in xpath format, the original
#' name is returned and a warning is given
#' 
#' @param strName is the xpath string
#' @param param is the parameter to be extracted. Can be "DIName", "DIType" or "Device"
#' @param removeExtended if True, then the x: prefix is removed from extended JSON class Types
#' @param show_warnings if false, silences the warnings
#' @export
#' 
#' @examples
#' 
#' xpaths = c("timestamp", 
#'  "nist_testbed_Mazak_QT_1<Device>:avail<AVAILABILITY>",
#'  "nist_testbed_Mazak_QT_1<Device>:execution<EXECUTION>",
#'  "nist_testbed_Mazak_QT_1<Device>:Fovr<x:PATH_FEEDRATE-OVERRIDE>")
#'  
#' extract_param_from_xpath(xpaths, "DIName")
#' extract_param_from_xpath(xpaths, "DIType")
#' extract_param_from_xpath(xpaths, "DIType", TRUE)
#' extract_param_from_xpath(xpaths, "Device")
#' 
extract_param_from_xpath <- function(strName, param = "DIName", removeExtended = F, show_warnings = T)
{
  if (param == "DIType" | param == "DIName") extract1 = sapply(strsplit(strName, ">:"), tail, 1)
  if (param == "Device") extract1 = sapply(strsplit(strName, ":"), function(x) x[1])
  if (param == "DIType") extract2 = str_extract(extract1, "<.*>") else
    extract2 = str_extract(extract1, ".*<")
  if (removeExtended) extract3 = str_extract(extract2, "[[:upper:]_-]+") else
    extract3 = str_extract(extract2, "[:\\.[:alnum:]_-]+") 
  if (length(extract3[is.na(extract3)])){
    if(show_warnings) warning("Parameters couldn't be extracted from some Paths and have been ignored")
    extract3[is.na(extract3)] = strName[is.na(extract3)]
  }
  extract3
  
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
  linesRead <- scan(file = file_path_adapter_log, what = "character", sep = '\n', quiet = T, skipNul = T)
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
  empty_result = data.frame(timestamp = as.POSIXct(1, origin='1970-01-01', tz = 'UTC')[0], data_item_name = character(0), value = character(0))
  
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


#' Add a new data item to an existing MTC Device Class
#' 
#' @param mtc_device An existing object of MTCDevice Class
#' @param data_item_data data for the new data item to add
#' @param data_item_name Name of the new data Item
#' @param data_item_type Type of the new data item. Can be Event or Sample
#' @param source_type source from where data is derived. Free form text
#' @param xmlID id of the data item (optional)
#' @examples 
#' data_item_data = data.frame(timestamp = as.POSIXct(c(0.5, 1, 1.008, 1.011) +
#'                                         1445579573,  tz = 'CST6CDT', origin = "1970-01-01"),
#'                             value = c("a", "b", "c", "d"))
#' data("example_mtc_device")
#' mtc_device_updated = 
#'    add_data_item_to_mtc_device(example_mtc_device, data_item_data, data_item_name = "test",
#'                                data_item_type = "Event", source_type = "derived")
#' print(mtc_device_updated)
#' @export
#' 
add_data_item_to_mtc_device <- function(mtc_device, data_item_data, data_item_name,
                                        data_item_type = "Event", source_type = "derived", xmlID = ""){
  
  if(any(names(data_item_data) != c("timestamp", "value"))) stop("Data Item data has to have timestamp, value structre")
  if(!(data_item_type %in% c("Event", "Sample"))) stop("Data item type has to be Event or Sample")

  attr(data_item_data$timestamp, "tzone") <-  attr(mtc_device@data_item_list[[1]]@data$timestamp[1], "tzone")
  
  new_data_item = new("MTCDataItem", data_item_data, data_type = data_item_type, path = data_item_name,
                      dataSource = source_type, xmlID = xmlID) 
  mtc_device@data_item_list = append(mtc_device@data_item_list, new_data_item)
  names(mtc_device@data_item_list)[length(names(mtc_device@data_item_list))] = new_data_item@path
  
  mtc_device
}

