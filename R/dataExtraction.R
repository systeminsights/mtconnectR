

#' title createDevicefromAgent
create_mtcdevice_from_agent_data <- function(file_path_agent_log, device_uuid) 
{
  agent_data = readAgentData(file_path)
  
  data = metadata = list()
  data$rawdata = agent_data
  metadata$device = device_name
  data$metadata = metadata
  
  qdevice = new("Device", data, type="Agent")
  if(sum(str_detect(string=names(qdevice@DataItemlist),pattern="<AMPERAGE>"))) {
    if(cleanAmperageLevel==1) {
      qdevice <- RemoveAmperageDataLossPoints(qdevice)
    } else if(cleanAmperageLevel==3) {
      qdevice <- StandardizeAmperageRows(qdevice)
    } 
  }
  return(qdevice)
}

create_mtcdevice_from_adapter_data <- function(file_path_adapter_log, file_path_xml, device_uuid) {
  
  xpaths_map <- GetXPathsFromProbeXML(file_path_xml, device_uuid = device_uuid, mtconnectVersion = '1.3')
  
  CONDITION_DATAITEM_NAMES = paste0(":", paste0(subset(xpaths_map, category == "CONDITION")$name, collapse = "<|:"), "<") 
  SAMPLE_DATAITEM_NAMES =  paste0(":", paste0(subset(xpaths_map, category == "SAMPLE")$name, collapse = "<|:"), "<")
  
  # Get log data into R data frames
  dataFromLog <- ReadAdapterLogFile(file_path = file_path, conditionNames = CONDITION_DATAITEM_NAMES)
  
  # Merging log data and data from json file 
  # Discarding path position
  mergedData <- subset(merge(dataFromLog, xpaths_map, by.x = "dataItemName", by.y = "name", all = FALSE), type != "PATH_POSITION") %>%
    select(timestamp, xpath, value) %>% arrange(timestamp)
  
  dataItemList <- dlply(.data = mergedData, .variables = 'xpath', .fun = function(x){
    
    new('DataItem', x %>% data.frame %>% select(timestamp, value),
        ifelse(test = str_detect(x$xpath[1], SAMPLE_DATAITEM_NAMES), yes = 'Sample', no = 'Event'),
        x$xpath[1], 'logData')})
  attr(dataItemList, 'split_type') = NULL
  attr(dataItemList, 'split_labels') = NULL
  
  result <- new('Device', dataItemList, 'default', device_name = device_uuid)
}

# Function to load the log data into R as a data.frame
ReadAdapterLogFile <- function (file_path, conditionNames = CONDITION_DATAITEM_NAMES) {
  linesRead <- scan(file = file_path, what = "character", sep = '\n', quiet = T)
  lapply(linesRead[-1L], ReadAdapterLogLine, conditionNames) %>%
    rbindlist(use.names = T, fill = F) %>%
    arrange(timestamp) %>% 
    as.data.frame()
}

# Function to read one line of adapter log data
ReadAdapterLogLine = function (lineRead, conditionNames = CONDITION_DATAITEM_NAMES) {
  
  line_split <- str_split(lineRead, pattern = "\\|" )[[1]]
  
  full_length <- length(line_split)
  empty_result = data.frame(timestamp = as.POSIXct(1, origin='1970-01-01')[0], dataItemName = character(0), value = character(0))
  
  if (full_length < 3L) return(empty_result)
  
  # TODO Handle the asset data case correctly. Returns NULL for now.
  if (any(line_split == "@ASSET@")) return(empty_result)
  
  timestamp <- line_split[1]
  variables <- character(1000)
  values <- character(1000)
  count <- 1L
  current_position <- 2L
  
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



create_device <- function(data_source = 'adapter', ...) {
  switch(data_source,
         'adapter'  = create_mtcdevice_from_adapter_data(...),
         'agent' = create_mtcdevice_from_agent_data(...)
  )
}
