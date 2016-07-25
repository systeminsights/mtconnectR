
# Common functions across the package

#' @importFrom data.table rbindlist
#' @importFrom stringr str_split
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_detect
#' @importFrom stringr str_extract
#' @importFrom stringr str_trim
#' @importFrom utils tail
#' @importFrom plyr ldply
#' @importFrom plyr rbind.fill
#' @importFrom dplyr arrange_
#' @importFrom dplyr '%>%'
#' @importFrom dplyr mutate
#' @importFrom dplyr select_
#' @importFrom dplyr group_by
#' @importFrom dplyr do
#' @importFrom dplyr select
#' @importFrom dplyr arrange
#' @importFrom dplyr filter
#' @importFrom dplyr slice
#' @importFrom dplyr mutate
#' @importFrom dplyr rename
#' @importFrom dplyr rename_
#' @importFrom dplyr one_of
#' @importFrom dplyr contains
#' @importFrom dplyr everything
#' @importFrom magrittr extract2
#' @import methods
#' @import ggplot2
NULL

#' Example data set showing MTC Log data
#'
#' A manually created dataset showing a log data file, parsed and read into R. The columns are
#' \itemize{
#'   \item timestamp.  Timestamp of the event
#'   \item data_item_name. Name of the data Item from the delimited MTC data. Can be empty.
#'   \item value. of the data item
#' }
#'
#' @format A data frame with some rows and 3 variables
"example_dmtcd"

#' Example data set showing Xpaths from a device XML
#'
#' Dataset showing a parsed DeviceXML file showing all the XPaths and the properties
#' \itemize{
#'   \item id ID of the data item
#'   \item name Name of the data Item from the delimited MTC data. Can be empty.
#'   \item type MTC Type of the data item
#'   \item category MTC Category of the data item
#'   \item subType MTC subType of the data item. Can be emoty
#'   \item xpath xpath showing the truncated path to the particular data item in the device XML
#' }
#'
#' @format A data frame with some rows and 6 variables
"example_xpath_info"

#' Example data set showing a MTConnect Device
#'
#' The data can be accessed using the @ function. The slots are:
#' \itemize{
#'   \item rawdata Original delimited MTC data (parsed from the file using which the data was created)
#'   \item metadata Metadata (if any) for the device
#'   \item data_item_list Processed data showing each data item as a separate device
#'   \item device_uuid UUID of the device
#' }
#'
#' @format An MTCDevice data item
"example_mtc_device"

#' A bigger example data set showing a MTConnect Device with path position and conditions
#'
#' The data can be accessed using the @ function. The slots are:
#' \itemize{
#'   \item rawdata Original delimited MTC data (parsed from which the data was created)
#'   \item metadata Metadata (if any) for the device
#'   \item data_item_list Processed data showing each data item as a separate device
#'   \item device_uuid UUID of the device
#' }
#'
#' @format An MTCDevice data item
"example_mtc_device_2"


#' Example data set showing a MTConnect Device
#'
#' The data can be accessed using the @ function. The slots are:
#' \itemize{
#'   \item rawdata Original delimited MTC data (parsed from the file using which the data was created)
#'   \item metadata Metadata (if any) for the device
#'   \item data_item_list Processed data showing each data item as a separate device
#'   \item device_uuid UUID of the device
#' }
#'
#' @format An MTCDevice data item
"example_mtc_device_3"

#' Example data set showing a MTConnect DataItem
#'
#' The data can be accessed using the @ function. The slots are:
#' \itemize{
#'   \item data Data for a single data item in a data.frame in timestamp, value format
#'   \item data_type Type of Data - can be event or sample
#'   \item path XML Xpath
#'   \item data_source Source from which the data item was created
#'   \item xmlID ID of the data item in the devices XML
#' }
#'
#' @format An MTCDevice data item
"example_mtc_data_item"


#' Example data set showing parsed G code data
#'
#' A manually created dataset showing a raw gcode data file, parsed and read into R. The columns are
#' \itemize{
#'   \item line  Line number
#'   \item single_block A single block of G code from a line
#'   \item value Value of the data item corresponding to the command
#'   \item priority Priority of the block as per the pre-written dictionary
#'   \item prefix Prefix of the block
#'   \item type Type 
#'   \item subtype Subtype
#'   \item supported Whether the specific G code block is supported or not by the dictionary
#' }
#'
#' @format A data frame with some rows and 8 variables
"example_gcode_parsed"


#' Example data set showing simulated G code data
#'
#' Dataset created using the simulate_gcode function using parsed G code. The columns are
#' \itemize{
#'   \item timestamp Simulated timestamp
#'   \item lineid Line ID 
#'   \item program Program name
#'   \item tool_id Tool ID
#'   \item pfr Simulated path feed rate
#'   \item rot_vel Simulated rotational velocity
#'   \item x_pos Simulated X axis position
#'   \item y_pos Simulated Y axis position
#'   \item z_pos Simulated Z axis position
#'   \item x_vel Simulated X axis velocity
#'   \item y_vel Simulated Y axis velocity
#'   \item z_vel Simulated Z axis velocity
#'   \item state_upcoming_tool State upcoming tool
#' }
#'
#' @format A data frame with some rows and 13 variables
"example_simulated_gcode_data"


#' MTCDevice object showing simulated G code data
#'
#' MTCDevice object created using the simulate_gcode function using parsed G code and 
#' convert_mtc_device_from_ts to convert data.frame to MTCDevice object.
#'
#' @format A data frame with some rows and 13 variables
"example_mtc_device_sim"

#' MTCDevice object containing actual and simulated data and the mapping

#' @format An MTCDevice object
"example_mtc_sim_mapped"


#' ggplot object showing mapping between simulated and actual time series

#' @format An ggplot object
"example_mapped_plot"

#' Example dataset showing the parsed xml for a device
#'
#' The data can be accessed using the @ function. The slots are:
#' \itemize{
#'   \item parsed_xml Raw XML
#'   \item device_details Name,uuid and id of the device 
#'   \item mtconnect_version 
#' }
#'
#' @format An MTCDevice data item
"example_parsed_device_xml"

#' Convert Time Series to Intervals
#'
#' Function to convert a continuous time series data to interval data.
#' The last row which goes to infinity can be deleted, else will be given dump value.
#'
#' @param df A data frame with continuous time series data
#' @param endtime_lastrow POSIXct value for the last row. Defaults to NA
#' @param arrange_cols Whether to add the interval and duration columns at the front or not
#' @param time_colname Column name of the timestamp variable
#' @param round_duration Number of decimals to rounds the duration to. Defaults
#'  to 2. If no rounding required, give NULL.
#' @seealso \code{\link{convert_interval_to_ts}}
#' @export
#' @examples
#' ts_data = data.frame(ts = as.POSIXct(c(0.5, 1, 1.008, 1.011),  tz = 'UTC', origin = "1970-01-01"),
#'                      x = c("a", "b", "c", "d"), y = c("e", "e", "e", "f"))
#' convert_ts_to_interval(ts_data, time_colname = "ts", endtime_lastrow = ts_data$ts[1] + 10)
convert_ts_to_interval <- function(df, endtime_lastrow = as.POSIXct(NA), arrange_cols = T,
                                   time_colname = 'timestamp', round_duration = 2)
{
  start_col = which(colnames(df) == time_colname)
  if (!is.null(endtime_lastrow)) df$end = endtime_lastrow else
    df$end = df[,start_col]  # Dump values for the the End times

  df = df[order(df[,start_col]), ]

  if (nrow(df) > 1) {
    df$end[1:(nrow(df) - 1)] = df[,time_colname][2:nrow(df)]
    if (is.null(endtime_lastrow))
    {
      df = df[-nrow(df),] #Deleting the last row which goes to infinity
    }else df$end[nrow(df)] = endtime_lastrow
  }
  df$duration = as.numeric(df$end) - as.numeric(df[,time_colname]) #Duration of each process
  if (!is.null(round_duration))
    df$duration <- round(df$duration, round_duration)

  colnames(df)[start_col] = 'start'
  if (arrange_cols == T) df = df[,c(start_col, (ncol(df) - 1 ), ncol(df), setdiff(1:(ncol(df) - 2), start_col))]
  rownames(df) = NULL
  return(df)
}

#' Convert Interval to Time Series
#'
#' Basically reverse the effect of \code{\link{convert_ts_to_interval}}.
#' Column names should be same as mentioned in the example
#'
#' @param df Data.frame in start, end, duration, value1, value2,...
#' @param time_colname Name of the time column
#'
#' @seealso \code{\link{convert_ts_to_interval}}
#' @export
#' @examples
#' test_interval =
#'   data.frame(start = as.POSIXct(c(0.5, 1, 1.008, 1.011),  tz = 'CST6CDT', origin = "1970-01-01"),
#'              end   = as.POSIXct(c(1, 1.008, 1.011, 2),  tz = 'CST6CDT', origin = "1970-01-01"),
#'              duration = c(0.50, 0.01, 0.00, 0.99),
# '             x     = c("a", "b", "c", "d"),
#'              y     = c("e", "e", "e", "f"))
#' convert_interval_to_ts(test_interval)
convert_interval_to_ts <- function(df, time_colname = 'start')
{
  data_n = df
  data_n$end = data_n$duration = NULL
  data_n[nrow(data_n) + 1,] = NA
  rownames(data_n) = NULL
  data_n$start[nrow(data_n)] = tail(df$end, 1)

  data_n[nrow(data_n), is.na( data_n[nrow(data_n), ])] = NA
  colnames(data_n)[1] = "timestamp"

  if (all(is.na(data_n[nrow(data_n),])) & nrow(data_n) > 1) data_n = data_n[1:(nrow(data_n)-1),]

  return(data_n)
}


#' Removes Redundant Rows in a data frame assuming statefulness
#'
#' @param df Data.frame in timestamp, value1, value2,...
#' @param clean_colname Name of the column to clean as basis
#' @param echo Whether to return messages or not
#'
#' @export
#' @examples
#' test_interval =
#'   data.frame(timestamp = as.POSIXct(c(0.5, 1, 1.008, 1.011), origin = "1970-01-01"),
#'             x     = c("a", "b", "b", "b"),
#'              y     = c("e", "e", "e", "f"))
#' clean_reduntant_rows(test_interval, "x")
clean_reduntant_rows = function(df, clean_colname = "value", echo = F) {
  df = data.frame(df)
  clean_col = grep(clean_colname, names(df))

  if (echo) message(paste("Cleaning table with ", paste(names(df)[clean_col], collapse=","), " as basis..."))

  if (length(clean_col) == 0 ) message("No Columns match the required pattern for cleaning!")
  if (length(clean_col) > 1 )  pasted_vector = do.call(paste, df[clean_col]) else
    pasted_vector = df[[clean_col]]

  data_n = diff(as.numeric(as.factor(pasted_vector)))
  data_n[is.na(data_n)] = -100
  selected_rows = c(T, abs(data_n)!= 0)
  df = df[selected_rows,]
  rownames(df) = NULL
  return(df)
}

#' Subset a data frame using regex matching on the column name and also on the value
#'
#' @param dataFrame is a data.frame
#' @param colGrep is a regex pattern for finding the columns
#' @param subGrep is a regex pattern to subset the values in the matched column
#' @param echo If TRUE, messages are printed on the console
#' @param invert If TRUE, returns everything other than the rows and columns matched using colGrep and subGrep
#' @examples  
#' df = data.frame(type = c("sample","event","condition","sample"),value = c("value1","value2",
#'                  "value3","value4"))
#' filtered_df = grep_subset(df,"type","sample")                
#' @export

grep_subset <- function(dataFrame, colGrep, subGrep, echo = T, invert = F)
{
  col_index = grep(colGrep, names(dataFrame))
  if (length(col_index) != 1)
  {
    message(paste(col_index, " columns Matched with colGrep. Change the colGrep parameter and try again"))
    simpleError()
  }else if (echo) message(paste("'", names(dataFrame)[col_index], "' column matched with colGrep. Proceeding to subset by '", subGrep, "' regexp value" ))
  dataFrame[grep(subGrep, dataFrame[[col_index]], invert = invert),]
}


