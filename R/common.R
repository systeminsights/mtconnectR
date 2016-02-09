
# Common functions across the package

#' @importFrom data.table rbindlist
#' @importFrom stringr str_split
#' @importFrom stringr str_replace_all
#' @importFrom stringr str_replace
#' @importFrom stringr str_detect
#' @importFrom plyr ldply
#' @importFrom dplyr arrange_
#' @importFrom dplyr '%>%' 
#' @importFrom dplyr mutate 
#' @importFrom dplyr select_
#' @importFrom dplyr group_by
#' @importFrom dplyr do
#' @importFrom dplyr select
NULL

#' Example data set showing MTC Log data
#'
#' A manually created dataset showing a log data file, parsed and read into R. The columns are 
#' \itemize{
#'   \item timestamp. Timestamp of the event
#'   \item data_item_name Name of the data Item from the adapter log. Can be empty.
#'   \item value of the data item
#' }
#'
#' @format A data frame some rows and 3 variables
"example_log_data"

#' Example data set showing Xpaths from a device XML
#'
#' Dataset showing a parsed DeviceXML file showing all the XPaths and the properties
#' \itemize{
#'   \item id ID of the data item
#'   \item name Name of the data Item from the adapter log. Can be empty.
#'   \item type MTC Type of the data item
#'   \item category MTC Category of the data item
#'   \item subType MTC subType of the data item. Can be emoty
#'   \item xpath xpath showing the truncated path to the particular data item in the device XML
#' }
#'
#' @format A data frame some rows and 6 variables
"example_xpath_info"

#' Example data set showing a MTConnect Device
#'
#' The data can be accessed using the @ function. The slots are:
#' \itemize{
#'   \item rawdata Original adapter log (parsed from which the data was created)
#'   \item metadata Metadata (if any) for the device
#'   \item data_item_list processed data showing each data item as a separate device
#'   \item device_uuid UUID of the device
#' }
#'
#' @format An MTCDevice data item
"example_log_data"