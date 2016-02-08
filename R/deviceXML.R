
.infer_mtc_version <- function(parsed_xml){
  xml_data_list = XML::xmlToList(parsed_xml)
  return(xml_data_list$Header[['version']])
  
}

parse_devicexml_for_a_device <- function(file_path_xml, device_name, mtconnect_version = NULL) {
  parsed_xml <- XML::xmlParse(file = file_path_xml)
  xpath_query_string <- paste0("//ns:Device[@name='", device_name, "']")
  if(is.null(mtconnect_version)) mtconnect_version = .infer_mtc_version(parsed_xml)
  parsed_xml = XML::getNodeSet(doc = XML::xmlRoot(parsed_xml), path = xpath_query_string,
                               namespaces = c(ns = paste0("urn:mtconnect.org:MTConnectDevices:", mtconnect_version)))[[1]]
  
  list(parsed_xml = parsed_xml,
       device_details = XML::xmlToList(parsed_xml)[['.attrs']],
       mtconnect_version = mtconnect_version)
}

data_items_in_devicexml <- function(xml_details, mtconnect_version) {
  XML::xpathApply(
    xml_details$parsed_xml, ".//ns:DataItem", 
    namespaces = c(ns = paste0("urn:mtconnect.org:MTConnectDevices:", xml_details$mtconnect_version)),
    fun = function(x) {
      temp <- XML::xmlAttrs(x)
      list(
        id=temp["id"],
        name=temp["name"],
        type=temp["type"],
        category=temp["category"],
        subType=temp["subType"]
      )
    }) %>% data.table::rbindlist(use.names = TRUE, fill = TRUE) %>% as.data.frame
}

#' Get XML xpath info
#' 
#' Get the info on all the xpaths for a single device from the xml file. Data is 
#'  organized into a data.frame
#' @param file_path_xml Path to the XML file
#' @param device_name name of the device in the xml. List of all the devices and their
#'  names can be got using the \code{\link{get_device_info_from_xml}} function
#' @param mtconnect_version Specify MTConnect Version manually. If not specified, it is inferred automatically from the data.
#' @export
#' 
#' @examples 
#' 
get_xpaths_from_xml <- function(file_path_xml, device_name, mtconnect_version = NULL) {
  xml_details = parse_devicexml_for_a_device(file_path_xml, device_name, mtconnect_version)
  
  ans = data_items_in_devicexml(xml_details) %>%
    mutate(xpath = paste0(device_name, '<Device>:',
                          name, '<', ifelse(is.na(subType), type, paste0(type,'-',subType)), '>'))
  attr(ans, "details") = xml_details$device_details
  ans
}

#' Get info on all the devices in the xml file
#' 
#' Device XML usually consists of the configuration details of multiple file. This
#' function can detail all the device info in the XML into a data.frame for easy reference
#' 
#' @inheritParams get_xpaths_from_xml
#' @export
#' @seealso \code{\link{get_xpaths_from_xml}}
#' 
#' @examples
#' file_path_xml = "tests/dataExtraction/test_devices.xml"
#' devices_info = get_device_info_from_xml(system.file(file_path_xml, package = "mtconnectR"))
#' print(devices_info)
#' 
get_device_info_from_xml <- function(file_path_xml, mtconnect_version = NULL){
  xml_data_list = XML::xmlToList(XML::xmlParse(file = file_path_xml))
  ldply(xml_data_list$Devices, function(x) x[['.attrs']]) %>% mutate(.id = NULL)
}