parse_devicexml_for_a_device <- function(file_path, device_name, mtconnectVersion = '1.2') {
  parse_xml <- XML::xmlParse(file = file_path)
  xpath_query_string <- paste0("//ns:Device[@uuid='", device_name, "']")
  XML::getNodeSet(doc = XML::xmlRoot(parse_xml), path = xpath_query_string, namespaces = c(ns = paste0("urn:mtconnect.org:MTConnectDevices:", mtconnectVersion)))[[1]]
}

data_items_in_devicexml <- function(parsed_devicexml, mtconnectVersion = '1.2') {
  xpathApply(
    parsed_devicexml, ".//ns:DataItem", 
    namespaces = c(ns = paste0("urn:mtconnect.org:MTConnectDevices:", mtconnectVersion)),
    fun = function(x) {
      temp <- xmlAttrs(x)
      list(
        id=temp["id"],
        name=temp["name"],
        type=temp["type"],
        category=temp["category"],
        subType=temp["subType"]
      )
    }) %>% rbindlist(use.names = TRUE, fill = TRUE) %>% as.data.frame
}

get_xpaths_from_xml <- function(xml_file_path, device_xml_name, mtconnectVersion = '1.2') {
  
  parsed_xml = parse_devicexml_for_a_device(xml_file_path, device_xml_name, mtconnectVersion)

  ans = data_items_in_devicexml(parsed_xml, mtconnectVersion) %>%
    mutate(xpath = paste0(device_xml_name, '<Device>:',
                          name, '<', ifelse(is.na(subType), type, paste0(type,'-',subType)), '>'))
}


#' @title Given ProbeJSON data as a string, returns the XPaths
GetXPathsFromProbeJSON <- function(probeText) {
  
  probeJSON <- jsonlite::fromJSON(probeText,simplifyVector=FALSE)
  
  output <- list()
  
  construct_xpath <- function(subelement, current_xpath = "", current_element_type = 'Device') {
    if(is.null(subelement$name)) {
      temp <- lapply(X = subelement, construct_xpath, current_xpath = current_xpath, current_element_type = current_element_type)
    }
    
    current_element_type <- gsub(x = current_element_type,pattern = '(^.)(.*)', replacement = '\\U\\1\\E\\2', perl = TRUE)
    
    new_xpath <- paste0(current_xpath, ':', subelement$name, '<', current_element_type, '>')
    
    if('dataItems' %in% names(subelement)) {
      if(all(lapply(subelement$dataItems[[1]],is.atomic) %>% unlist() == TRUE)) {
        dataItemObjs <- subelement$dataItems[[1]] %>% as.data.frame()
      } else {
        dataItemObjs <- rbindlist(subelement$dataItems[[1]], use.names = TRUE, fill = TRUE)
      }
      
      dataItemNames <- dataItemObjs$name
      dataItemTypes <- dataItemObjs$type 
      dataItemIDs <- dataItemObjs$id
      
      dataItemNames <- ifelse(is.na(dataItemNames), dataItemIDs, dataItemNames)
      
      if(is.null(dataItemObjs$subType)) {
        dataItemSubTypes <- rep(NA_character_,times = nrow(dataItemObjs))
      } else {
        dataItemSubTypes <- dataItemObjs$subType
      }
      dataItemXpaths <- paste0(":", dataItemNames, '<', 
                               ifelse(test = is.na(dataItemSubTypes),
                                      yes = dataItemTypes,
                                      no = paste0(dataItemTypes,'-',dataItemSubTypes))
                               , '>')
      dataItemXpaths <- paste0(new_xpath, dataItemXpaths)
      output <<- c(output, list(
        data.frame(dataItemID = dataItemIDs,
                   dataItemName = dataItemNames,
                   xpath = dataItemXpaths)
      ))
    }
    
    if('components' %in% names(subelement)) {
      temp <- lapply(names(subelement$components), function(x) {
        construct_xpath(subelement = subelement$components[[x]], current_xpath = new_xpath, current_element_type = x)
      })
    }
    
    return(NULL)
  }
  
  temp <- construct_xpath(probeJSON)
  
  output <- rbindlist(output, use.names = TRUE, fill = FALSE)
  
  output$xpath <- str_replace_all(output$xpath, '^:', replacement = '')
  
  output$deviceUUID <- probeJSON$uuid
  
  return(as.data.frame(output) %>% select(deviceUUID, dataItemID, dataItemName, xpath) )
}
