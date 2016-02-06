parse_devicexml_for_a_device <- function(file_path, device_xml_name, mtconnectVersion = '1.2') {
  parse_xml <- XML::xmlParse(file = file_path)
  xpath_query_string <- paste0("//ns:Device[@name='", device_xml_name, "']")
  XML::getNodeSet(doc = XML::xmlRoot(parse_xml), path = xpath_query_string, namespaces = c(ns = paste0("urn:mtconnect.org:MTConnectDevices:", mtconnectVersion)))[[1]]
}

data_items_in_devicexml <- function(parsed_devicexml, mtconnectVersion = '1.2') {
  XML::xpathApply(
    parsed_devicexml, ".//ns:DataItem", 
    namespaces = c(ns = paste0("urn:mtconnect.org:MTConnectDevices:", mtconnectVersion)),
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

get_xpaths_from_xml <- function(xml_file_path, device_xml_name, mtconnectVersion = '1.2') {
  
  parsed_xml = parse_devicexml_for_a_device(xml_file_path, device_xml_name, mtconnectVersion)

  ans = data_items_in_devicexml(parsed_xml, mtconnectVersion) %>%
    mutate(xpath = paste0(device_xml_name, '<Device>:',
                          name, '<', ifelse(is.na(subType), type, paste0(type,'-',subType)), '>'))
  
  
}
