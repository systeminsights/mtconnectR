## ------------------------------------------------------------------------
require(mtconnectR)
file_path_dmtcd = "extdata/test_dmtc_data_2.log"
file_path_xml = "extdata/test_devices_2.xml"

## ------------------------------------------------------------------------
(device_info = get_device_info_from_xml(system.file(file_path_xml, package = "mtconnectR")))
device_name = device_info$name[1]

## ------------------------------------------------------------------------
xpath_info = get_xpaths_from_xml(system.file(file_path_xml, package = "mtconnectR"), device_name)
conditions_data_items = xpath_info$id[xpath_info$category == "CONDITION"]


