
library("testthat")

context("find_line_type")
log_data = c("*This is a command",
             "2015-01-01T07:07:07|This is time series",
             "2015-01-01T07:07:07|@ASSET@|",
             "Unknown Type")

expect_equal(vapply(log_data, find_line_type, "", USE.NAMES = F), c("COMMAND", "TS", "ASSET", "UNKNOWN"))

context("create_device_from_adapter_data")
file_path_adapter_log = "extdata/tft-405-pfh.log"
file_path_xml = "extdata/Devices.xml.txt"
device_xml_name = "TFT-405-PFH"
mtc_device = create_mtc_device_from_adapter_data(
  system.file(file_path_adapter_log, package = "mtconnectR"),
  system.file(file_path_xml, package = "mtconnectR"),
  device_xml_name)