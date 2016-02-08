
library("testthat")

context("find_line_type")
log_data = c("*This is a command",
             "2015-01-01T07:07:07|This is time series",
             "2015-01-01T07:07:07|@ASSET@|",
             "Unknown Type")

expect_equal(vapply(log_data, find_line_type, "", USE.NAMES = F), c("COMMAND", "TS", "ASSET", "UNKNOWN"))

context("create_device_from_adapter_data")
file_path_adapter_log = "tests/dataExtraction/test_log_data.log"
file_path_xml = "tests/dataExtraction/test_devices.xml"
device_xml_name = "test_device"
mtc_device = create_mtc_device_from_adapter_data(
  system.file(file_path_adapter_log, package = "mtconnectR"),
  system.file(file_path_xml, package = "mtconnectR"),
  device_xml_name)

expected = readRDS(system.file("tests/dataExtraction/test_device_mtcdevice.rds", package = "mtconnectR"))

expect_equal(mtc_device, expected)
