
library("testthat")

file_path_adapter_log = "tests/dataExtraction/test_log_data.log"
file_path_xml = "tests/dataExtraction/test_devices.xml"
device_name = "test_device"


#===============================================================================
context("create_device_from_adapter_data")
mtc_device = create_mtc_device_from_adapter_data(
  system.file(file_path_adapter_log, package = "mtconnectR"),
  system.file(file_path_xml, package = "mtconnectR"),
  device_name)

data("example_mtc_device")
expect_equal(mtc_device, example_mtc_device)

#===============================================================================
context("get_device_info_from_xml")
devices_info = get_device_info_from_xml(system.file(file_path_xml, package = "mtconnectR"))

expected = data.frame(name = c("test_device", "test_device_2"),
                      uuid = c("test_device_uuid", "test_device_2_uuid"),
                      id = c("id_1234", "id_5678"))
expect_equal(expected, devices_info)


#===============================================================================
context("get_xpaths_from_xml")
xpath_info = get_xpaths_from_xml(system.file(file_path_xml, package = "mtconnectR"), device_name)
data("example_xpath_info")

expect_equal(xpath_info, example_xpath_info)

#===============================================================================

context("read_adapter_log_file")
condition_names = c("servo_cond", "logic_cond")
log_data = read_adapter_log_file(system.file(file_path_adapter_log, package = "mtconnectR"), condition_names)
data("example_log_data")
expect_equal(log_data, example_log_data)

#===============================================================================

