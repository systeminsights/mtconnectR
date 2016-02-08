## ------------------------------------------------------------------------
require(mtconnectR)
file_path_adapter_log = "tests/dataExtraction/test_log_data.log"
file_path_xml = "tests/dataExtraction/test_devices.xml"
device_name = "test_device"

mtc_device = create_mtc_device_from_adapter_data(
  system.file(file_path_adapter_log, package = "mtconnectR"),
  system.file(file_path_xml, package = "mtconnectR"),
  device_name)

print(summary(mtc_device))


