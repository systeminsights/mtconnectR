## ------------------------------------------------------------------------
require(mtconnectR)
file_path_adapter_log = "tests/dataExtraction/test_log_data.log"
file_path_xml = "tests/dataExtraction/test_devices.xml"
device_name = "test_device"
mtc_device = create_mtc_device_from_adapter_data(
  system.file(file_path_adapter_log, package = "mtconnectR"),
  system.file(file_path_xml, package = "mtconnectR"),
  device_name)


## ------------------------------------------------------------------------
# Get the first data item in the list
mtc_data_item = getDataItem(mtc_device)
print(mtc_data_item)

# Get the first data item in the list
mtc_data_item = getDataItem(mtc_device, "Xabs")
print(mtc_data_item)

# Get the first data item in the list
mtc_data_item = getDataItem(mtc_device, 5)
print(mtc_data_item)

# Get all data items with path matching the string 'POSITION'
mtc_data_item_list = getDataItem(mtc_device, "POSITION")
print(mtc_data_item)

## ------------------------------------------------------------------------

print(summary(mtc_device))
print(summary(mtc_data_item))


## ------------------------------------------------------------------------

mtc_device_data = getData(mtc_device)
print(mtc_device_data)

mtc_data_item_data = getData(mtc_data_item)
print(mtc_data_item_data)


## ------------------------------------------------------------------------

print(merge(mtc_device, "POSIT"))
print(merge(mtc_device, 3:4))

merged_mtc_device = (merge(mtc_device))

# renaming column names to make it more readable
names(merged_mtc_device) = stringr::str_replace(names(merged_mtc_device), "test_device<Device>:", "")
print(merged_mtc_device)


