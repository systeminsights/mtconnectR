
library("mtconnectR")

file_path_adapter_log = "../SI/si_data/nist_test_bed/GF.txt"
file_path_xml = "../SI/si_data/nist_test_bed/Devices.xml"
device_name = "nist_testbed_Mazak_QT_1"

A = create_mtc_device_from_adapter_data(file_path_adapter_log, file_path_xml, device_name)
A1 = merge(A)
A@data_item_list
