library(mtconnectR)
file_path_adapter_log = "../data/adapter_logs/tft/tft-405-pfh.log"
file_path_xml = "../data/adapter_logs/tft/Devices.xml.txt"
device_xml_name = "TFT-405-PFH"

A = create_mtcdevice_from_adapter_data(file_path_adapter_log, file_path_xml, device_xml_name)

