## ------------------------------------------------------------------------
library(mtconnectR)
library(ggplot2)
library(dplyr)

file_path_dmtcd = "extdata/data_truncated.bz2"
file_path_xml   = "extdata/Devices.xml"

# Reading MTC Data
device_name = get_device_info_from_xml(system.file(file_path_xml,package = "mtconnectR"))$name[2]
mtc_device = create_mtc_device_from_dmtcd(system.file(file_path_dmtcd,package = "mtconnectR"),     system.file(file_path_xml,package = "mtconnectR"),device_name)

