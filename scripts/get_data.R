
if(!require("mtconnectR")) devtools::install_github("systeminsights/mtconnectR")

args = commandArgs(trailingOnly = T)
file_path_dmtcd = args[1]
file_path_xml = args[1]
device_name = args[3]
file_path_output = args[4]
type = args[5]
if(type == "") type = "long"

mtc_device = create_mtc_device_from_dmtcd(file_path_dmtcd, file_path_xml, device_names)

if(type == "long") output = getData(mtc_device)
if(type == "wide") output = merge(mtc_device)

write.csv(output, file_path_output, row.names = F)