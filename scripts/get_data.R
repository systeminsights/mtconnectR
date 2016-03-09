
suppressPackageStartupMessages({
  install.packages("mtconnectR")
  library(mtconnectR, quietly = T)
  library(dplyr, quietly = T)
})

args = commandArgs(trailingOnly = T)
file_path_dmtcd = args[1] %>% normalizePath()
file_path_xml = args[2] %>% normalizePath()
device_name = args[3]
file_path_output = args[4] 
type = args[5]

if(is.na(type)) type = "long"
if(is.na(file_path_output)) file_path_output = "output.csv" 

message("Creating data of ", type, " format for device: ", device_name, " from DMTCD file: ", file_path_dmtcd,
        " using XML file: ", file_path_xml, " for contextualization into output file: ", file_path_output)

mtc_device = create_mtc_device_from_dmtcd(file_path_dmtcd, file_path_xml, device_name)

if(type == "long") output = getData(mtc_device)
if(type == "wide") output = merge(mtc_device)

write.csv(output, file_path_output, row.names = F)


