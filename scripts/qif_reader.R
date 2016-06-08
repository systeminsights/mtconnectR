if(!require("devtools")) install.packages("devtools")
if(!require("mtconnectR")) devtools::install_github("systeminsights/mtconnectR")
if(!require("ggplot2")) install.packages("ggplot2")
if(!require("magrittr")) install.packages("magrittr")
if(!require("dtw")) install.packages("dtw")
if(!require("tidyr")) install.packages("tidyr")

library(mtconnectR)
library(plyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)
library(magrittr)
library(dtw)
library(tidyr)

# gcode_file_path = "../data/nist_test_bed/827-9999-904, OP1.NC"
file_path_qif = "../data/nist_test_bed/qif/sampleInstanceFiles/Part1Plan.QIF"

qif_xml =  XML::xmlParse(file_path_qif)
qif_list = XML::xmlToList(qif_xml)

qif_list
