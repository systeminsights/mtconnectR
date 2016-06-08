if(!require("devtools")) install.packages("devtools")
if(!require("mtconnectR")) devtools::install_github("systeminsights/mtconnectR")
if(!require("ggplot2")) install.packages("ggplot2")

library(mtconnectR)
library(dplyr)
library(stringr)
library(ggplot2)
library(reshape2)


gcode_file_path = "../Data Examples/mfgfiles/Part1[1].txt"
file_path_dmtcd = "../Data Examples/mfgfiles/Part16061_cleaned_1.txt"
file_path_xml = "../Data Examples/mfgfiles/Devices.xml"

device_name_1 = get_device_info_from_xml(file_path_xml)$name[1]
device_name_2 = get_device_info_from_xml(file_path_xml)$name[2]

mtc_device = create_mtc_device_from_dmtcd(file_path_dmtcd, file_path_xml, device_name_1)


gcode_dict = read.csv2("inst/gcode_dict.csv", sep = ",")
gcode_lines = readLines(gcode_file_path, warn = F)




head(gcode_lines)


#



#####
# DTW - SS
ss_dtw = dtw(ss_merge$speed, ss_merge$S1speed, 
             open.begin = T, open.end = T,
             step.pattern = asymmetric, keep = T)
plot(ss_dtw, type = "twoway", offset = 4000, match.indices = 200)

ss_dtw$costMatrix
ss_dtw$directionMatrix
ss_dtw$jmin
ss_dtw$distance
ss_dtw$normalizedDistance
ss_dtw$index1
ss_dtw$index2
ss_dtw$index1
ss_dtw$index1


# Get distance matrix
# Position
pos_mtc = merge(mtc_device, "PATH_POS|TOOL") %>% na.omit()
names(pos_mtc) = extract_param_from_xpath(names(pos_mtc), show_warnings = F)

test = pos_mtc[1:10,2:4]
rownames(test) = NULL
A = proxy::dist(test)
A
nrow(A)
ncol(A)



ggplot(pos_mtc, aes(x = timestamp)) +
  geom_line(aes(y = path_position_x), col = 'red') +
  geom_line(aes(y = path_position_y), col = 'green') +
  geom_line(aes(y = path_position_z), col = 'blue')  +
  coord_cartesian(ylim = c(-50, 50))

ggplot(simulated_gcode_data, aes(x = timestamp)) +
  geom_step(aes(y = x_pos), col = 'red') +
  geom_step(aes(y = y_pos), col = 'green') +
  geom_step(aes(y = z_pos), col = 'blue') +
  coord_cartesian(ylim = c(-50, 50))


pos_sim = merge(mtc_device_sim, "pos")
pos_merge = mergeTS(list(pos_data %>% select(timestamp, path_position_x),
                         simulated_gcode_data %>% select(timestamp, x_pos)), use_list_names = F) %>% 
  na.omit() 

gcode_mtc_dtw = dtw(A$x_pos, A$path_position_x, open.begin = T, open.end = T, 
                    window.type = "sakoechiba", window.size = 300,
                    step.pattern = asymmetric,keep = T)
plot(gcode_mtc_dtw)
dtwPlotTwoWay(gcode_mtc_dtw, offset = 100, match.indices = 200)
gcode_mtc_dtw$normalizedDistance

A = gcode_mtc_dtw$localCostMatrix
head(A)
gcode_mtc_dtw$windowFunction()
gcode_mtc_dtw$
  
  A[1:10,1:10]
?plot.dtw
# A1 = gcode_mtc_dtw$index1
gcode_mtc_dtw$norma
A$index1 = gcode_mtc_dtw$index1
A$index2 = gcode_mtc_dtw$index2

###
# Spindle speed
ss_mtc = merge(mtc_device, "SPEED>") %>% na.omit()
names(ss_mtc) = extract_param_from_xpath(names(ss_mtc), show_warnings = F)
ss_sim = merge(mtc_device_sim_mapped, "speed") 
ts_ref = data.frame(timestamp = seq(min(ss_mtc$timestamp), max(ss_mtc$timestamp), by = 0.2))

ss_merge =  mergeTS(list(ts_ref,
                         ss_mtc %>% select(timestamp, S1speed),
                         ss_sim %>% select(timestamp, speed)), use_list_names = F) %>% 
  na.omit()

ggplot(ss_merge, aes(x = 1:nrow(ss_merge))) +
  geom_line(aes(y = S1speed)) +
  geom_line(aes(y = speed, col = 'red'))

# Position
pos_mtc = merge(mtc_device, "PATH_POSITION") %>% na.omit()
names(pos_mtc) = extract_param_from_xpath(names(pos_mtc), show_warnings = F)
pos_sim = merge(mtc_device_sim_mapped, "pos") 
names(pos_mtc) = names(pos_sim)

df_plot = rbind(pos_mtc %>% mutate(type = "mtc"), pos_sim %>% mutate(type = "sim"))

ggplot(df_plot, aes(x = timestamp)) +
  geom_step(aes(y = x_pos), col = 'red') +
  geom_step(aes(y = y_pos), col = 'blue') +
  geom_step(aes(y = z_pos), col = 'green') +
  coord_cartesian(ylim = c(-50, 50)) +
  facet_grid(type ~ .)

####

ggplot(pos_sim, aes(x = timestamp)) +
  geom_step(aes(y = x_pos, col = 'red')) +
  geom_step(aes(y = y_pos, col = 'blue')) +
  geom_step(aes(y = z_pos, col = 'green')) 


scale_colour_gradient(low="red")

ggplot(pos_sim) + geom_path(aes(x = x_pos, y = y_pos))
ggplot(pos_sim) + geom_path(aes(x = y_pos, y = z_pos))
ggplot(pos_sim) + geom_path(aes(x = x_pos, y = z_pos))


gcode_library = read.csv2("inst/gcode_library.csv", sep = ",") %>%
  mutate(supported = 0) %>% select(1:5, 7, 6)
write.csv(gcode_library, "inst/gcode_library.csv", row.names = F)
