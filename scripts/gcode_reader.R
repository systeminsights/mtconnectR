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

file_path_dmtcd = "../data/ucb_data/Part36061.txt"
file_path_xml   = "../data/ucb_data/Devices.xml"

device_name_1 = get_device_info_from_xml(file_path_xml)$name[1]
device_name_2 = get_device_info_from_xml(file_path_xml)$name[2]
# mtc_device = create_mtc_device_from_dmtcd(file_path_dmtcd, file_path_xml, device_name_1)

# gcode_file_path = "../data/nist_test_bed/827-9999-904, OP1.NC"
gcode_file_path = "../data/ucb_data/Part3.txt"
gcode_parsed = parse_gcode(gcode_file_path)
simulated_gcode_data = simulate_data_from_gcode(gcode_parsed, start_time = 0, data_res = 0.05, data_type = "HH") %>% na.omit()

mtc_device_sim = create_mtc_device_from_ts(simulated_gcode_data)

mtc_sim_mapped = map_gcode_mtc(mtc_device_sim, mtc_device)

# Mapping 
plot_twoway(mtc_sim_mapped, mtc_device_sim, mtc_device)


# X Position Comparisons
merged_sim = merge(mtc_sim_mapped, "x_pos|position_x")
merged_sim = merge(mtc_sim_mapped, "position|feed")
names(merged_sim) = extract_param_from_xpath(names(merged_sim))

merged_sim = merged_sim %>% na.omit %>% 
  mutate(feed_ratio = abs(round(((path_feedrate)/(feed + 1e-6)), 2))) %>% 
  mutate(feed_ratio = replace(feed_ratio, feed_ratio > 1, 1))

ggplot(merged_sim) +
  geom_path(aes(x = path_position_x, y = path_position_y, col = feed_ratio)) +
  scale_colour_gradient2(low="red", mid = "green", high = "red", midpoint = 1)

ggplot(merged_sim) +
  geom_path(aes(x = timestamp, y = path_position_y, col = feed_ratio)) +
  scale_colour_gradient2(low="red", mid = "green", high = "red", midpoint = 1)


# ggplot(merged_sim) +
#   geom_path(aes(x = path_position_y, y = path_position_z, col = feed_ratio)) +
#   scale_colour_gradient(low="red")
# 
# 
# # Simulated vs actual 
# pos_mtc = merge(mtc_sim_mapped, "PATH_POSITION|SPEED>|FEEDRATE-ACTUAL>") %>% na.omit()
# names(pos_mtc) = extract_param_from_xpath(names(pos_mtc), show_warnings = F)
# pos_sim = merge(mtc_sim_mapped, "pos$|speed$|feed$") 
# pos_sim = pos_sim[,c("timestamp", "feed", "x_pos", "y_pos", "z_pos", "speed")]
# names(pos_mtc) = names(pos_sim)
# 
# df_plot = rbind(pos_mtc %>% mutate(type = "mtc"), pos_sim %>% mutate(type = "sim"))
# 
# ggplot(df_plot, aes(x = timestamp)) +
#   geom_step(aes(y = x_pos), col = 'red') +
#   geom_step(aes(y = y_pos), col = 'blue') +
#   geom_step(aes(y = z_pos), col = 'green') +
#   geom_step(aes(y = speed/100), col = 'black') +
#   # geom_step(aes(y = feed/10), col = 'maroon') +
#   coord_cartesian(ylim = c(-50, 50)) +
#   facet_grid(type ~ .)