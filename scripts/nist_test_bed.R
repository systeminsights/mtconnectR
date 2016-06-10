library(mtconnectR)
library(ggplot2)
library(dplyr)

file_path_dmtcd = "inst/extdata/data_truncated.bz2"
file_path_xml   = "inst/extdata/Devices.xml"

# Reading MTC Data
device_name = get_device_info_from_xml(file_path_xml)$name[2]
mtc_device = create_mtc_device_from_dmtcd(file_path_dmtcd, file_path_xml, device_name)
calc_feed = calculated_feed_from_position(mtc_device) %>%
  mutate(value = replace(value, value < 0.1, 0)) %>% 
  clean_reduntant_rows()
mtc_device = add_data_item_to_mtc_device(mtc_device, calc_feed, data_item_type = "Sample",
                                         data_item_name = "pfr_calculated<PATH_FEEDRATE>",
                                         source_type = "calculated")

pos_data_mtc = merge(mtc_device, "path_pos") %>% filter(timestamp > as.POSIXct("2016-03-22"))
names(pos_data_mtc) = extract_param_from_xpath(names(pos_data_mtc), show_warnings = F)

# Reading G Code data
gcode_file_path_1 = "../data/nist_test_bed/planned/the_part/827-9999-904.H OPERATION #1.NC"
# gcode_file_path_2 = "../data/nist_test_bed/planned/the_part/827-9999-904.H OPERATION #2.NC"
# gcode_file_path_2 = "../data/nist_test_bed/planned/827-9999-904, OP2.NC"

gcode_parsed_1 = parse_gcode(gcode_file_path_1)
# gcode_parsed_2 = parse_gcode(gcode_file_path_2)

# Simulating G Code data
simulated_gcode_data_1 = simulate_data_from_gcode(gcode_parsed_1, start_time = 0, data_res = 0.1, data_type = "HH") %>% 
  na.omit()
# simulated_gcode_data_2 = simulate_data_from_gcode(gcode_parsed_2, start_time = 0, data_res = 0.1, data_type = "HH") %>% 
#   na.omit()

mtc_device_sim_1 = create_mtc_device_from_ts(simulated_gcode_data_1)
# mtc_device_sim_2 = create_mtc_device_from_ts(simulated_gcode_data_2)

# pos_data_sim_2 = merge(mtc_device_sim_2, "pos")
pos_data_sim_1 = merge(mtc_device_sim_1, "pos")
# Subsetting individual operations in MTC data
ggplot(pos_data_mtc) + geom_path(aes(x = timestamp, y = path_pos_y)) 

# time_start = as.POSIXct("2016-03-22 08:30:00", tz = "UTC") 
# time_split_1 = as.POSIXct("2016-03-22 10:30:00", tz = "UTC") 
time_split_2 = as.POSIXct("2016-03-22 12:45:00", tz = "UTC") 
time_split_3 = as.POSIXct("2016-03-22 13:00:00", tz = "UTC") 
# time_end = as.POSIXct("2016-03-22 15:30:00", tz = "UTC") 

# pos_data_mtc_1 = pos_data_mtc %>% filter(timestamp > time_start   & timestamp < time_split_1)
# pos_data_mtc_2 = pos_data_mtc %>% filter(timestamp > time_split_1 & timestamp < time_split_2)
pos_data_mtc_3 = pos_data_mtc %>% filter(timestamp > time_split_2 & timestamp < time_split_3)
# pos_data_mtc_4 = pos_data_mtc %>% filter(timestamp > time_split_3 & timestamp < time_end)

# ggplot(pos_data_mtc_1) + geom_path(aes(x = path_pos_x, y = path_pos_y)) 
# ggplot(pos_data_mtc_2) + geom_path(aes(x = path_pos_x, y = path_pos_y))
ggplot(pos_data_mtc_3) + geom_path(aes(x = path_pos_x, y = path_pos_y)) 
# ggplot(pos_data_mtc_4) + geom_path(aes(x = path_pos_x, y = path_pos_y)) 

# Plotting positions data
ggplot(pos_data_sim_1) + geom_path(aes(x = x_pos, y = y_pos)) +
  coord_cartesian(xlim = c(-2.5, 6), ylim = c(-4, 1))
ggsave("../data/nist_test_bed/results/1_op1_sim.png", width = 15, height = 10, dpi = 500)

ggplot(pos_data_mtc_3) + geom_path(aes(x = path_pos_x, y = path_pos_y)) +
  coord_cartesian(xlim = c(-2.5, 6), ylim = c(-4, 1))
# ggsave("../data/nist_test_bed/results/2_op1_mtc.png", width = 15, height = 10, dpi = 500)
# 
# ggplot(pos_data_sim_2) + geom_path(aes(x = x_pos, y = y_pos)) +
#   coord_cartesian(xlim = c(-5, 1), ylim = c(-3, 1))
# ggsave("../data/nist_test_bed/results/3_op2_sim.png", width = 15, height = 10, dpi = 500)
# 
# ggplot(pos_data_mtc_2) + geom_path(aes(x = path_pos_x, y = path_pos_y)) +
#   coord_cartesian(xlim = c(-5, 1), ylim = c(-3, 1))
# ggsave("../data/nist_test_bed/results/4_op2_mtc_1.png", width = 15, height = 10, dpi = 500)
# 
# ggplot(pos_data_mtc_4) + geom_path(aes(x = path_pos_x, y = path_pos_y)) +
#   coord_cartesian(xlim = c(-5, 1), ylim = c(-3, 1))
# ggsave("../data/nist_test_bed/results/5_op2_mtc_2.png", width = 15, height = 10, dpi = 500)

# Mapping

time_start_op1 = as.POSIXct("2016-03-22 12:45:00", tz = "UTC")
time_end_op1 = as.POSIXct("2016-03-22 12:46:00", tz = "UTC")
# time_start_op2_1 = as.POSIXct("2016-03-22 11:20:00", tz = "UTC")
# time_end_op2_1 = as.POSIXct("2016-03-22 11:40:00", tz = "UTC")
# time_start_op2_2 = as.POSIXct("2016-03-22 14:00:00", tz = "UTC")
# time_end_op2_2 = as.POSIXct("2016-03-22 14:20:00", tz = "UTC")


mtc_device_op1_1 = mtc_device %>% filter_timestamps_mtc_device(time_start_op1, time_end_op1)
# mtc_device_op2_1 = mtc_device %>% filter_timestamps_mtc_device(time_start_op2_1, time_end_op2_1)
# mtc_device_op2_2 = mtc_device %>% filter_timestamps_mtc_device(time_start_op2_2, time_end_op2_2)

mtc_sim_mapped_op1 = map_gcode_mtc(mtc_device_sim_1, mtc_device_op1_1, elasticity = 200)
# mtc_sim_mapped_op2_1 = map_gcode_mtc(mtc_device_sim_2, mtc_device_op2_1, elasticity = 200)
# mtc_sim_mapped_op2_2 = map_gcode_mtc(mtc_device_sim_2, mtc_device_op2_2, elasticity = 200)

# Plotting positions data
plot_twoway(mtc_sim_mapped_op1, mtc_device_sim_1, mtc_device_op1_1, 20, 100)
# ggsave("../data/nist_test_bed/results/xpos_mapping_mtc_sim_op1.png", width = 15, height = 10, dpi = 500)
# plot_twoway(mtc_sim_mapped_op2_1, mtc_device_sim_2, mtc_device_op2_1, 20, 100)
# ggsave("../data/nist_test_bed/results/xpos_mapping_mtc_sim_op2_1.png", width = 15, height = 10, dpi = 500)
# plot_twoway(mtc_sim_mapped_op2_2, mtc_device_sim_2, mtc_device_op2_2, 20, 100)
# ggsave("../data/nist_test_bed/results/xpos_mapping_mtc_sim_op2_2.png", width = 15, height = 10, dpi = 500)


# Simulated vs actual 
# feed_compare_op2_2 = merge(mtc_sim_mapped_op2_2, "pfr") %>% na.omit()
# names(feed_compare_op2_2) = extract_param_from_xpath(names(feed_compare_op2_2), show_warnings = F)
# 
# ggplot(feed_compare_op2_2, aes(x = timestamp)) + 
#   geom_step(aes(y = pfr_calculated, colour = 'Calculated'), size = 1) + 
#   geom_step(aes(y = pfr, colour = "Simulated"), size = 1) + 
#   coord_cartesian(ylim = c(-3, 300))
# ggsave("../data/nist_test_bed/results/feed_compare_op2_2.png", width = 15, height = 10, dpi = 500)
# 
# pos_feed_ratio_compare_op2_2 = merge(mtc_sim_mapped_op2_2, "pfr|_pos_") %>% na.omit()
# names(pos_feed_ratio_compare_op2_2) = extract_param_from_xpath(names(pos_feed_ratio_compare_op2_2), show_warnings = F)
# 
# pos_feed_ratio_compare_op2_2 = pos_feed_ratio_compare_op2_2 %>%
#   mutate(feed_ratio = abs(round(((pfr_calculated)/(pfr + 1e-6)), 2))) %>% 
#   mutate(feed_ratio = replace(feed_ratio, feed_ratio > 1.1, NA_real_))
# 
# ggplot(pos_feed_ratio_compare_op2_2) +
#   geom_path(aes(x = path_pos_x, y = path_pos_y, col = feed_ratio), size = 1) +
#   scale_colour_gradient(low="#91bfdb", high = "#fc8d59", na.value = "#ffffbf") + 
#   coord_cartesian(xlim = c(-5, 1), ylim = c(-3, 0.5))
# ggsave("../data/nist_test_bed/results/position_feed_ratio_op2_2.png", width = 15, height = 10, dpi = 500)

# Save Data
# Feed calculated,
mtc_device_op1_1_merged = merge(mtc_device_op1_1)
# mtc_device_op2_1_merged = merge(mtc_device_op2_1)
mtc_sim_mapped_op1_merged = merge(mtc_sim_mapped_op1)
# mtc_sim_mapped_op2_1_merged = merge(mtc_sim_mapped_op2_1)

# write.csv(mtc_device_op1_1_merged , "../data/nist_test_bed/results/mtc_device_op1_1_merged.csv", row.names = F)
# write.csv(mtc_device_op1_1_merged, "../data/nist_test_bed/results/mtc_device_op1_1_merged.csv", row.names = F)
# write.csv(gcode_parsed_1, "../data/nist_test_bed/results/gcode_parsed_1.csv", row.names = F)
# write.csv(gcode_parsed_2, "../data/nist_test_bed/results/gcode_parsed_2.csv", row.names = F)
# write.csv(simulated_gcode_data_1, "../data/nist_test_bed/results/simulated_gcode_data_1.csv", row.names = F)
# write.csv(simulated_gcode_data_2, "../data/nist_test_bed/results/simulated_gcode_data_2.csv", row.names = F)
# write.csv(mtc_sim_mapped_op1_merged, "../data/nist_test_bed/results/mtc_sim_mapped_op1_merged.csv", row.names = F)
# write.csv(mtc_sim_mapped_op2_1_merged, "../data/nist_test_bed/results/mtc_sim_mapped_op2_1_merged.csv", row.names = F)
