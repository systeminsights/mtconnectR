
data_item_name_map_speed <- function(sim_data_item_names, mtc_data_item_names){
  mtc_data_item_names[mtc_data_item_names %>% str_detect("<SPINDLE_SPEED>")]
}

data_item_name_map_position <- function(sim_data_item_names, mtc_data_item_names){
  mtc_data_item_names[mtc_data_item_names %>% str_detect("<PATH_POSITION>")] %>% sort
}

data_item_name_map_feed <- function(sim_data_item_names, mtc_data_item_names){
  mtc_data_item_names[mtc_data_item_names %>% str_detect("<PATH_FEEDRATE-ACTUAL>")]
}

find_best_data_item_map <- function(mtc_device_sim, mtc_device){
  sim_data_item_names = names(mtc_device_sim@data_item_list)
  mtc_data_item_names = names(mtc_device@data_item_list)

  speed_map = data_item_name_map_speed(sim_data_item_names, mtc_data_item_names)
  # feed_map = data_item_name_map_feed(sim_data_item_names, mtc_data_item_names)
  pos_map = data_item_name_map_position(sim_data_item_names, mtc_data_item_names)

  output = NULL
  if(length(pos_map) != 0) output = output %>% rbind(data.frame(sim_name = c("x_pos", "y_pos","z_pos"), mtc_name = pos_map))
  # if(length(feed_map) != 0) output = output %>% rbind(data.frame(sim_name = c("pfr"), mtc_name = feed_map))
  if(length(speed_map) != 0) output = output %>% rbind(data.frame(sim_name = c("rot_vel"), mtc_name = speed_map))

  output
}

standardize_times <- function(mtc_merged, data_res = 0.2){
  contains = everything = NULL
  timestamp = NULL
  start_end = range(mtc_merged$timestamp)
  reference_series = data.frame(timestamp = seq(start_end[1], start_end[2] + data_res, by = data_res))
  merged_ref_series = mtc_merged %>% rbind.fill(reference_series)
  merged_ref_series %>% arrange(timestamp) %>%
    tidyr::fill(everything()) %>%
    filter(timestamp %in% reference_series$timestamp) %>%
    unique()
}


find_distance_matrix <- function(sim_merged_std, mtc_merged_std){
  timestamp = NULL
  dist_variables_mtc = mtc_merged_std %>% select(-timestamp) #%>%  scale(center = F, scale = T)
  dist_variables_sim = sim_merged_std %>% select(-timestamp) #%>%  scale(center = F, scale = T)

  proxy::dist(dist_variables_sim, dist_variables_mtc)
}


#' Create a mapping between simulated and actual data
#'
#' Creates a timestamp based mapping to map every simulated timestamp to an actual timestamp based on real data
#'
#' @param mtc_device_sim is the simlated version
#' @param mtc_device is the actual log data
#' @param elasticity is the maximum consecutive reference elements skippable (passed to dtw::mvmStepPattern())
#' @examples 
#' data("example_gcode_parsed") # Parsed gcode
#' data("example_mtc_device_3") # MTCDevice object of actual log data
#' simulated_gcode_data = na.omit(simulate_data_from_gcode(example_gcode_parsed, 
#' start_time = 0, data_res = 0.1, data_type = "HH"))
#' mtc_device_sim = create_mtc_device_from_ts(simulated_gcode_data)
#' mtc_sim_mapped = map_gcode_mtc(mtc_device_sim, example_mtc_device_3, elasticity = 200)

#' @export
 
map_gcode_mtc <- function(mtc_device_sim, mtc_device, elasticity = 2){
  timestamp = one_of = NULL
  data_item_map = find_best_data_item_map(mtc_device_sim, mtc_device)

  message("Using the following mapping: ")
  print(data_item_map)

  mtc_merged = merge(mtc_device, paste0("^", paste0(data_item_map$mtc_name, collapse = "$|^"), "$")) %>% stats::na.omit()
  mtc_merged = mtc_merged %>% select(timestamp, one_of(data_item_map$mtc_name))
  names(mtc_merged) = extract_param_from_xpath(names(mtc_merged), show_warnings = F)
  sim_merged = merge(mtc_device_sim, paste0("rot_vel|pfr|pos|line_id")) %>% stats::na.omit()
  sim_merged = sim_merged #%>% select(timestamp, one_of(data_item_map$sim_name), line_id)

  mtc_merged_std = mtc_merged %>% standardize_times() #%>% slice(1:300)
  sim_merged_std = sim_merged %>% standardize_times() %>% stats::na.omit() #%>% slice(1:100)

  dist_matrix = find_distance_matrix(sim_merged_std %>% select(timestamp, one_of(data_item_map$sim_name)),
                                     mtc_merged_std)
  dtw_distance = dtw::dtw(dist_matrix, step.pattern = dtw::mvmStepPattern(elasticity), keep = T,
      open.end = T, open.begin = T)
  message("Normalized Distance: ", dtw_distance$normalizedDistance)
  dtw_distance$index2
  # temp = data.frame(index = dtw_distance$index1, value = 1:length(dtw_distance$index1)) %>% clean_reduntant_rows("index")

  sim_mapped = data.frame(timestamp = mtc_merged_std$timestamp[dtw_distance$index2],
             sim_merged_std %>% dplyr::rename(sim_timestamp = timestamp))

  mtc_device_sim_mapped = create_mtc_device_from_ts(sim_mapped)
  mtc_device@data_item_list = c(mtc_device@data_item_list, mtc_device_sim_mapped@data_item_list)
  #   A = mtconnectR::merge(mtc_device, "_pos|FEED|pfr|ror|speed")
  #   names(A) = extract_param_from_xpath(names(A))
  mtc_device
}


#' To plot the mapping between the simulated and actual versions
#'
#' This function is a wrapper around the dtwPlotTwoWay from the dtw package in R. 
#' 
#' In our case, this function can be used to compare the variation of a single variable
#' in the simulated and actual data. The simulated and actual data are represented 
#' on the same y axis with the time representing the x axis. An offset is provided to the
#' simulated data to easily disinguish the two. Data points that are mapped to each other
#' are connected by a line segment between the two fraphs
#'
#' @param mtc_sim_mapped is the mapping between simulated and actual data
#' @param mtc_device_sim is the simulated data
#' @param mtc_device is the actual log data
#' @param offset is the amount by which the simulated data is shifted in the y axis
#' @param total_maps is the number of mapping line segment between the two plots 
#' @param mtc_map_string is the name of the data item to match in the actual data
#' @param sim_map_string is the name of the data item to match in the simulated data
#' @export
#' @examples 
#' data("example_mtc_device_3")   # MTCDevice object of actual log data
#' data("example_mtc_device_sim") # Simulated gcode 
#' data("example_mtc_sim_mapped") # Mapping between simulated and actual data
#' mapping_ggplot = plot_twoway(example_mtc_sim_mapped, example_mtc_device_sim, 
#' example_mtc_device_3,offset = 20, total_maps = 100)
#' 
plot_twoway <- function(mtc_sim_mapped, mtc_device_sim, mtc_device, offset = 100,
                        total_maps = 50, mtc_map_string = "path_pos_x", sim_map_string = "x_pos"){

  timestamp = path_pos_x = x_pos = sim_timestamp = NULL
  map_char = paste("sim_timestamp", sim_map_string, mtc_map_string, sep = "|")
  timestamp_mapping = mtconnectR::merge(mtc_sim_mapped, map_char) %>% stats::na.omit()
  names(timestamp_mapping) = extract_param_from_xpath(names(timestamp_mapping), show_warnings = F)

  merged_mtc = mtconnectR::merge(mtc_device, mtc_map_string) %>% stats::na.omit()
  names(merged_mtc) = extract_param_from_xpath(names(merged_mtc), show_warnings = F)
  merged_sim = mtconnectR::merge(mtc_device_sim, sim_map_string)
  merged_sim[[2]] = merged_sim[[2]] - offset

  timestamp_mapping_filtered = timestamp_mapping %>% clean_reduntant_rows(sim_map_string) %>%
    rename_("path_pos_x" = mtc_map_string, "x_pos" = sim_map_string)
  merged_mtc = merged_mtc %>% select_("timestamp", mtc_map_string) %>% rename_("path_pos_x" = mtc_map_string)
  merged_sim = merged_sim %>% select_("timestamp", sim_map_string) %>% rename_("x_pos" = sim_map_string)

  ggplot() +
    geom_step(data = merged_mtc, aes(x = timestamp, y = path_pos_x, col = 'brown')) +
    geom_step(data = merged_sim, aes(x = as.numeric(timestamp) + timestamp_mapping$timestamp[1], y = x_pos)) +
    geom_segment(data = timestamp_mapping_filtered[seq(1, nrow(timestamp_mapping_filtered),length.out = total_maps),],
                 aes(x = timestamp, y = path_pos_x,
                     xend = sim_timestamp + timestamp[1], yend = x_pos - offset, colour = "grey"))
}
