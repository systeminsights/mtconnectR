max_pfr = 1e6
block_exec_time = 1e-3
spindle_rot_acc = 10000
ang_acc_coeff = 10
lin_acc_coeff = 10

simulate_program <- function(current_values, single_gcode_block){
  current_values$program = as.character(single_gcode_block[['value']])
  current_values
}

simulate_rapid <- function(current_values, single_gcode_block){
  current_values$state_motion = single_gcode_block$type
  current_values$pfr = max_pfr
  current_values
}

# TODO: Why special case for "HH"?
simulate_pfr <- function(current_values, single_gcode_block){
  compensation = ifelse(single_gcode_block$data_type == "HH", 10, 1)
  current_values$pfr = single_gcode_block[['value']] / compensation 
  current_values
}

simulate_rot_vel <- function(current_values, single_gcode_block){
  current_values$rot_vel = single_gcode_block[['value']]
  current_values
}

simulate_tool_change <- function(current_values, single_gcode_block){
  current_values$tool_id = as.character(current_values$state_upcoming_tool)
  current_values
}

simulate_tool_select <- function(current_values, single_gcode_block){
  current_values$state_upcoming_tool = single_gcode_block[['value']]
  current_values
}

simulate_position <- function(current_values, single_gcode_block){
  position_type = paste(tolower(single_gcode_block$subtype), "pos", sep = "_")
  current_values[[position_type]] = single_gcode_block[['value']]
  current_values
}

# TODO: Why special case for "HH"?
simulate_arc_center <- function(current_values, single_gcode_block){
  position_type = paste(tolower(single_gcode_block$subtype), "pos", sep = "_")
  center_type = paste(tolower(single_gcode_block$subtype), "center_pos", sep = "_")
  
  if(single_gcode_block$data_type == "HH") new_position = single_gcode_block$value else
    new_position = current_values[[position_type]] + single_gcode_block$value
  
  current_values[[center_type]] = new_position
  current_values
}

simulate_motion <- function(current_values, single_gcode_block){
  current_values$state_motion = paste(single_gcode_block$type, single_gcode_block$subtype, sep = "_")
  current_values
}

simulate_block_data <- function(current_values, single_gcode_block){
  
  simulation_function = switch(single_gcode_block$type, 
                               "PROGRAM"         = simulate_program, 
                               "MOTION_RAPID"    = simulate_rapid,
                               "TOOL_CHANGE"     = simulate_tool_change,
                               "TOOL_NUMBER"     = simulate_tool_select,
                               "ROTARY_VELOCITY" = simulate_rot_vel,
                               "PATH_FEEDRATE"   = simulate_pfr,
                               "MOTION_LINEAR"   = simulate_motion,
                               "MOTION_ARC"      = simulate_motion,
                               "POSITION"        = simulate_position,
                               "CENTER_POSITION" = simulate_arc_center
  )
  simulation_function(current_values, single_gcode_block)
}

init_data <- function(gcode_parsed, start_time){
  data.frame(timestamp = start_time, line_id = min(gcode_parsed$line), program = "UNKNOWN",
             tool_id = "UNKNOWN", 
             pfr = 0, rot_vel = 0,
             x_pos = 0, y_pos = 0, z_pos = 0,
             x_vel = 0, y_vel = 0, z_vel = 0,
             #x_acc = 0, y_acc = 0, z_acc = 0,
             #spindle_acc =0, 
             state_upcoming_tool = "UNKNOWN", state_motion = "LINEAR",
             x_center_pos = 0, y_center_pos = 0, z_center_pos = 0
  )
}

linear_position_change <- function(current_values, previous_values){
  position_change = c(current_values$x_pos - previous_values$x_pos,
                      current_values$y_pos - previous_values$y_pos,
                      current_values$z_pos - previous_values$z_pos
  )
  if(sum(position_change, na.rm = T) == 0L) return(0)
  sqrt(sum(position_change ^ 2))
}

get_angle_details <- function(current_values, previous_values){
  old_slope = atan2(previous_values$y_pos - current_values$y_center_pos,
                    previous_values$x_pos - current_values$x_center_pos)
  
  new_slope = atan2(current_values$y_pos - current_values$y_center_pos,
                    current_values$x_pos - current_values$x_center_pos)
  
  old_radius = sqrt((previous_values$x_pos - current_values$x_center_pos) ^ 2 +
                      (previous_values$y_pos - current_values$y_center_pos) ^ 2)
  
  new_radius =  sqrt((current_values$x_pos - current_values$x_center_pos) ^ 2 +
                       (current_values$y_pos - current_values$y_center_pos) ^ 2)
  if(abs(old_radius/new_radius - 1) > 0.5) {
    # browser()
    warning("Start, End, Center doesn't match for Arc coordinates")
  }
  list(old_slope = old_slope, new_slope = new_slope, radius = old_radius)

}

arc_position_change <- function(current_values, previous_values){
  angle_details = get_angle_details(current_values, previous_values)
  abs(angle_details$new_slope - angle_details$old_slope) * angle_details$radius
}


pfr_time <- function(current_values, previous_values, pfr, type = "units_per_min"){
  # Will add more rules with time

  if(str_detect(current_values$state_motion, "ARC_"))
    distance_moved = arc_position_change(current_values, previous_values) else
      distance_moved = linear_position_change(current_values, previous_values)
  
  distance_moved * 60 / pfr
}

update_timestamps <- function(current_values, previous_values){
  current_values$timestamp = current_values$timestamp + block_exec_time
  
  pfr_time = pfr_time(current_values, previous_values, current_values$pfr, current_values$state_motion)
  rot_vel_change = abs(current_values$rot_vel - previous_values$rot_vel)
  
  current_values$timestamp = current_values$timestamp + 
    max(rot_vel_change / spindle_rot_acc, pfr_time, na.rm = T)
  
  current_values
}

interpolate_values <- function(current_values, previous_values, data_res, data_type){
  one_of = NULL
  time_difference = as.numeric(current_values$timestamp) - as.numeric(previous_values$timestamp)
  if(time_difference < data_res) return(current_values)
  time_segments = floor(time_difference / data_res) + 1
  interpolate_columns = c("timestamp", "pfr", "rot_vel", "x_pos", "y_pos", "z_pos")
  interpolate_columns = interpolate_columns[!is.na(previous_values[interpolate_columns])]
  updated_columns = lapply(interpolate_columns, function(x) seq(previous_values[[x]], current_values[[x]], length.out = time_segments))
  names(updated_columns) = interpolate_columns
  
  if(str_detect(current_values$state_motion, "ARC_")){
    angle_details = get_angle_details(current_values, previous_values)
    
    if(str_detect(current_values$state_motion, "COUNTER") & data_type != "HH" | 
       !str_detect(current_values$state_motion, "COUNTER") & data_type == "HH") 
      angle_details$new_slope = angle_details$new_slope + 2 * pi
    
    angle_seq = seq(angle_details$old_slope, angle_details$new_slope, length.out = time_segments)
    updated_columns$x_pos = current_values$x_center_pos + angle_details$radius * cos(angle_seq)
    updated_columns$y_pos = current_values$y_center_pos + angle_details$radius * sin(angle_seq)
  }
  
  current_values %>% data.frame() %>% 
    select(-one_of(interpolate_columns)) %>% 
    data.frame(updated_columns) %>% 
    slice(-1) %>% 
    select(one_of(names(previous_values)))
}

calculate_vel_acc <- function(current_values, previous_values){
  travel_length = (
    (previous_values$x_pos - current_values$x_pos) ^ 2 +
    (previous_values$x_pos - current_values$x_pos) ^ 2 +
    (previous_values$x_pos - current_values$x_pos) ^ 2 ) ^ 0.5
    
  x_proj = ((previous_values$x_pos - current_values$x_pos) / travel_length) %>% abs
  y_proj = ((previous_values$y_pos - current_values$y_pos) / travel_length) %>% abs
  z_proj = ((previous_values$z_pos - current_values$z_pos) / travel_length) %>% abs
  
  current_values$x_vel = current_values$pfr * x_proj / 1000 / 60
  current_values$y_vel = current_values$pfr * y_proj / 1000 / 60
  current_values$z_vel = current_values$pfr * z_proj / 1000 / 60
  
#   current_values$x_acc = lin_acc_coeff * x_proj
#   current_values$y_acc = lin_acc_coeff * y_proj
#   current_values$z_acc = lin_acc_coeff * z_proj
  current_values
}

#' Simulate position,velocity and other data from G-code
#'
#' Reads parsed gcode and returns simulated data
#'
#' @param gcode_parsed Parsed gcode 
#' @param start_time Starting time (default 0)
#' @param data_res Resolution for occurrence of a new data point. (seconds) 
#' @param data_type Data type
#' @examples 
#' data("example_gcode_parsed")
#' simulated_data_from_gcode <- simulate_data_from_gcode(example_gcode_parsed,start_time = 0, 
#' data_res = 0.2, data_type = "HH")
#' @export

simulate_data_from_gcode <- function(gcode_parsed, start_time = 0, data_res = 0.2, data_type = "ISO"){
  
  type = supported = state_motion = contains = NULL
  gcode_parsed_filtered = gcode_parsed %>% filter(type != "UNKNOWN" & supported == 1) %>% 
    mutate(data_type = data_type)
  current_values = init_data(gcode_parsed_filtered, start_time); simulated_data = NULL
  i = 2
  for(i in 1:nrow(gcode_parsed_filtered)){
  
    single_gcode_block = gcode_parsed_filtered[i, ]
    current_values = simulate_block_data(current_values, single_gcode_block)
    if(i == nrow(gcode_parsed_filtered) || gcode_parsed_filtered$line[i] < gcode_parsed_filtered$line[i+1]){
      # print(i)
      # if(i==14) asasda
      current_values$line_id = single_gcode_block$line
      previous_values = simulated_data[nrow(simulated_data),]
      if(is.null(previous_values))
        previous_values = current_values
      current_values = update_timestamps(current_values, previous_values)
      
      current_values = calculate_vel_acc(current_values, previous_values)
      
      current_values = interpolate_values(current_values, previous_values, data_res, data_type)
      
      # print(current_values)
      simulated_data = rbind(simulated_data, current_values)
      current_values = simulated_data[nrow(simulated_data),, drop = F]
      row.names(current_values) = NULL
    }
  }
  simulated_data %>% select(-state_motion, -contains("_center"))
}

