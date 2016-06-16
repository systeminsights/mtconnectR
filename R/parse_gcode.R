parse_single_line <- function(single_block, gcode_dict){
  #TODO: More clarity on what will be returned if the G-code line is not present in dictionary
  prefix = NULL
  line_type = single_block %>% str_extract("[[:alpha:]\\[#]*")
  gcode_dict_type = filter(gcode_dict, prefix == line_type)
  
  line_value = suppressWarnings(single_block %>% str_replace_all("[:alpha:]*", "") %>% as.numeric())
  
  if(nrow(gcode_dict_type) == 0 || str_detect(gcode_dict_type$code[1], "digit")) 
    return(data.frame(single_block, value = line_value, gcode_dict_type[1,])) # variable code line type
  
  i = 1
  while(i <= nrow(gcode_dict_type)){
    if(line_value == gcode_dict_type$code[i] %>% as.numeric()) break
    i = i + 1
  }
  
  if(i > nrow(gcode_dict_type)) 
    return(data.frame(single_block, value = line_value, gcode_dict[nrow(gcode_dict),])) # Currently not supported
  
  data.frame(single_block, value = line_value, gcode_dict_type[i,])
}

reflow_gcode_line <- function(single_line, gcode_dict){
  
  priority = NULL
  single_line_split = single_line %>% 
    str_replace_all("([[:alpha:]])", " \\1") %>% 
    str_split(" ") %>% extract2(1)
  single_line_split = single_line_split[single_line_split != ""]
  
  single_block = single_line_split[1]
  lines_context = ldply(single_line_split, parse_single_line, gcode_dict)
  
  lines_context %>% arrange(dplyr::desc(priority))
}

#' Read the gcode and translate it as per the dictionary
#' 
#' Returns a data frame with each row referring to a block of G-code
#' @param gcode_file_path Directory path of the file containing G-code
#' @examples
#' gcode_file_path = "extdata/raw_gcode_sample.NC"
#' gcode_parsed = parse_gcode(system.file(gcode_file_path,package = "mtconnectR"))
#' @export
parse_gcode <- function(gcode_file_path){
  notes = priority = code = NULL
  gcode_dict = utils::read.csv(system.file("gcode_dict.csv", package = "mtconnectR")) %>% select(-notes)
  gcode_lines = readLines(gcode_file_path, warn = F)
  gcode_dict = gcode_dict %>% arrange(dplyr::desc(priority)) 
    
  gcode_lines = gcode_lines %>% str_replace_all("\\(.*\\)", "") %>% str_trim()
  gcode_lines = gcode_lines[gcode_lines != ""]
  lines_context = ldply(seq_len(length(gcode_lines)),function(line){
    # print(i)
    data.frame(line, reflow_gcode_line(gcode_lines[line], gcode_dict))
  }) %>% select(-code)
  print(table(lines_context$type))
  # lines_context_known = lines_context %>% filter(type != "UNKNOWN")
  lines_context
}
