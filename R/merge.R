
#' Merges all the data.frames in the list into single data.frame
#' 
#' @param dataList is a list of data.frames. Each data.frame should be of type timestamp|value1|value2...
#' @param outputDF if TRUE, then returns output in the form of data.frame instead of data.table
mergeTS <- function(dataList, outputDF = T, useListNames = F, missTimes = integer())
{
  a = NULL
  
  if (length(dataList) == 0) 
  {
    flog.info(paste("You gave me a list with zero elements!"))
    return(NULL)
  }
  
  for (i in setdiff(1:length(dataList), missTimes))
    a = append(a, dataList[[i]]$timestamp)
  
  seed = data.table(timestamp = sort(unique(a), method = "quick"),key = "timestamp")
  
  #   i = 1
  
  for (i in length(dataList):1)
  {
    data = dataList[[i]]
    data = data.table(data, key = "timestamp")
    
    seed = data[seed, mult = "first", roll = T]
    
    setnames(seed ,c("timestamp", paste0("V", 2:ncol(seed)))) 
    
  }
  if (!useListNames)  valNames = unlist(sapply(dataList, names)) else
    valNames = unlist(names(dataList))
  
  valNames = valNames[valNames!="timestamp"]
  setnames(seed, c("timestamp", valNames))
  data_output = seed
  if (outputDF == T) data_output = data.frame(data_output)
  (data_output)
}