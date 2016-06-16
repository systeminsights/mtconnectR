#' mtconnectR: A package to read analyze data in the 'MTConnect' standard 
#' 
#' You can use the package to read data from historical 'MTConnect logs' along
#' with the 'devices.xml' describing
#' the device. The data is organised into a 'MTConnectDevice' S4 data structure
#' and some convenience methods are also provided for basic read/view operations.
#' The package also includes some functions for analysis of MTConnect data. This includes
#' functions to simulate data (primarily postion data, feed rate and velocities) 
#' based on the G code and visualisation functions to compare the actual and simulated data.
#'
#' @section mtconnectR functions are divided into two categories:
#' \itemize{
#'   \item Functions to read XML and log data
#'   \item Functions read Gcode and simulate data and visualize actual and simulated data
#' of some analysis and visualization functions
#' }
#' @docType package
#' @name mtconnectR
NULL