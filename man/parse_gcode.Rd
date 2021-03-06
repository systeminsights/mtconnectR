% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/parse_gcode.R
\name{parse_gcode}
\alias{parse_gcode}
\title{Read the gcode and translate it as per the dictionary}
\usage{
parse_gcode(gcode_file_path)
}
\arguments{
\item{gcode_file_path}{Directory path of the file containing G-code}
}
\description{
Returns a data frame with each row referring to a block of G-code
}
\examples{
gcode_file_path = "extdata/raw_gcode_sample.NC"
gcode_parsed = parse_gcode(system.file(gcode_file_path,package = "mtconnectR"))
}
