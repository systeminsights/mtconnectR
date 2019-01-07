

[![Build Status](https://travis-ci.org/systeminsights/mtconnectR.svg?branch=master)](https://travis-ci.org/systeminsights/mtconnectR)
[![codecov.io](https://codecov.io/github/systeminsights/mtconnectR/coverage.svg?branch=master)](https://codecov.io/github/systeminsights/mtconnectR?branch=master)
[![AppVeyor Build Status](https://ci.appveyor.com/api/projects/status/github/systeminsights/mtconnectR?branch=master&svg=true)](https://ci.appveyor.com/project/systeminsights/mtconnectR)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/mtconnectR)](https://cran.r-project.org/package=mtconnectR)
[![CRAN RStudio mirror downloads](http://cranlogs.r-pkg.org/badges/mtconnectR)](http://www.r-pkg.org/pkg/mtconnectR)

# Credits

This research was supported by the National Institute of Standards and Technology through the cooperative research agreement 70NANB15H090, “Standards Based Infrastructure for Enabling Smart Manufacturing Research and Development Test Bed.”

# Installation

Just follow the regular installation procedure.

```{r}
# Install devtools if not installed already

install.packages("mtconnectR", repos ="http://ftp.iitm.ac.in/cran/")

```

# Introduction

The MTConnectR package provides a way to read data from MTConnect and G Code to the statistical software R (www.r-project.org).  You can use the package to read data from historical MTConnect logs along with the devices.xml describing the device. The data is organised into a 'MTConnectDevice' S4 data structure and some convenience methods are also provided for basic read/view operations. Also, the package further provides the user a way to read and parse G Code data into R, for simulating data in the MTConnect format, and to map the simulated data with the actual MTConnect data.

# The mtconnectR package

You can use a ruby parser on the MTConnect agent data dump to get data data from a CNC machine as a log file. This file is a continuous stream of data with data for different aspects of the machine (execution, path feed rate, controller mode, conditions, etc) combined into a coninuous object. One issue faced by the those who want to analyse machine data is the fact that raw MTConnect data, although ideal for stream data processing and canonical storage, doesn't lend itself to aggregation and quick analysis. 

In addition, we don't have any context towards what data corresponds to which data item in the Device. This data is stored in another file called the Devices XML. However the metadata from the Devices XML and the raw data are disjoint unless passed through an MTConnect agent. 

We try to solve this issue with this package. It can read in a data from the device and parse the Devices XML and store the data in a form more appropriate for analysis with data seperated into different entities called **data items**. 

`mtconnectR` package also provides functions with further analysis capabilities. 

The user can also read and parse G Code data into R using this pacjage. The user can also use this parsed G Code data for simulating data in the MTConnect format. In addition, the user has functions which can be used to map the simulated data with the actal MTConnect data.


# MTC Classes

We've added a few S4 classes to aid in the correct structuring of MTC data. They are:

* MTCCycle (virtual)
* MTCDevice
* MTCDataItem

## MTCCycle

This is a virtual class that can be used to represent any type of time series object. The only necessary
slots are:

* data_item_list - Representing the list of data items
* device_uuid - UUID of the device associated with the data

Currently, this is inherited only by the MTCDevice Class

## MTCDevice

This is the class in which all the main details are stored. The slots(in addition to the ones for the MTCCycle Class ) are:

* rawdata Original Delimited data log (parsed from which the data was created)
* metadata Metadata (if any) for the device

## MTCDataItem

This class represents the data from a single data item from MTConnect. The slots for these class can be accessed
directly by using the `@` accessor function to access the slots in the class but it is adviced that the methods over the MTCDevice Class be used directly. The slots for MTCDataItem class are

* data Data for a single data item at a data.frame in timestamp, value format
* data_type Type of Data - can be even or sample
* path XML Xpath
* data_source source from which the data item was created
* xmlID id of the data item in the devices XML


# Functions

## Create MTCDevice Class

`create_mtc_device` is a wrapper over multiple functions, and can accept different types of data input including:

* Delimited Log Data from MTConnect Agent (DMTCD) and a Devices.XML file.
* URI of a running MTConnect agent and a sampling duration to parse streaming data. (Not Implemented)

The output of the `create_mtc_device` function is a MTCDevice S4 Class that has all the data organized into easily accessible forms. In additon, the following convenience functions are also provided to facilitate exploratory analysis:

* getData
* fix (to view data in RStudio)
* summary
* merge

Within the MTCDevice Class, each data

For more info on individual functions, read the vignette on `create_mtc_device`

`simulate_data_from_gcode` - This function is used to simulate MTConnect-style data 
from gcode data. The inputs are parsed gcode values from the parse funciton. Optionally,
start time can be provided to make the first data point start from that point (or numeric value),
and a data resolution can be provided if you need to interpolate data simulating the 
stateful representation of sample values.

## Parse G Code Data

`parse_gcode` function can be used to read G Code data into R. The G Codes currently 
supported can be got fromt he `gcode_dict.csv` file as follows:

```{r}
gcode_dict = utils::read.csv(system.file("gcode_dict.csv", package = "mtconnectR")) %>% select(-notes)
```

Currently, 47 different G Codes are in the dictionary.

## Simulate G Code data

`simulate_data_from_gcode` function can be used to simulate MTConnect data from G Code. Data from G Code including timestamp and values in MTConnect style (stateful, tesselated interpolation for samples). Currently, the follwing data items are implemented:

- Program
- Rapid
- Tool Change
- Upcoming Tool
- Rotary Velocity/Spindle Speed
- Path Feedrate
- Path Position
- Motion Linear
- Arc Motion

## Map G Code data

Given data for one instance of the part and the G Code, the `map_gcode_mtc` function maps the G code and raw data using Simulated G code values. Currently the mapping is done using 

- Spindle Speed
- Path Position Values

We can expand using Event (program/Tool etc) mapping, but this is not implemented yet
    
## Plotting Mapped data 

The `plot_twoway` function can be used to plot the Simulated and the Actual values against each other once the mapping has been completed.

# Scripts

We have also bundled some scripts to do common tasks that can be done with the MTConnectR
package. For more information on how to use them, read the README in the scripts folder.

# TODO

For DMTCD Data deal with:

* Assets
* Commands
* Tell the user if anydata is remaining

# LICENSE

This package is licensed under AGPL-3 (http://www.r-project.org/Licenses/AGPL-3) and comes with no warranty.

YEAR: 2016

COPYRIGHT HOLDER: System Insights Inc.

