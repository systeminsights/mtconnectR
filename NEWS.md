# mtconnectR 1.2.0
- Numerous bugfixes, exposed a method to get meta data from an MTC data object

# mtconnectR 1.1.0
- Numerous bugfixes

# mtconnectR 1.0.1
- Minor changes to accommodate dplyr 0.5.0 update

# mtconnectR 1.0.0
- This is a new release
- A number of functions for analysis of delimited MTC data has been added
- These include functions to parse G Code, simulate device data from it and map it to actual MTC data
- `parse_gcode` to read Raw G Code and add context to it
- `simulate_data_from_gcode` to simulate device data (position,velocities,etc) from G Code
- `map_gcode_mtc` to map the simulated timestamps to timestamps of actual MTC data
- `plot_twoway` to visualise the mapping between simulated and actual timestamps

# mtconnectR 0.2.0

- Added a scripts folder to hold conveyance scripts for common uses.
- added `get_data.R` to get data in CSV format

# mtconnectR 0.1.7

- `ts_to_mtc_device` to convert merged time series data back to mtc_device object
- `clean_reduntant_rows` to clean reduntant rows

# mtconnectR 0.1.6

- Can now handle conditions!

# mtconnectR 0.1.5

- Can now handle pathpositions!

# mtconnectR 0.1.4

- Changed some taxonomy

# mtconnectR 0.1.3

- `convert_ts_to_interval` to convert time series to interval
- `convert_interval_to_ts` to convert interval to time series

# mtconnectR 0.1.2 

- `add_data_item_to_mtc_device` to add new data items
- Bug fixes in merge function

# mtconnectR 0.1.1

- Added a `NEWS.md` file to track changes to the package.
- `extract_param_from_path` to extract param from xpaths


# mtconnectR 0.1.0

- First Release
- `get_device_info_from_xml` to get all the device info from xml
- `get_xpaths_from_xml` to get all the xpath info from xml for a device
- `read_adapter_log_file` to read the adapter log files
- `create_mtc_device_from_adapter_data` to read data into a MTCDevice Class


