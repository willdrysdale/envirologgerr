# **envirologgerr**

[![Build Status](https://travis-ci.org/skgrange/envirologgerr.svg?branch=master)](https://travis-ci.org/skgrange/envirologgerr)

**envirologgerr** is an R interface to the [Envirologger](http://www.airmonitors.co.uk/Air%20Monitors.net) API. **envirologgerr** interfaces with the default JSON interface and uses the [**jsonlite**](https://github.com/jeroenooms/jsonlite) package to parse the data format into friendly tibbles (tables).

To gain access to the API, credentials are needed in the form of a user and key (also called account and licence keys).

## Installation

To install the development version the [`devtools`](https://github.com/hadley/devtools) or the [`remotes`](https://github.com/r-lib/remotes) package will need to be installed first. Then:

```
# Install envirologgerr
remotes::install_github("skgrange/envirologgerr")
```

## Usage

**envirologgerr**'s primary function is `get_envirologger_data`. `get_envirologger_data` allows a user to easily query the API for stations' data between date periods. For example:

```
# Define user and key...

# Get some data for two made up stations
data_test <- get_envirologger_data(user, key, server = 1, station = c(1000, 1001), 
  start = "2016-05-01", end = "2016-05-15")
```

This will return all variables available for the two stations between the date periods. 

The main helper function is `get_envirologger_stations` and this returns a table containing integer keys/codes for the stations and servers which are available for a particular user.

**envirologgerr** and [**smonitor**](https://github.com/skgrange/smonitor) work well together.
