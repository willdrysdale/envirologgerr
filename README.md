# **envirologgerr**

[![Build Status](https://travis-ci.org/skgrange/envirologgerr.svg?branch=master)](https://travis-ci.org/skgrange/envirologgerr)

**envirologgerr** is an R interface to the [Envirologger](http://www.envirologger.com/home) [API](http://api.envirologger.net/2.0/documentation). **envirologgerr** interfaces with the default JSON interface and uses the [**jsonlite**](https://github.com/jeroenooms/jsonlite) package to parse the data format into friendly data frames (tables).

To gain access to the API, credentials are needed in the form of a user and key (also called account and licence keys).

## Installation

To install the development version the [`devtools`](https://github.com/hadley/devtools) package will need to be installed first. Then:

```
# Install envirologgerr
devtools::install_github("skgrange/envirologgerr")
```

## Usage

**envirologgerr**'s primary function is `get_envirologger_data`. `get_envirologger_data` allows a user to easily query the API for stations' data between date periods. For example:

```
# Define user and key...

# Get some data for two made up stations
data_test <- get_envirologger_stations(user, key, station = c(1000, 1001), 
  server = 1, start = "2016-05-01", end = "2016-05-15")
```

This will return all variables available for the two stations between the date periods. 

The main helper function is `get_envirologger_stations` and returns a table containing integer keys/codes for the stations and servers which are available for a particular user.

**envirologgerr** and [**smonitor**](https://github.com/skgrange/smonitor) work well together.
