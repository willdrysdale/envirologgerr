# **envirologgerr**

[![Build Status](https://travis-ci.org/skgrange/envirologgerr.svg?branch=master)](https://travis-ci.org/skgrange/envirologgerr)

**envirologgerr** is an R interface to the [Envirologger](http://www.envirologger.com/home) [API](http://api.envirologger.net/2.0/documentation). **envirologgerr** interfaces with the JSON interface and uses the excellent [**jsonlite**](https://github.com/jeroenooms/jsonlite) package to parse the data format into friendly data frames (tables).

To gain access to the API, credentials are needed in the form of a user and key (also called account and licence keys respectively). 

## Installation

To install the development version the [`devtools`](https://github.com/hadley/devtools) package will need to be installed first. Then:

```
# Install envirologgerr
devtools::install_github("skgrange/envirologgerr")
```

## Usage

**envirologgerr**'s primary function is `get_envirologger_data`. `get_envirologger_data` allows a user to query the API for stations' data between date periods. The main helper function is `get_envirologger_stations`. `get_envirologger_stations` returns data, and critically, codes for the stations and servers which are available for a user-key combination. 

