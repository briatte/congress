library(jsonlite) # read JSON data
library(yaml)     # read YAML data
library(dplyr)    # data manipulation
library(stringr)  # strings manipulation
library(network)  # network manipulation

dir.create("data"  , showWarnings = FALSE)
dir.create("plots" , showWarnings = FALSE)

source("01-data.r")  # data collection
source("02-build.r") # network construction

# have a nice day
