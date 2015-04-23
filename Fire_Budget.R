# Created by Andrew Russell, 2015.
# These packages are used at various points: 
# install.packages("RODBC", "data.table", "reshape2", "readxl", "dplyr", "tidyr", "ggplot2", "ggmap", "scales", "lubridate")

# Load packages
library(RODBC)
library(data.table)
#library(reshape2)
library(readxl)
library(dplyr) # data manipulation
library(tidyr) # a few pivot-table functions
library(ggplot2) # plotting  
library(scales) # works with ggplot2 to properly label axes on plots

# Connect to Access db
channel <- odbcConnectAccess("//fileshare1/Departments/Fire/OFFICE/SFDRECORDS/SFD 2012")

# Check that connection is working (Optional)
# odbcGetInfo(channel)

# Find out what tables are available (Optional)
# Tables <- sqlTables(channel)