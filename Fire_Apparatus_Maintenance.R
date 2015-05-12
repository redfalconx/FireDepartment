# Created by Andrew Russell, 2015.
# These packages are used at various points: 
# install.packages("RODBC", "data.table", "dplyr", "tidyr", "ggplot2", "scales")

# Load packages
library(RODBC)
library(data.table)
#library(reshape2)
library(dplyr) # data manipulation
library(tidyr) # a few pivot-table functions
library(ggplot2) # plotting  
library(scales) # works with ggplot2 to properly label axes on plots

# Connect to Access db
channel <- odbcConnectAccess2007("//fileshare1/Departments/Fire/SFD-Files/Apparatus and Equipment Repair Request/Company Officer Apparatus Maintenance.accdb")

# Check that connection is working (Optional)
# odbcGetInfo(channel)

# Find out what tables are available (Optional)
# Tables <- sqlTables(channel)

# Fetch the Maintenance Query from the database and put the results in a dataframe
Maintenance <- sqlFetch(channel, "All Companies Repair Query for FireStat")
Maintenance <- as.data.table(Maintenance)

# Closes the connection
close(channel)

# Format column names
setnames(Maintenance, names(Maintenance), gsub(" ", "_", names(Maintenance)))

# Add Time_to_Complete and Age_of_Vehicle columns
Maintenance$Time_to_Complete <- ifelse(!is.na(Maintenance$Date_Completed) & difftime(Maintenance$Date_Completed, Maintenance$Notified_Shop, units = "days") > 0, difftime(Maintenance$Date_Completed, Maintenance$Notified_Shop, units = "days"), 0)
Maintenance$Age_of_Vehicle <- year(Maintenance$Notified_Shop) - Maintenance$Year_of_Vehicle

# Get average time and count of completed repairs by month
Repairs_avg_count = Maintenance %>% 
  group_by(year = year(Date_Completed), month = month(Date_Completed)) %>% 
  summarise(Time_to_Complete = mean(Time_to_Complete), Count = n_distinct(ID)) %>%
  arrange(year, month)

# Take the count of the repair requests by month
Repairs_requested = Maintenance %>% 
  group_by(year = year(Notified_Shop), month = month(Notified_Shop)) %>% 
  summarise(Count = n_distinct(ID)) %>%
  arrange(year, month)

# Plot it!

Maintenance$Month <- as.Date(cut(Maintenance$Date, breaks = "month"))

# Plot of average time to complete repairs by the month the repair was completed
ggplot(Maintenance[as.Date(Date_Completed) >= ((Sys.Date()-395) - as.POSIXlt(Sys.Date())$mday + 1) & as.Date(Date_Completed) < (Sys.Date() - as.POSIXlt(Sys.Date())$mday + 1)], aes(Month, Time_to_Complete)) +
  stat_summary(fun.y = mean, geom = "line") + 
  scale_x_date(breaks = pretty_breaks(10))

# Plot of average time to complete repairs by the month the repair request was made
ggplot(Maintenance[as.Date(Notified_Shop) >= ((Sys.Date()-395) - as.POSIXlt(Sys.Date())$mday + 1) & as.Date(Notified_Shop) < (Sys.Date() - as.POSIXlt(Sys.Date())$mday + 1)], aes(Month, Time_to_Complete)) +
  stat_summary(fun.y = mean, geom = "line") + 
  scale_x_date(breaks = pretty_breaks(10))

