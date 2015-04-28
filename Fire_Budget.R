# Created by Andrew Russell, 2015.
# These packages are used at various points: 
# install.packages("RODBC", "data.table", "dplyr", "tidyr", "ggplot2", "scales")

# Load packages
library(RODBC)
library(data.table)
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

# Fetch the Overtime Query from the database and put the results in a dataframe
Overtime <- sqlFetch(channel, "Cost Of Overtime w /ranks Query for Total Daily Hours")
Overtime <- Overtime[complete.cases(Overtime),]
Overtime <- as.data.table(Overtime)

# Format column names
setnames(Overtime, names(Overtime), gsub(" ", "_", names(Overtime)))
setnames(Overtime, names(Overtime), gsub("#", "ID", names(Overtime)))

# Change Reason from numbers to strings

Overtime$Reason[Overtime$Reason == 1] <- "Sick"
Overtime$Reason[Overtime$Reason == 2] <- "Vacation"
Overtime$Reason[Overtime$Reason == 3] <- "Injury"
Overtime$Reason[Overtime$Reason == 4] <- "Personal Day"
Overtime$Reason[Overtime$Reason == 5] <- "Pilot"
Overtime$Reason[Overtime$Reason == 6] <- "Air Supply"
Overtime$Reason[Overtime$Reason == 7] <- "Union Bus"
Overtime$Reason[Overtime$Reason == 8] <- "Misc"
Overtime$Reason[Overtime$Reason == 9] <- "Funeral"
Overtime$Reason[Overtime$Reason == 10] <- "Storm Emerg"
Overtime$Reason[Overtime$Reason == 11] <- "Training"
Overtime$Reason[Overtime$Reason == 12] <- "Fireworks"
Overtime$Reason[Overtime$Reason == 13] <- "MDU Drill"
Overtime$Reason[Overtime$Reason == 14] <- "FIU Investigations"
Overtime$Reason[Overtime$Reason == 15] <- "Haz-Mat"
Overtime$Reason[Overtime$Reason == 16] <- "Emerg Leave"
Overtime$Reason[Overtime$Reason == 17] <- "Paternity"
Overtime$Reason[Overtime$Reason == 18] <- "Safe Grant"
Overtime$Reason[Overtime$Reason == 19] <- "Non-Grant Specific"
Overtime$Reason[Overtime$Reason == 20] <- "Light Duty"
Overtime$Reason[Overtime$Reason == 21] <- "Staff Meetings"

# Closes the connection
close(channel)

# Get sum totals by month by reason
Overtime_sum = Overtime %>% 
  group_by(year = year(Date), month = month(Date)) %>% 
  summarise(Cost = sum(Cost)) %>%
  arrange(year, month)

# Sum of all reasons
Overtime_sum_Reasons = Overtime %>% 
  group_by(year = year(Date), month = month(Date), Reason) %>% 
  summarise(Cost = sum(Cost)) %>%
  spread(Reason, Cost, fill = 0) %>%
  arrange(year, month)

# Plot it!
Overtime$Month <- as.Date(cut(Overtime$Date, breaks = "month"))

ggplot(Overtime[as.Date(Date) >= ((Sys.Date()-395) - as.POSIXlt(Sys.Date())$mday + 1) & as.Date(Date) < (Sys.Date() - as.POSIXlt(Sys.Date())$mday + 1)], aes(Month, Cost)) +
  stat_summary(fun.y = sum, geom = "line", aes(colour = Reason)) + 
  scale_x_date(breaks = pretty_breaks(10)) +
  scale_y_continuous(labels = dollar) # This turns it back into $ money for the plot

ggplot(Overtime[as.Date(Date) >= ((Sys.Date()-395) - as.POSIXlt(Sys.Date())$mday + 1) & as.Date(Date) < (Sys.Date() - as.POSIXlt(Sys.Date())$mday + 1)], aes(Month, Cost)) +
  stat_summary(fun.y = sum, geom = "bar", aes(colour = Reason)) + 
  scale_x_date(breaks = pretty_breaks(10)) + 
  scale_y_continuous(labels = dollar) # This turns it back into $ money for the plot

