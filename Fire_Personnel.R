# Created by Andrew Russell, 2015.
# These packages are used at various points: 
# install.packages("RODBC", "data.table", "reshape2", "readxl", "dplyr", "tidyr", "ggplot2", "ggmap", "scales", "lubridate")

# Load packages
library(RODBC)
library(data.table)
library(reshape2)
library(readxl)
library(ggplot2)
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

# Fetch the Sick Table from the database and put the results in a dataframe
SickTable <- sqlFetch(channel, "Sick Table")

# Remove uneccesary columns
SickTable <- SickTable[, 1:8]

# Format column names & Time columns into H:M:S & Reason column
names(SickTable) <- gsub(" ", "_", names(SickTable))
names(SickTable) <- gsub("#", "ID", names(SickTable))
SickTable$Time_Out2 <- format(SickTable$Time_Out, format = "%H:%M:%S")
SickTable$Time_Return2 <- format(SickTable$Time_Return, format = "%H:%M:%S")
SickTable$Reason[agrep("INJRY", SickTable$Reason)] <- "INJURED"
SickTable$Reason[agrep("INJURY", SickTable$Reason)] <- "INJURED"
SickTable$Reason[agrep("SSSS", SickTable$Reason)] <- "SICK"

# Create Date + Time columns
SickTable$Date_Time_Out <- paste(SickTable$Date_Out, SickTable$Time_Out2)
SickTable$Date_Time_Out[agrep("NA", SickTable$Date_Time_Out)] <- NA
SickTable$Date_Time_Out <- as.POSIXct(SickTable$Date_Time_Out)
SickTable$Date_Time_Return <- paste(SickTable$Date_Return, SickTable$Time_Return2)
SickTable$Date_Time_Return[agrep("NA", SickTable$Date_Time_Return)] <- NA
SickTable$Date_Time_Return <- as.POSIXct(SickTable$Date_Time_Return)

# Format time to remove mislabeled dates and include only H
SickTable$Time_Out <- format(SickTable$Time_Out, format = "%H")
SickTable$Time_Return <- format(SickTable$Time_Return, format = "%H")

# Convert times to numeric
SickTable$Time_Out <- as.numeric(SickTable$Time_Out)
SickTable$Time_Return <- as.numeric(SickTable$Time_Return)

# Create Shift column with NA values, then change to Day and Night shifts
SickTable$Shift <- NA
SickTable$Shift <- ifelse(SickTable$Time_Out < 8, "Night",
                      ifelse(SickTable$Time_Out >= 16, "Night",
                        "Day"  ))

# Remove NAs from Total_Tours_Missed, Date_Out and create new Dataframe
SickTable <- SickTable[complete.cases(SickTable$Total_Tours_Missed),]
SickTable <- SickTable[complete.cases(SickTable$Date_Out),]

# Convert to data table
SickTable <- as.data.table(SickTable)

# Subset the 0s in Total_Tours_Missed, otherwise they will be lost
dt2 <- subset(SickTable, Total_Tours_Missed == 0)

# All of the relevant dates
dates.all = SickTable[, seq(Date_Out, length = Total_Tours_Missed/2, by = "4 days"), by = Record_ID]

# Set the key and merge filling in the blanks with previous observation
setkey(SickTable, Record_ID, Date_Out)
SickTable <- SickTable[dates.all, roll = T]

# Combine the two subsets, then order it by Record_ID
SickTable <- rbind(SickTable, dt2)
SickTable <- setorder(SickTable, Employee_ID, Date_Out, Total_Tours_Missed)

# Duplicate data to include remaining shifts, 
# except for Total_Tours_Missed == 0 and the first shift for odd Total_Tours_Missed
SickTable[, Shifts := if(Total_Tours_Missed[1] %% 2 == 0) T else c(F, rep(T, .N-1))
   , by = .(Employee_ID, Shift)]
dt2 <- rbind(SickTable, SickTable[Shifts & Total_Tours_Missed != 0][, Shift := ifelse(Shift == 'Day', 'Night', 'Day')])
SickTable <- dt2

# Add the weekday and the number of shifts missed
SickTable$Day <- weekdays(SickTable$Date_Out)
SickTable$Shifts <- ifelse(SickTable$Total_Tours_Missed == 0, 0, 1)

# Subset to Sick and Injured and exclude unnecessary columns.
# Change to short- and long-term reasons
Sick <- subset(SickTable, Reason == "SICK", select = c(1,2,3,4,8,13,14,15))
Sick$Reason <- ifelse(Sick$Total_Tours_Missed < 5, "Short-term Sick", "Long-term Sick")
Injured <- subset(SickTable, Reason == "INJURED", select = c(1,2,3,4,8,13,14,15))
Injured$Reason <- ifelse(Injured$Total_Tours_Missed < 5, "Short-term Injured", "Long-term Injured")

# Combine again
Personnel <- rbind(Sick, Injured)

# Find the sum of shifts missed by month
Sick_sum = Sick %>% 
  group_by(year = year(Date_Out), month = month(Date_Out), Reason) %>% 
  summarise(Total_Shifts = sum(Shifts)) %>%
  spread(Reason, Total_Shifts, fill = 0) %>%
  arrange(year, month)

Injured_sum = Injured %>% 
  group_by(year = year(Date_Out), month = month(Date_Out), Reason) %>% 
  summarise(Total_Shifts = sum(Shifts)) %>%
  spread(Reason, Total_Shifts, fill = 0) %>%
  arrange(year, month)

# Take the count of the employees by short- and long-term by month
Sick_count = Sick %>% 
  group_by(year = year(Date_Out), month = month(Date_Out), Reason) %>% 
  summarise(Count = n_distinct(Employee_ID)) %>%
  spread(Reason, Count, fill = 0) %>%
  arrange(year, month)

Injured_count = Injured %>% 
  group_by(year = year(Date_Out), month = month(Date_Out), Reason) %>% 
  summarise(Count = n_distinct(Employee_ID)) %>%
  spread(Reason, Count, fill = 0) %>%
  arrange(year, month)

# Take the count for specific dates. Replace the dates below.
Sick_Short_count_yr = aggregate(Employee_ID ~ Reason, data=Sick[Reason == "Short-term Sick" & Date_Out >= as.POSIXct("2014-04-01") & Date_Out < as.POSIXct("2015-04-01")], FUN=function(x) length(unique(x)))
Sick_Long_count_yr = aggregate(Employee_ID ~ Reason, data=Sick[Reason == "Long-term Sick" & Date_Out >= as.POSIXct("2014-04-01") & Date_Out < as.POSIXct("2015-04-01")], FUN=function(x) length(unique(x)))
Injured_Short_count_yr = aggregate(Employee_ID ~ Reason, data=Injured[Reason == "Short-term Injured" & Date_Out >= as.POSIXct("2014-04-01") & Date_Out < as.POSIXct("2015-04-01")], FUN=function(x) length(unique(x)))
Injured_Long_count_yr = aggregate(Employee_ID ~ Reason, data=Injured[Reason == "Long-term Injured" & Date_Out >= as.POSIXct("2014-04-01") & Date_Out < as.POSIXct("2015-04-01")], FUN=function(x) length(unique(x)))


### Fetch the Vacation Tours from the database and put the results in a dataframe
VacationTable <- sqlFetch(channel, "Vacation Tours")
VacationTable <- as.data.table(VacationTable)

# Format column names & Shift column
names(VacationTable) <- gsub(" ", "_", names(VacationTable))
names(VacationTable) <- gsub("#", "ID", names(VacationTable))
setnames(VacationTable, "ID", "Record_ID")
setnames(VacationTable, "Tour_Taken", "Date_Out")
VacationTable$Shift[agrep(" DAY", VacationTable$Shift)] <- "DAY"
VacationTable$Shift[agrep("1/2 DAY", VacationTable$Shift)] <- "DAY"
VacationTable$Shift[agrep("B=NIGHT", VacationTable$Shift)] <- "NIGHT"
VacationTable$Shift <- gsub("DAY", "Day", VacationTable$Shift)
VacationTable$Shift <- gsub("NIGHT", "Night", VacationTable$Shift)

# Add Reason, Total_Tours_Missed, Shifts, Day columns
VacationTable$Reason <- "Vacation Day"
VacationTable$Total_Tours_Missed <- 1
VacationTable$Shifts <- 1
VacationTable$Day <- weekdays(VacationTable$Date_Out)

# Reorder, then combine with Personnel table
setcolorder(VacationTable, c("Record_ID","Employee_ID","Reason","Date_Out","Total_Tours_Missed","Shift","Shifts","Day"))
Personnel <- rbind(Personnel, VacationTable)

# Find the sum of shifts missed by month
Vacation_sum = VacationTable %>% 
  group_by(year = year(Date_Out), month = month(Date_Out), Reason) %>% 
  summarise(Total_Shifts = sum(Shifts)) %>%
  spread(Reason, Total_Shifts, fill = 0) %>%
  arrange(year, month)

# Take the count of the employees
Vacation_count = VacationTable %>% 
  group_by(year = year(Date_Out), month = month(Date_Out), Reason) %>% 
  summarise(Count = n_distinct(Employee_ID)) %>%
  spread(Reason, Count, fill = 0) %>%
  arrange(year, month)


### Fetch the Personal Days table from the database and put the results in a dataframe
PersonalDaysTable <- sqlFetch(channel, "Personal Days")
PersonalDaysTable <- as.data.table(PersonalDaysTable)

# Format column names
names(PersonalDaysTable) <- gsub(" ", "_", names(PersonalDaysTable))
names(PersonalDaysTable) <- gsub("#", "ID", names(PersonalDaysTable))
setnames(PersonalDaysTable, "ID", "Record_ID")
setnames(PersonalDaysTable, "Tour_Taken", "Date_Out")

# Format Shift column
PersonalDaysTable$Shift <- gsub("DAY", "Day", PersonalDaysTable$Shift)
PersonalDaysTable$Shift <- gsub("NIGHT", "Night", PersonalDaysTable$Shift)

# Add Reason, Total_Tours_Missed, Shifts, Day columns
PersonalDaysTable$Reason <- "Personal Day"
PersonalDaysTable$Total_Tours_Missed <- ifelse(PersonalDaysTable$Shift == "1/2 Day", 0.5,
                                               ifelse(PersonalDaysTable$Shift == "1/2 Night", 0.5,
                                                      1))
PersonalDaysTable$Shifts <- ifelse(PersonalDaysTable$Shift == "1/2 Day", 0.5,
                                   ifelse(PersonalDaysTable$Shift == "1/2 Night", 0.5,
                                          1))
PersonalDaysTable$Day <- weekdays(PersonalDaysTable$Date_Out)

# Find the sum of shifts missed by month
Personal_sum = dcast(PersonalDaysTable, year(Date_Out) + month(Date_Out) ~ Reason, value.var="Shifts", fun.aggregate=sum)

Personal_sum = PersonalDaysTable %>% 
  group_by(year = year(Date_Out), month = month(Date_Out), Reason) %>% 
  summarise(Total_Shifts = sum(Shifts)) %>%
  spread(Reason, Total_Shifts, fill = 0) %>%
  arrange(year, month)

# Take the count of the employees
Personal_count = aggregate(Employee_ID ~ month(Date_Out) + year(Date_Out), data=PersonalDaysTable, FUN=function(x) length(unique(x)))

Personal_count = PersonalDaysTable %>% 
  group_by(year = year(Date_Out), month = month(Date_Out), Reason) %>% 
  summarise(Count = n_distinct(Employee_ID)) %>%
  spread(Reason, Count, fill = 0) %>%
  arrange(year, month)

# Add to Personnel
Personnel <- rbind(Personnel, PersonalDaysTable)
# Format Date_Out column
Personnel$Date_Out <- as.Date(Personnel$Date_Out, format = "%Y-%m-%d")

# Closes the connection
close(channel)


# Fetch the Vacation Weeks table from the Excel spreadsheet and put the results in a dataframe
VacationWeeks <- read_excel("//fileshare1/Departments/Fire/FireStat/FireStat Workbook - Personnel and Budget.xlsx", "Vacation Weeks")

# Include only necessary columns
VacationWeeks <- VacationWeeks[, c(1,3,4)]

# Make data.table
VacationWeeks <- unclass(VacationWeeks)
VacationWeeks <- as.data.table(VacationWeeks)

# Format column names
setnames(VacationWeeks, "Employee #", "Employee_ID")
setnames(VacationWeeks, "Tour Taken", "Date_Out")

# Add necessary columns
VacationWeeks[, Record_ID := integer(.N)]
VacationWeeks$Total_Tours_Missed <- 1
VacationWeeks$Reason <- "Vacation Week"
VacationWeeks$Shifts <- 1
VacationWeeks$Day <- weekdays(VacationWeeks$Date_Out)

# Set column order
setcolorder(VacationWeeks, c("Record_ID", "Employee_ID", "Reason", "Date_Out", "Total_Tours_Missed", "Shift", "Shifts", "Day"))

# Add to Personnel
Personnel <- rbind(Personnel, VacationWeeks)

# Save the file
write.csv(Personnel, file = "Personnel.csv")

# Find the sum of shifts missed by month
VacationWeeks_sum = VacationWeeks %>% 
  group_by(year = year(Date_Out), month = month(Date_Out), Reason) %>% 
  summarise(Total_Shifts = sum(Shifts)) %>%
  spread(Reason, Total_Shifts, fill = 0) %>%
  arrange(year, month)

# Take the count of the employees
VacationWeeks_count = VacationWeeks %>% 
  group_by(year = year(Date_Out), month = month(Date_Out), Reason) %>% 
  summarise(Count = n_distinct(Employee_ID)) %>%
  spread(Reason, Count, fill = 0) %>%
  arrange(year, month)



# Calculate potential leave enhancement (PLE)
# (Taking a short-term sick day before or after another kind of leave)
# Change to PLE and set order by Employee_ID then Date_Out

PLE <- Personnel[complete.cases(Personnel),]
setorder(PLE, Employee_ID, Date_Out)

# Set up PLE column, then use a for loop and if statements to calculate
PLE$PLE <- 0
for (i in 2:nrow(PLE)) {
  if(PLE$Employee_ID[i] == PLE$Employee_ID[i+1] & PLE$Reason[i] != PLE$Reason[i+1] & PLE$Date_Out[i+1] - PLE$Date_Out[i] <= 4 & PLE$Reason[i] == "Short-term Sick" & PLE$Reason[i+1] != "Long-term Sick") PLE$PLE[i] = 1 else 
      if(PLE$Employee_ID[i] == PLE$Employee_ID[i-1] & PLE$Reason[i] != PLE$Reason[i-1] & PLE$Date_Out[i] - PLE$Date_Out[i-1] <= 4 & PLE$Reason[i] == "Short-term Sick" & PLE$Reason[i-1] != "Long-term Sick") PLE$PLE[i] = 1 else
          0 }

# Find the total sum
sum(PLE$PLE)

# Save the file
write.csv(PLE, file = "Personnel.csv")

# Find the sum of PLE by month
PLE_sum = dcast(PLE, year(Date_Out) + month(Date_Out) ~ PLE, value.var="PLE", fun.aggregate=sum)

# Find the sum of all Reasons
Personnel_sum = dcast(Personnel, year(Date_Out) + month(Date_Out) ~ Reason, value.var="Shifts", fun.aggregate=sum)
names(Personnel_sum) <- gsub(" ", "_", names(Personnel_sum))
names(Personnel_sum) <- gsub("-", "_", names(Personnel_sum))



# Graph it!
Personnel$Month <- as.Date(cut(Personnel$Date_Out,
                         breaks = "month"))

ggplot(Personnel[Date_Out > as.Date("2012-01-01") & Date_Out < as.Date("2016-01-01")], aes(Month, Shifts)) + 
  stat_summary(fun.y = sum, geom = "line", aes(colour = Reason)) 
  
