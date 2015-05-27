# Created by Andrew Russell, 2015.
# These packages are used at various points: 
# install.packages("RODBC", "data.table", "readxl", "dplyr", "tidyr", "ggplot2", "scales", "weatherData")

#### Load packages ####
library(RODBC)
library(data.table)
library(readxl)
library(dplyr) # data manipulation
library(tidyr) # a few pivot-table functions
library(ggplot2) # plotting  
library(scales) # works with ggplot2 to properly label axes on plots
library(weatherData) # gets weather data

# Connect to Access db
channel <- odbcConnectAccess("//fileshare1/Departments/Fire/OFFICE/SFDRECORDS/SFD 2012")

# Check that connection is working (Optional)
# odbcGetInfo(channel)

# Find out what tables are available (Optional)
# Tables <- sqlTables(channel)

#### Fetch the Sick Table from the database and put the results in a dataframe ####
SickTable <- sqlFetch(channel, "Sick Table")

# Remove uneccesary columns
SickTable <- SickTable[, 1:8]

# Format column names & Time columns into H:M:S & Reason column
setnames(SickTable, names(SickTable), gsub(" ", "_", names(SickTable)))
setnames(SickTable, names(SickTable), gsub("#", "ID", names(SickTable)))
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

# Take the count for the last three 12-month periods
# Current 12 months
Sick_count_yr1 = Sick %>% 
  filter(as.Date(Date_Out) >= ((Sys.Date()-365) - as.POSIXlt(Sys.Date())$mday + 1) & as.Date(Date_Out) < (Sys.Date() - as.POSIXlt(Sys.Date())$mday + 1)) %>% 
  group_by(Reason) %>% 
  summarise(Count = n_distinct(Employee_ID))
# Prior 12 months
Sick_count_yr2 = Sick %>% 
  filter(as.Date(Date_Out) >= ((Sys.Date()-730) - as.POSIXlt(Sys.Date())$mday + 1) & as.Date(Date_Out) < (Sys.Date()-365 - as.POSIXlt(Sys.Date())$mday + 1)) %>% 
  group_by(Reason) %>% 
  summarise(Count = n_distinct(Employee_ID))
# Prior prior 12 months
Sick_count_yr3 = Sick %>% 
  filter(as.Date(Date_Out) >= ((Sys.Date()-1095) - as.POSIXlt(Sys.Date())$mday + 1) & as.Date(Date_Out) < (Sys.Date()-730 - as.POSIXlt(Sys.Date())$mday + 1)) %>% 
  group_by(Reason) %>% 
  summarise(Count = n_distinct(Employee_ID))

# Current 12 months
Injured_count_yr1 = Injured %>% 
  filter(as.Date(Date_Out) >= ((Sys.Date()-365) - as.POSIXlt(Sys.Date())$mday + 1) & as.Date(Date_Out) < (Sys.Date() - as.POSIXlt(Sys.Date())$mday + 1)) %>% 
  group_by(Reason) %>% 
  summarise(Count = n_distinct(Employee_ID))
# Prior 12 months
Injured_count_yr2 = Injured %>% 
  filter(as.Date(Date_Out) >= ((Sys.Date()-730) - as.POSIXlt(Sys.Date())$mday + 1) & as.Date(Date_Out) < (Sys.Date()-365 - as.POSIXlt(Sys.Date())$mday + 1)) %>% 
  group_by(Reason) %>% 
  summarise(Count = n_distinct(Employee_ID))
# Prior prior 12 months
Injured_count_yr3 = Injured %>% 
  filter(as.Date(Date_Out) >= ((Sys.Date()-1095) - as.POSIXlt(Sys.Date())$mday + 1) & as.Date(Date_Out) < (Sys.Date()-730 - as.POSIXlt(Sys.Date())$mday + 1)) %>% 
  group_by(Reason) %>% 
  summarise(Count = n_distinct(Employee_ID))


#### Fetch the Vacation Tours from the database and put the results in a dataframe ####
VacationTable <- sqlFetch(channel, "Vacation Tours")
VacationTable <- as.data.table(VacationTable)

# Format column names & Shift column
setnames(VacationTable, names(VacationTable), gsub(" ", "_", names(VacationTable)))
setnames(VacationTable, names(VacationTable), gsub("#", "ID", names(VacationTable)))
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


#### Fetch the Personal Days table from the database and put the results in a dataframe ####
PersonalDaysTable <- sqlFetch(channel, "Personal Days")
PersonalDaysTable <- as.data.table(PersonalDaysTable)

# Format column names
setnames(PersonalDaysTable, names(PersonalDaysTable), gsub(" ", "_", names(PersonalDaysTable)))
setnames(PersonalDaysTable, names(PersonalDaysTable), gsub("#", "ID", names(PersonalDaysTable)))
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
Personal_sum = PersonalDaysTable %>% 
  group_by(year = year(Date_Out), month = month(Date_Out), Reason) %>% 
  summarise(Total_Shifts = sum(Shifts)) %>%
  spread(Reason, Total_Shifts, fill = 0) %>%
  arrange(year, month)

# Take the count of the employees
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


#### Fetch the Vacation Weeks table from the Excel spreadsheet and put the results in a dataframe ####
VacationWeeks <- read_excel("P:/Somerstat Data/Fire/Ongoing Analyses/FireStat Workbook - Personnel and Budget.xlsx", "Vacation Weeks")

# Include only necessary columns
VacationWeeks <- VacationWeeks[, c(1,3,4)]

# Make data.table
VacationWeeks <- unclass(VacationWeeks)
VacationWeeks <- as.data.table(VacationWeeks)

# Format column names
setnames(VacationWeeks, "Employee #", "Employee_ID")
setnames(VacationWeeks, "Tour Taken", "Date_Out")

# Format to Date class
VacationWeeks$Date_Out <- as.Date(VacationWeeks$Date_Out, format = "%Y-%m-%d")

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


#### Calculate potential leave enhancement (PLE) ####
# (Taking a short-term sick day before or after another kind of leave)
# Change to PLE and set order by Employee_ID then Date_Out

PLE <- Personnel[complete.cases(Personnel),]
setorder(PLE, Employee_ID, Date_Out)

# Set up PLE column, then vectorize and fill in PLE$PLE. Basically, it acts like if statements to calculate
PLE$PLE <- 0
n <- nrow(PLE)
PLE$PLE[2:(n-1)] <- as.numeric((PLE$Employee_ID[2:(n-1)] == PLE$Employee_ID[3:n] & PLE$Reason[2:(n-1)] != PLE$Reason[3:n] & PLE$Date_Out[3:n] - PLE$Date_Out[2:(n-1)] <= 4 & PLE$Reason[2:(n-1)] == "Short-term Sick" & PLE$Reason[3:n] != "Long-term Sick") |
                               (PLE$Employee_ID[2:(n-1)] == PLE$Employee_ID[1:(n-2)] & PLE$Reason[2:(n-1)] != PLE$Reason[1:(n-2)] & PLE$Date_Out[2:(n-1)] - PLE$Date_Out[1:(n-2)] <= 4 & PLE$Reason[2:(n-1)] == "Short-term Sick" & PLE$Reason[1:(n-2)] != "Long-term Sick"))

## Old for loop that still works but is very, very slow
# for (i in 2:nrow(PLE)) {
#  if(PLE$Employee_ID[i] == PLE$Employee_ID[i+1] & PLE$Reason[i] != PLE$Reason[i+1] & PLE$Date_Out[i+1] - PLE$Date_Out[i] <= 4 & PLE$Reason[i] == "Short-term Sick" & PLE$Reason[i+1] != "Long-term Sick") PLE$PLE[i] = 1 else 
#    if(PLE$Employee_ID[i] == PLE$Employee_ID[i-1] & PLE$Reason[i] != PLE$Reason[i-1] & PLE$Date_Out[i] - PLE$Date_Out[i-1] <= 4 & PLE$Reason[i] == "Short-term Sick" & PLE$Reason[i-1] != "Long-term Sick") PLE$PLE[i] = 1 else
#      0 }

# Find the total sum
sum(PLE$PLE)

# Save the file
write.csv(PLE, file = "Personnel.csv")

# Find the sum of PLE by month
PLE_sum = PLE %>% 
  group_by(year = year(Date_Out), month = month(Date_Out)) %>% 
  summarise(Total_PLE = sum(PLE)) %>%
  arrange(year, month)

# Find the sum of PLE by month by Employee_ID
PLE_sum_Employee = PLE %>% 
  filter(PLE > 0) %>%
  group_by(year = year(Date_Out), month = month(Date_Out), Employee_ID) %>% 
  summarise(Total_PLE = sum(PLE)) %>%
  spread(Employee_ID, Total_PLE, fill = 0) %>%
  arrange(year, month)

PLE_sum_Employee_totals <- data.frame(colSums(PLE_sum_Employee))

# Find the sum of all Reasons
Personnel_sum = Personnel %>% 
  group_by(year = year(Date_Out), month = month(Date_Out), Reason) %>% 
  summarise(Total_Shifts = sum(Shifts)) %>%
  spread(Reason, Total_Shifts, fill = 0) %>%
  arrange(year, month)
setnames(Personnel_sum, names(Personnel_sum), gsub(" |-", "_", names(Personnel_sum)))


#### Plot it! ####
#lime_green = "#2ecc71"
#soft_blue = "#3498db"
#pinkish_red = "#e74c3c"
#purple = "#9b59b6"
#teele = "#1abc9c"
#nice_blue = "#2980b9"

#my.theme <- 
#  theme(#plot.background = element_rect(fill="white"), # Remove background
#    panel.grid.major = element_blank(), # Remove gridlines
#    # panel.grid.minor = element_blank(), # Remove more gridlines
#    # panel.border = element_blank(), # Remove border
#    # panel.background = element_blank(), # Remove more background
#    axis.ticks = element_blank(), # Remove axis ticks
#    axis.text=element_text(size=6), # Enlarge axis text font
#    axis.title=element_text(size=8), # Enlarge axis title font
#    plot.title=element_text(size=12) # Enlarge, left-align title
#    ,axis.text.x = element_text(angle=60, hjust = 1) # Uncomment if X-axis unreadable 
#  )



Personnel$Month <- as.Date(cut(Personnel$Date_Out, breaks = "month"))
PLE$Month <- as.Date(cut(PLE$Date_Out, breaks = "month"))

ggplot(Personnel[Date_Out >= ((Sys.Date()-395) - as.POSIXlt(Sys.Date())$mday + 1) & Date_Out < (Sys.Date() - as.POSIXlt(Sys.Date())$mday + 1)], aes(Month, Shifts)) +
  stat_summary(fun.y = sum, geom = "line", aes(colour = Reason)) + scale_x_date(breaks = pretty_breaks(10)) 

ggplot(Personnel[Date_Out >= ((Sys.Date()-395) - as.POSIXlt(Sys.Date())$mday + 1) & Date_Out < (Sys.Date() - as.POSIXlt(Sys.Date())$mday + 1) & Reason == "Short-term Sick"], aes(Month, Shifts)) +
  stat_summary(fun.y = sum, geom = "line", aes(colour = Reason)) + scale_x_date(breaks = pretty_breaks(10)) 

ggplot(Personnel[Date_Out >= ((Sys.Date()-395) - as.POSIXlt(Sys.Date())$mday + 1) & Date_Out < (Sys.Date() - as.POSIXlt(Sys.Date())$mday + 1) & Reason == "Long-term Sick"], aes(Month, Shifts)) +
  stat_summary(fun.y = sum, geom = "line", aes(colour = Reason)) + scale_x_date(breaks = pretty_breaks(10))

ggplot(Personnel[Date_Out >= ((Sys.Date()-395) - as.POSIXlt(Sys.Date())$mday + 1) & Date_Out < (Sys.Date() - as.POSIXlt(Sys.Date())$mday + 1) & Reason == "Short-term Injured"], aes(Month, Shifts)) +
  stat_summary(fun.y = sum, geom = "line", aes(colour = Reason)) + scale_x_date(breaks = pretty_breaks(10))

ggplot(Personnel[Date_Out >= ((Sys.Date()-395) - as.POSIXlt(Sys.Date())$mday + 1) & Date_Out < (Sys.Date() - as.POSIXlt(Sys.Date())$mday + 1) & Reason == "Long-term Injured"], aes(Month, Shifts)) +
  stat_summary(fun.y = sum, geom = "line", aes(colour = Reason)) + scale_x_date(breaks = pretty_breaks(10))

ggplot(Personnel[Date_Out >= ((Sys.Date()-395) - as.POSIXlt(Sys.Date())$mday + 1) & Date_Out < (Sys.Date() - as.POSIXlt(Sys.Date())$mday + 1) & Reason == "Personal Day"], aes(Month, Shifts)) +
  stat_summary(fun.y = sum, geom = "line", aes(colour = Reason)) + scale_x_date(breaks = pretty_breaks(10))

ggplot(Personnel[Date_Out >= ((Sys.Date()-395) - as.POSIXlt(Sys.Date())$mday + 1) & Date_Out < (Sys.Date() - as.POSIXlt(Sys.Date())$mday + 1) & Reason == "Vacation Day"], aes(Month, Shifts)) +
  stat_summary(fun.y = sum, geom = "line", aes(colour = Reason)) + scale_x_date(breaks = pretty_breaks(10))

ggplot(Personnel[Date_Out >= ((Sys.Date()-395) - as.POSIXlt(Sys.Date())$mday + 1) & Date_Out < (Sys.Date() - as.POSIXlt(Sys.Date())$mday + 1) & Reason == "Vacation Week"], aes(Month, Shifts)) +
  stat_summary(fun.y = sum, geom = "line", aes(colour = Reason)) + scale_x_date(breaks = pretty_breaks(10))

ggplot(PLE[Date_Out >= ((Sys.Date()-395) - as.POSIXlt(Sys.Date())$mday + 1) & Date_Out < (Sys.Date() - as.POSIXlt(Sys.Date())$mday + 1)], aes(Month, PLE)) +
  stat_summary(fun.y = sum, geom = "line") + scale_x_date(breaks = pretty_breaks(10))
  

# Find the count by day for all Reasons
Personnel_count = Personnel %>% 
  group_by(Date_Out) %>% 
  summarise(Count = n_distinct(Employee_ID)) %>%
  arrange(Date_Out)


# Find the count by day by Reason
Personnel_count_reasons = Personnel %>% 
  group_by(Date_Out, Reason) %>% 
  summarise(Count = n_distinct(Employee_ID)) %>%
  spread(Reason, Count, fill = 0) %>%
  arrange(Date_Out)

#### Retrieve weather data from BOS airport weather data ####
# Data can only retrieve so much at once, so it needs to be separated into years
# This area still needs some work to determine importance of weather data
bos1 <- getWeatherForDate("BOS", start_date="2011-01-01", end_date = "2011-12-31", opt_custom_columns = TRUE, custom_columns = c(2,3,4,22))
bos2 <- getWeatherForDate("BOS", start_date="2012-01-01", end_date = "2012-12-31", opt_custom_columns = TRUE, custom_columns = c(2,3,4,22))
bos3 <- getWeatherForDate("BOS", start_date="2013-01-01", end_date = "2013-12-31", opt_custom_columns = TRUE, custom_columns = c(2,3,4,22))
bos4 <- getWeatherForDate("BOS", start_date="2014-01-01", end_date = "2014-12-31", opt_custom_columns = TRUE, custom_columns = c(2,3,4,22))
bos5 <- getWeatherForDate("BOS", start_date="2015-01-01", end_date = as.character(Sys.Date()), opt_custom_columns = TRUE, custom_columns = c(2,3,4,22))

# Bind weather data together
w <- rbind(bos1, bos2, bos3, bos4, bos5)
setnames(w, "Date", "Date_Out")
w$Date_Out <- as.Date(w$Date_Out)

# Merge weather data with personnel_count and personnel_count_reasons
Personnel_count = left_join(Personnel_count, w, by = "Date_Out", copy = T)
Personnel_count_reasons = left_join(Personnel_count_reasons, w, by = "Date_Out", copy = T)
Personnel_count <- Personnel_count[complete.cases(Personnel_count$Max_TemperatureF),]
Personnel_count_reasons <- Personnel_count_reasons[complete.cases(Personnel_count_reasons$Max_TemperatureF),]
setnames(Personnel_count_reasons, names(Personnel_count_reasons), gsub(" |-", "_", names(Personnel_count_reasons)))

# Take some correlations
cor(Personnel_count$Count, Personnel_count$Max_TemperatureF)
cor(Personnel_count$Count, Personnel_count$Mean_TemperatureF)
cor(Personnel_count$Count, Personnel_count$Min_TemperatureF)
cor(Personnel_count_reasons$Short_term_Sick, Personnel_count_reasons$Max_TemperatureF)
cor(Personnel_count_reasons$Short_term_Sick, Personnel_count_reasons$Mean_TemperatureF)
cor(Personnel_count_reasons$Short_term_Sick, Personnel_count_reasons$Min_TemperatureF)

