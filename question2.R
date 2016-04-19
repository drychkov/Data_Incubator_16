library(readr)
library(dplyr)
library(tidyr)

# read data
df11 <- read_csv('./Calls_for_Service_2011.csv')
df12 <- read_csv('./Calls_for_Service_2012.csv')
df13 <- read_csv('./Calls_for_Service_2013.csv')
df14 <- read_csv('./Calls_for_Service_2014.csv')
df15 <- read_csv('./Calls_for_Service_2015.csv')

df <- bind_rows(df11, df12, df13, df14, df15)

# Recycle memory
# Save '11 and '15 for later
df12 = df13 = df14 <- NULL

# =====================================================
# Part 1
# What fraction of calls are of the most common type?

# take all call types
types <- df$Type_

# find most frequent call and the fraction
freqCall <- names(which.max(table(types)))
callFrac <- sum(types == freqCall)/ length(types)

print(callFrac, digits = 10) # 0.24539537

types <- NULL

# =====================================================
# Part 2
# Some calls result in an officer being dispatched to the scene, and some log an arrival time. 
# What is the median response time (dispatch to arrival), in seconds, considering only valid 
# (i.e. non-negative) times?

dispatch <- df[,c("TimeDispatch", "TimeArrive")]

# convert into date-time format
dispatch$TimeArrive <- strptime(dispatch$TimeArrive, "%m/%d/%Y %I:%M:%S %p")
dispatch$TimeDispatch <- strptime(dispatch$TimeDispatch, "%m/%d/%Y %I:%M:%S %p")

# Find time difference
dispatch$responseTime <- as.numeric(dispatch$TimeArrive - dispatch$TimeDispatch)

# Median response time
medianTime <- median(dispatch$responseTime[dispatch$responseTime >= 0], na.rm = TRUE)
print(medianTime, digits = 10) # 269 s

dispatch <- NULL

# =====================================================
# Part 3
# Work out the average (mean) response time in each district. 
# What is the difference between the average response times of the districts 
# with the longest and shortest times?

response <- df[,c("TimeCreate", "TimeDispatch", "PoliceDistrict")]

# Convert into date-time format
response$TimeCreate <- strptime(response$TimeCreate, "%m/%d/%Y %I:%M:%S %p")
response$TimeDispatch <- strptime(response$TimeDispatch, "%m/%d/%Y %I:%M:%S %p")

# Find time difference
response$TimeDiff <- as.numeric(difftime(response$TimeDispatch, response$TimeCreate))

# Remove NAs
response <- response[!is.na(response$TimeDiff),][, c(3,4)]

# Summarise by District and mean response time
timeMeans <-  response %>% group_by(PoliceDistrict) %>% summarise(mean(TimeDiff))

# Find differense between max and min
print(max(timeMeans[,2]) - min(timeMeans[,2]), digits = 10) # 2594.116151

response <- NULL

# =====================================================
# Part 4
# We can define surprising event types as those that occur more often in a district than 
# they do over the whole city. What is the largest ratio of the conditional probability of 
# an event type given a district to the unconditional probably of that event type? 
# Consider only events types which have more than 100 events. Note that some events have their 
# locations anonymized and are reported as being in district "0". These should be ignored.


typesDistrict <- df[,c("Type_", "PoliceDistrict")]

# Delete anonymous district
typesDistrict <- typesDistrict[!(typesDistrict$PoliceDistrict == 0),]

# Reshaping
typesDistrict <- typesDistrict %>% 
  group_by(Type_, PoliceDistrict) %>% 
  summarise(Events = length(Type_)) %>% 
  spread(PoliceDistrict, Events) 

typesDistrict <- typesDistrict %>% transform(Events=rowSums(typesDistrict[,2:9], na.rm = TRUE))

# Filter rare events
typesDistrict <- typesDistrict[typesDistrict$Events > 100,]

# Find probabilities
typesDistrict$EventPercent <- typesDistrict$Events / sum(typesDistrict$Events)

# Convert into fractions
typesSums <- colSums(typesDistrict[,2:9], na.rm = TRUE)
for (j in 1:8) {
  typesDistrict[,j+1] <- typesDistrict[,j+1] / typesSums[j]
}

# Set all NAs to zero
typesDistrict[is.na(typesDistrict)] = 0

# Reindexing
rownames(typesDistrict) <- 1:nrow(typesDistrict)

# Looking for surprising events
for (rows in 1:nrow(typesDistrict)) {
  typesDistrict$MaxRatio[rows] <- max(typesDistrict[rows, 2:9]) / typesDistrict$EventPercent[rows]
}

print(max(typesDistrict$MaxRatio), digits = 10) # 6.949613502

typesDistrict <- NULL
          
# =====================================================
# Part 5
# Find the call type that displayed the largest percentage decrease in volume between 2011 and 2015. 
# What is the fraction of the 2011 volume that this decrease represents? The answer should be between 0 and 1.


callTypes11 <- count(df11, Type_)
callTypes11$n <- callTypes11$n / sum(callTypes11$n)
callTypes15 <- count(df15, Type_)
callTypes15$n <- callTypes15$n / sum(callTypes15$n)

# Join both call data
callTypes <- full_join(callTypes11, callTypes15, by = "Type_")

# Assume NA means equals to zero
callTypes[is.na(callTypes)] = 0

# Call changes
callTypes$CallChange <- callTypes$n.x - callTypes$n.y

# Find max drop
maxDrop <- callTypes[which.max(callTypes$CallChang),]
maxDrop11 <- as.numeric(maxDrop[,2]) 

print(maxDrop11, digits = 10) # 0.1947513785

callTypes = callTypes11 = callTypes15 = callFreqs = df11 = df15 <- NULL

# =====================================================
# Part 6
# The disposition represents the action that was taken to address the serivce call. 
# Consider how the disposition of calls changes with the hour of the record's creation time. 
# Find the disposition whose fraction of that hour's disposition varies the most over a typical day. 
# What is its change (maximum fraction minus minimum fraction)?

action <- df[,c("TimeCreate", "Disposition")]

# Convert into date-time format and take only hours
action$TimeCreate <- strptime(action$TimeCreate, "%m/%d/%Y %I:%M:%S %p")
action$TimeCreate <- as.integer(format(action$TimeCreate, "%H"))


# Reshaping
action <- action %>% 
  group_by(TimeCreate, Disposition) %>% 
  summarise(Events = length(Disposition)) %>% 
  spread(TimeCreate, Events) 

eventSums <- colSums(action[,2:25], na.rm = TRUE) 

for (j in 0:23) {
  action[,j+2] <- action[,j+2]/ eventSums[j+1]
}

# Find max and min values
action$Max <-  apply(action[,2:25], 1, max, na.rm = TRUE)
action$Min <-  apply(action[,2:25], 1, min, na.rm = TRUE)
# Find max-min 
action$Change <- action$Max - action$Min

print(max(action$Change), digits = 10) # 0.1880895

action <- NULL

# =====================================================
# Part 7
# We can use the call locations to estimate the areas of the police districts. 
# Represent each as an ellipse with semi-axes given by a single standard deviation of the 
# longitude and latitude. What is the area, in square kilometers, of the largest district 
# measured in this manner?

# Constants
a <- 6378.137
b <- 6356.752314245
e2 <- (a^2 - b^2)/ a^2

# Function to find latitude change in km
latKm <- function(angle, a, e2){
  angle <- (pi * angle) / 180
  dist <- (pi * a * (1 - e2)) / (180 * (1 - e2 * sin(angle)^2)^(3/2))
  dist
}

# Function to find longitude change in km
lonKm <- function(angle, a, e2){
  angle <- (pi * angle) / 180
  dist <- (pi * a * cos(angle)) / (180 * sqrt(1 - e2 * sin(angle)^2))
  dist
}

# Extract data
distLoc <- df[,c("PoliceDistrict", "Location")]

# Remove brackets from location values
distLoc <- data.frame(apply(distLoc, 2, function(x) gsub("[()]", '', x)), stringsAsFactors = FALSE)

# Split location into latitude and longitude
distLoc <- distLoc %>%
  separate(Location, c("Lat", "Lon"), ", ") %>%
  apply(2, function(x) as.numeric(x)) %>%
  data.frame()

# Filter wrong data based on manual observations
distLoc <- distLoc[(distLoc$Lat > 29.89 & distLoc$Lon < 89.7),] 

# Find mean and sd by district
distLoc <- distLoc %>%
  group_by(PoliceDistrict) %>%
  summarise(MeanLat = mean(Lat), LatSd = sd(Lat), MeanLon = mean(Lon), LonSd = sd(Lon))

# Remove 0th district and NA
distLoc <- distLoc[-c(1,10),]

# Convert SDs into semi-axes of ellipses in km
distLoc$LatSd <- latKm(distLoc$MeanLat, a, e2) * distLoc$LatSd
distLoc$LonSd <- lonKm(distLoc$MeanLat, a, e2) * distLoc$LonSd

# Find areas
distLoc$Area <- pi * distLoc$LatSd * distLoc$LonSd

# Return largest area
print(max(distLoc$Area), digits = 10) # 25.17403302

distLoc <- NULL

# =====================================================
# Part 8
# The calls are assigned a priority. Some types of calls will receive a greater variety of priorities. 
# To understand which type of call has the most variation in priority, find the type of call whose 
# most common priority is the smallest fraction of all calls of that type. What is that smallest fraction?

# Extract data
priority <- df[,c("Type_", "Priority")]

# Group by Type and find most frequent priority for each type
callFreqs <- priority %>%
  group_by(Type_) %>%
  summarise(Freq = names(which.max(table(Priority))))

# Find number of calls for each type and most frequent priority as a fraction
for (i in 1:length(callFreqs$Type_)){
  calls <- sum((priority$Type_ == callFreqs$Type_[i] & priority$Priority == callFreqs$Freq[i]), na.rm = TRUE)
  totalCalls <- sum(priority$Type_ == callFreqs$Type_[i], na.rm = TRUE)
  callFreqs$numCalls[i] <- calls / totalCalls
}

# Find and return minimum fraction
call <- min(callFreqs$numCalls)
print(call, digits = 10) # 0.3073611709

priority <- NULL

