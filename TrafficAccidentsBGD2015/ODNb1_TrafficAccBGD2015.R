
### --- Open Data R :: 01/24/2017

### --- Data Set: OPENDATA_SNEZGODE_exc.csv
### --- Description: Traffic Accidents in Belgrade, 2015
### --- Source: data.gov.rs

### ------------------------------------------------------
### ------------------ Setup
### ------------------------------------------------------
rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)

### --- Working Directory
wDir <- '/home/goran/Work/_DataScienceActual/OpenData_R/source/'
setwd(wDir)

### --- Load Raw Data Set
rawData <- read.csv('OPENDATA_SNEZGODE_exc.csv',
                    header = T,
                    check.names = F,
                    stringsAsFactors = F) 

### --- Inspect Data Set
dim(rawData)
glimpse(rawData)

# - VRSTA_NEZ
unique(rawData$VRSTA_NEZ)
table(rawData$VRSTA_NEZ)

# - NAZIV_TIP
unique(rawData$NAZIV_TIP)
table(rawData$NAZIV_TIP)

# - NAZIV_DET
unique(rawData$NAZIV_DET)

# - NAZIV_TIP vs. VRSTA_NEZ
table(rawData$VRSTA_NEZ, rawData$NAZIV_TIP)


### ------------------------------------------------------
### ------------------ Data Wrangling
### ------------------------------------------------------

# - Separate Date and Time
rawData <- separate(data = rawData,
                    col = VREME_NEZ,
                    into = c('Date', 'Time'),
                    sep = ',',
                    remove = F)

# - Date --> Day, Month, Year
rawData <- separate(data = rawData,
                    col = Date,
                    into = c('Day', 'Month', 'Year'),
                    sep = '\\.',
                    remove = F)

# - Date --> Hour, Minute
rawData <- separate(data = rawData,
                    col = Time,
                    into = c('Hour', 'Minute'),
                    sep = ':',
                    remove = F)

# - New Date-Time format:
rawData$DateTime <- paste(
  paste(rawData$Month,
        rawData$Day,
        rawData$Year,
        sep = '/'),
  paste(rawData$Hour,
        rawData$Min,
        sep = ":"),
  sep = ", "
)

# - Check Data Set:
table(rawData$Year, rawData$Month)

# - Year 2013, 2014: probably errors; correct
rawData[rawData$Year == '2014', 'Year'] <- '2015'
rawData[rawData$Year == '2013', 'Year'] <- '2015'

# - Check Data Set:
table(rawData$Year, rawData$Month)

# - December --> too few data points; drop:
rawData <- rawData[-which(rawData$Month == '12'), ]

# - Check Data Set:
table(rawData$Year, rawData$Month)

# - Outcome: from VRSTA_NEZ
unique(rawData$VRSTA_NEZ)
rawData$Outcome <- factor(rawData$VRSTA_NEZ)
levels(rawData$Outcome) <- c('Damage', 'Death','Injury')
levels(rawData$Outcome)
# - Outcome as ordered factor
rawData$Outcome <- factor(rawData$Outcome, 
                       levels = c('Damage', 'Injury', 'Death'),
                       ordered = T)

# - Type: from NAZIV_TIP
unique(rawData$NAZIV_TIP)
rawData$Type <- factor(rawData$NAZIV_TIP)
levels(rawData$Type) <- c('None', 
                          '2VehTurnOrCross',
                          '2VehNoTurn',
                          '1Veh',
                          'parkedVeh',
                          'pedestrian')
levels(rawData$Type)
# - how many accidents were categorized?
length(rawData$Type) - sum(rawData$Type == 'None')

# - Lat, Lon
rawData$Lat <- rawData$WGS_Y
rawData$Lon <- rawData$WGS_X

# - Check Geographical Data
# - Belgrade is on: Coordinates: 44°49′N 20°28′E
# - source: https://en.wikipedia.org/wiki/Belgrade
range(rawData$Lon)
range(rawData$Lat)

# - filter
rawData <- rawData %>% 
  filter (Lon < mean(Lon) + 3*sd(Lon), 
          Lon > mean(Lon) -3*sd(Lon), 
          Lat < mean(Lat) + 3*sd(Lat), 
          Lat > mean(Lat) - 3*sd(Lat))


# - Sort Data Set
rawData <- rawData[order(rawData$Day, 
                         rawData$Month,
                         rawData$Hour,
                         rawData$Minute), ]

### --- Working Data Set
dataSet <- rawData[, c('DateTime', 'Day', 'Month', 'Year',
                       'Time', 'Hour', 'Minute', 'Lat', 'Lon',
                       'Type', 'Outcome')]
rm(rawData)

### --- Save Working Data Set
outDir <- '/home/goran/Work/_DataScienceActual/OpenData_R/workingData/'
setwd(outDir)
write.csv(dataSet, file = 'MUP2015_TrafficAccidentsBGD.csv')


### ------------------------------------------------------
### ------------------ Visualize w. leaflet
### ------------------------------------------------------

### --- Accident Density

# - add an HTML descriptive column to dataSet
dataSet$Description <- paste(
  '<b>Date: </b>', paste(dataSet$Month, dataSet$Day, dataSet$Year, sep = "/"), '<br>',
  '<b>Time: </b>', paste(dataSet$Hour, dataSet$Minute, sep = ":"), '<br>',
  '<b>Outcome: </b>', dataSet$Outcome,
  sep = '')

library(leaflet)
accMap <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng = dataSet$Lon,
             lat= dataSet$Lat,
             popup = dataSet$Description, 
             clusterOptions = markerClusterOptions())
print(accMap)


### --- Accident Severity
# - Add Outcome Color
dataSet$OutcomeColor <- dataSet$Outcome
dataSet$OutcomeColor <- recode(dataSet$OutcomeColor, 
                               Damage = 'Yellow',
                               Injury = 'DarkOrange',
                               Death = 'Red')
# - Add Outcome Warning (Marker Size)
dataSet$OutcomeWarning <- dataSet$Outcome
dataSet$OutcomeWarning <- recode(dataSet$OutcomeWarning, 
                                 Damage = '10',
                                 Injury = '15',
                                 Death = '20')
dataSet$OutcomeWarning <- as.numeric(dataSet$OutcomeWarning)
  
accMap <- leaflet() %>%
  addTiles(urlTemplate = "http://{s}.tiles.wmflabs.org/bw-mapnik/{z}/{x}/{y}.png", 
           attribution = '&copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
  addCircleMarkers(lng = dataSet$Lon,
                   lat= dataSet$Lat,
                   radius = dataSet$OutcomeWarning*2,
                   popup = dataSet$Description,
                   fill = T,
                   fillColor = dataSet$OutcomeColor,
                   stroke = F,
                   fillOpacity = .5)
print(accMap)
