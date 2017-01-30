#' # Open Data Notebooks 2017-01 :: ODN2017-01
#' ## Case Study: Traffic Accidents in Belgrade 2015.
#' #### Data Set: OPENDATA_SNEZGODE_exc.csv
#' #### Source: [data.gov.rs](http://data.gov.rs)
#' #### Description: Traffic Accidents in Belgrade, 2015.
#' 
#' *** 
#' ![](../img/GoranSMilovanovic.jpg)
#' 
#' **Author:** [Goran S. Milovanovic](http//www.exactness.net), [Data Science Serbia](http//www.datascience.rs)
#' 
#' **Notebook:** 01/30/2017, Belgrade, Serbia
#' 
#' ![](../img/DataScienceSerbia_Logo.png)
#' 
#' ***
#' 
#' This notebook accompanied the first [Open Data R Meetup](https://www.meetup.com/BelgradeR/events/237202071/) in Belgrade, organized by [Data Science Serbia](http//www.datascience.rs) in [Startit Center](http://en.startit.rs/), Savska 5, Belgrade, 01/30/2017. The event took place to introduce the Data Science community in Serbia with the Open Data initative people, and celebrate the completion of the 2nd [Introduction to R for Data Science](https://github.com/GoranMilovanovic/Introduction-to-R-for-Data-Science) course that Data Science Serbia has provided in Startit, Belgrade. As of now, more than 45 people have learned - or are currently learning - to code in R for free, thanks to our efforts to develop the Introduction to R for Data Science course.
#' 
#' The notebook focuses on an exploratory analysis of the test open data set of Traffic Accidents in Belgrade in 2015, provided at the [Open Data Portal of the Republic of Serbia](http://data.gov.rs/sr/) *that is currently under development*. The data set was kindly provided to the Open Data Portal by the [Republic of Serbia Ministry of Interior](http://mup.gov.rs/wps/portal/en/!ut/p/z1/04_Sj9CPykssy0xPLMnMz0vMAfIjo8zi_S19zQzdDYy83c1cjQwcA80tXbxdLYwtPAz0wwkpiAJKG-AAjiD9UYSUFORGGKQ7KioCADcc2vk!/dz/d5/L0lJSkovd0RNQUJrQUVnQSEhLzRObEhVeEEhL1o2X085TTYxRzAySzhHSUUwQTY3R0pHR04yT00zL2Vu/). Many more open data sets will be indexed and uploaded in the forthcoming weeks and months. 
#' 
#' Besides focusing on the exploration and visualization of this test data set, we demonstrate the basic usage of [{weatherData}](https://cran.r-project.org/web/packages/weatherData/index.html) to fetch historical weather data to R, [{wbstats}](https://cran.r-project.org/web/packages/wbstats/index.html) to access the rich [World Data Bank](http://databank.worldbank.org/data/home.aspx) time series, and [{ISOcodes}](https://cran.r-project.org/web/packages/ISOcodes/) packages in R.
#' 
#' Some exploratory modeling (Negative Binomial Regression with `glm.nb()` and Ordinal Logistic Regression with `clm()` from [{ordinal}](https://cran.r-project.org/web/packages/ordinal/index.html)) is exercised merely to assess the *prima facie* effects of the most influential factors.
#' 
#' ***
#' 
#' ### 1. Setup
#' 
#' Load libraries + raw data:
#' 
## ----echo = T, message = F-----------------------------------------------
rm(list = ls())
library(dplyr)
library(tidyr)
library(ggplot2)
library(knitr)
library(ggplot2)
library(ggmap)

### --- Working Directory
wDir <- '../TrafficAccidentsBGD2015'
setwd(wDir)

### --- Load Raw Data Set
fileLoc <- 'OPENDATA_SNEZGODE exc.csv'
rawData <- read.csv(fileLoc,
                    header = T,
                    check.names = F,
                    stringsAsFactors = F) 

### --- Inspect Data Set
dim(rawData)

#' 
#' Take a sneak peek at the data set:
#' 
## ----echo = T------------------------------------------------------------
glimpse(rawData)

#' 
#' What are we looking at:
#' 
## ----echo = T, message = F-----------------------------------------------
bgdMap <- get_map(location = 'Belgrade',
                  maptype = "roadmap")
ggmap(bgdMap, 
      extent = "device") + 
  geom_point(data = rawData,
             aes(x = WGS_X, y = WGS_Y), 
             size = .25, 
             color = "blue",
             alpha = .35) 

#' 
#' With 220 points found outisde of the Belgrade Google Map area we're already certain that some measurement error is present in the geolocalisation data. We'll take care of it later.
#' 
#' Let's take a quick look at the city core in a 2D density plot with `geom_density2d` from {ggplot2}:
#' 
## ----echo = T, message = F-----------------------------------------------
bgdMap <- get_map(location = 'Kneza Milosa, Belgrade',
                  maptype = "roadmap",
                  zoom = 16)
ggmap(bgdMap, 
      extent = "device") + 
  geom_density2d(data = rawData,
                 aes(x = WGS_X, y = WGS_Y),
                 size = .1) +
  stat_density2d(data = rawData,
                 aes(x = WGS_X, y = WGS_Y, 
                     fill = ..level.., 
                     alpha = ..level..), 
                 size = 0.01, 
                 bins = 16, 
                 geom = "polygon") + 
  scale_fill_gradient(low = "white", high = "blue") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE) + 
  theme(legend.position="none")

#' 
## ----echo = T, message = F-----------------------------------------------
bgdMap <- get_map(location = 'Mostar, Belgrade',
                  maptype = "roadmap",
                  zoom = 16)
ggmap(bgdMap, 
      extent = "device") + 
  geom_density2d(data = rawData,
                 aes(x = WGS_X, y = WGS_Y),
                 size = .1) +
  stat_density2d(data = rawData, 
                 aes(x = WGS_X, y = WGS_Y, 
                     fill = ..level.., 
                     alpha = ..level..), 
                 size = 0.01, 
                 bins = 16, 
                 geom = "polygon") + 
  scale_fill_gradient(low = "white", high = "blue") + 
  scale_alpha(range = c(0, 0.3), guide = FALSE) +
  theme(legend.position="none")

#' 
#' Let's start figuring out the variables:
#' 
## ----echo = T------------------------------------------------------------
# - VRSTA_NEZ
unique(rawData$VRSTA_NEZ)

#' 
## ----echo = T------------------------------------------------------------
table(rawData$VRSTA_NEZ)

#' 
## ----echo = T------------------------------------------------------------
# - NAZIV_TIP
unique(rawData$NAZIV_TIP)

#' 
## ----echo = T------------------------------------------------------------
table(rawData$NAZIV_TIP)

#' 
## ----echo = T------------------------------------------------------------
# - NAZIV_DET
head(unique(rawData$NAZIV_DET))

#' 
## ----echo = T, results = 'asis'------------------------------------------
# - NAZIV_TIP vs. VRSTA_NEZ
kable(table(rawData$VRSTA_NEZ, rawData$NAZIV_TIP))

#' 
#' ***
#' 
#' ### 2. Data Wrangling: cleaning this up + some re-structuring for future use
#' 
#' Separate Date from Time:
#' 
## ----echo = T------------------------------------------------------------
# - Separate Date and Time
rawData <- separate(data = rawData,
                    col = VREME_NEZ,
                    into = c('Date', 'Time'),
                    sep = ',',
                    remove = F)

#' 
#' Separate Date to Day, Month, and Year:
#' 
## ----echo = T------------------------------------------------------------
# - Date --> Day, Month, Year
rawData <- separate(data = rawData,
                    col = Date,
                    into = c('Day', 'Month', 'Year'),
                    sep = '\\.',
                    remove = F)

#' 
#' Separate Time to Hour, Minute: 
#' 
## ----echo = T------------------------------------------------------------
# - Time --> Hour, Minute
rawData <- separate(data = rawData,
                    col = Time,
                    into = c('Hour', 'Minute'),
                    sep = ':',
                    remove = F)

#' 
#' Add a new Date-Time format:
#' 
## ----echo = T------------------------------------------------------------
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

#' 
#' And check:
#' 
## ----echo = T------------------------------------------------------------
# - Check Data Set:
kable(table(rawData$Year, rawData$Month))

#' 
#' There are some data points labeled by '2013' and '2014'; probably errors; correct:
#' 
## ----echo = T------------------------------------------------------------
# - Year 2013, 2014: probably errors; correct
rawData[rawData$Year == '2014', 'Year'] <- '2015'
rawData[rawData$Year == '2013', 'Year'] <- '2015'

# - Check Data Set:
kable(table(rawData$Year, rawData$Month))

#' 
#' Very few data points for December; get rid of it:
#' 
## ----echo = T------------------------------------------------------------
# - December --> too few data points; drop:
rawData <- rawData[-which(rawData$Month == '12'), ]

#' 
#' And check:
#' 
## ----echo = T------------------------------------------------------------
# - Check Data Set:
kable(table(rawData$Year, rawData$Month))

#' 
#' What's this:
#' 
## ----echo = T------------------------------------------------------------
# - Outcome: from VRSTA_NEZ
unique(rawData$VRSTA_NEZ)

#' 
#' Recode:
#' 
## ----echo = T------------------------------------------------------------
rawData$Outcome <- factor(rawData$VRSTA_NEZ)
levels(rawData$Outcome) <- c('Damage', 'Death','Injury')
levels(rawData$Outcome)

#' 
#' Turn into an ordered factor (possible use: ordinal logistic)
#' 
## ----echo = T------------------------------------------------------------
# - Outcome as ordered factor
rawData$Outcome <- factor(rawData$Outcome, 
                       levels = c('Damage', 'Injury', 'Death'),
                       ordered = T)

#' 
#' What is this + recode:
#' 
## ----echo = T------------------------------------------------------------
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

#' 
#' How many out of 12K+ recorded accidents were categorized:
#' 
## ----echo = T------------------------------------------------------------
# - how many accidents were categorized?
length(rawData$Type) - sum(rawData$Type == 'None')

#' 
#' Geography:
#' 
## ----echo = T------------------------------------------------------------
# - Lat, Lon
rawData$Lat <- rawData$WGS_Y
rawData$Lon <- rawData$WGS_X

# - Check Geographical Data
# - Belgrade is on: Coordinates: 44°49′N 20°28′E
# - source: https://en.wikipedia.org/wiki/Belgrade
range(rawData$Lon)

#' 
## ----echo = T------------------------------------------------------------
range(rawData$Lat)

#' 
#' Is this cool? Probably not. Keep only data points between $-3SD$ and $+3SD$.
#' N.B. This is not the right procedure to clean-up the data set. The correct procedure would involve having a vector map of Belgrade and then keeping only data points strictly found in the administratively defined region of interest.
#' 
## ----echo = T------------------------------------------------------------
# - filter
rawData <- rawData %>% 
  filter (Lon < mean(Lon) + 3*sd(Lon), 
          Lon > mean(Lon) -3*sd(Lon), 
          Lat < mean(Lat) + 3*sd(Lat), 
          Lat > mean(Lat) - 3*sd(Lat))

#' 
#' Sort:
#' 
## ----echo = T------------------------------------------------------------
# - Sort Data Set
rawData <- rawData[order(rawData$Day, 
                         rawData$Month,
                         rawData$Hour,
                         rawData$Minute), ]

#' 
#' `dataSet` will be the working data set:
#' 
## ----echo = T------------------------------------------------------------
### --- Working Data Set
dataSet <- rawData[, c('DateTime', 'Day', 'Month', 'Year',
                       'Time', 'Hour', 'Minute', 'Lat', 'Lon',
                       'Type', 'Outcome')]
rm(rawData)

#' 
#' Save:
#' 
## ----echo = T------------------------------------------------------------
### --- Save Working Data Set
### --- Working Directory
wDir <- '../TrafficAccidentsBGD2015'
setwd(wDir)
write.csv(dataSet, file = 'MUP2015_TrafficAccidentsBGD.csv')

#' 
#' ***
#' 
#' ### 3. Exploratory Data Analysis
#' 
#' Now, let's visualize properly w. `{leaflet}`:
#' 
#' #### Accident Density
#' 
#' The density of traffic accidents is dynamically represents by markers in the Leaflet map. Click or zoom in too discover fine-grained information.
#' 
## ----echo = T------------------------------------------------------------
library(leaflet)
# - add an HTML descriptive column to dataSet
dataSet$Description <- paste(
  '<b>Date: </b>', paste(dataSet$Month, dataSet$Day, dataSet$Year, sep = "/"), '<br>',
  '<b>Time: </b>', paste(dataSet$Hour, dataSet$Minute, sep = ":"), '<br>',
  '<b>Outcome: </b>', dataSet$Outcome,
  sep = '')

accMap <- leaflet() %>%
  addTiles() %>%
  addMarkers(lng = dataSet$Lon,
             lat= dataSet$Lat,
             popup = dataSet$Description, 
             clusterOptions = markerClusterOptions())
accMap

#' 
#' ***
#' 
#' #### Accident Severity
#' 
#' The severity of the traffic accident outcomes is represents by marker size and colors: yellow stands for 'Damage', orange for 'Injury', red for 'Death'. Click or zoom in too discover fine-grained information.
#' 
## ----echo = T------------------------------------------------------------
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
accMap

#' 
#' ***
#' 
#' What was the probability of a traffic accident **resulting in injury** in Belgrade, 2015? N.B. That is the *conditional probability*, $P(Injury|Road Accident)$:
#' 
## ----echo = T------------------------------------------------------------
outcomes <- table(dataSet$Outcome)
pInjury <- outcomes['Injury']/sum(outcomes)
pInjury

#' 
#' That is some 25%:
#' 
## ----echo = T------------------------------------------------------------
pInjury*100

#' 
#' What was the probability of a traffic accident **with fatal consequences** in Belgrade, 2015? N.B. That is the *conditional probability*, $P(Fatal|Road Accident)$:
#' 
## ----echo = T------------------------------------------------------------
outcomes <- table(dataSet$Outcome)
pDeath <- unname(outcomes['Death']/sum(outcomes))
pDeath

#' 
#' which is less than a half percent:
#' 
## ----echo = T------------------------------------------------------------
pDeath*100

#' 
#' **Reminder:** We do not know what is the probability of being engaged in a traffic accident, so do not rush with associating emotional responses to these probabilities...
#' 
#' Accidents by months:
#' 
## ----echo = T------------------------------------------------------------
byMonth <- dataSet %>%
  group_by(Month) %>%
  summarise(Accidents = n())
byMonth$Month <- as.numeric(byMonth$Month)
# byMonth$Month <- factor(byMonth$Month, levels <- byMonth$Month)
ggplot(byMonth, aes(x = Month, y = Accidents)) + 
  geom_line(color = "blue", size = .5) + 
  geom_point(color = "blue", size = 1.5) + 
  geom_point(color = "white", size = 1) + 
  scale_x_continuous(breaks = byMonth$Month,
                   labels = month.abb[as.numeric(byMonth$Month)]) +
  ggtitle('Belgrade, 2015: Traffic Accidents by Month') +
  ylim(1000, max(byMonth$Accidents)+100) +
  theme_bw() +
  theme(plot.title = element_text(size = 11))

#' 
#' Accidents by hour:
#' 
## ----echo = T------------------------------------------------------------
byHour <- dataSet %>%
  group_by(Hour) %>%
  summarise(Accidents = n())
byHour$HourData <- as.numeric(byHour$Hour)
ggplot(byHour, aes(y = Accidents, x = HourData)) + 
  geom_line(color = "blue", size = .5) + 
  geom_point(color = "blue", size = 1.5) + 
  geom_point(color = "white", size = 1) + 
  scale_x_continuous(breaks = byHour$HourData-.5,
                   labels = byHour$Hour) +
  ggtitle('Belgrade, 2015: Traffic Accidents by Hour') + 
  xlab('Hour') +
  theme_bw() +
  theme(plot.title = element_text(size = 11))

#' 
#' Accident outcome vs. Month:
#' 
## ----echo = T------------------------------------------------------------
dataSet$MonthLabel <- month.abb[as.numeric(dataSet$Month)]
plot(table(dataSet$MonthLabel, dataSet$Outcome),
     main = 'Accident Outcomes per Month',
     col = "gold")

#' 
#' No pattern seems to be present:
#' 
## ----echo = T, results = 'asis'------------------------------------------
testAccMonth <- as.matrix(table(dataSet$MonthLabel, dataSet$Outcome))
kable(testAccMonth)

#' 
#' To confirm, ${\chi}^2$-test for contingency tables:
#' 
## ----echo = T, results = 'asis'------------------------------------------
chisq.test(testAccMonth, 
           simulate.p.value = T)

#' 
#' When do the fatal accidents take place?
#' 
## ----echo = T, results = 'asis'------------------------------------------
fatalSet <- dataSet %>% 
  filter(Outcome == 'Death') %>% 
  group_by(Hour) %>% 
  summarise(Accidents = n()) %>% 
  t()
colnames(fatalSet) <- fatalSet[1,]
fatalSet <- fatalSet[-1,]
kable(t(fatalSet), caption = 'Fatal Accidents by Hour:')

#' 
#' Is there a pattern in the hourly distribution of the accident outcome?
#' 
## ----echo = T, results = 'asis'------------------------------------------
byHourSet <- dataSet %>%
  group_by(Hour, Outcome) %>% 
  tally() %>%
  mutate(Percent = round(n/sum(n)*100,2))
ggplot(data = byHourSet, 
       aes(x = Hour, y = Percent, color = Outcome, fill = Outcome)) +
  geom_bar(stat = "identity", position = "stack", width = .5) + 
  scale_fill_brewer(palette = "Blues") +
  scale_color_brewer(palette = "Blues") +
  theme_bw()

#' 
#' Any indication of a relationship?
#' We already know that there will missing data for fatal accidents:
#' 
## ----echo = T------------------------------------------------------------
byHourSet <- dataSet %>%
  filter(!(Outcome == 'Death')) %>% 
  group_by(Hour, Outcome) %>% 
  tally() %>%
  spread(key = Outcome, value = n) %>% 
  ungroup() %>% dplyr::select(-Hour)
  chisq.test(byHourSet, 
             simulate.p.value = T)

#' 
#' Hm.
#' 
## ----echo = T------------------------------------------------------------
byHourSet <- dataSet %>%
  group_by(Hour, Outcome) %>% 
  tally() %>%
  mutate(Percent = round(n/sum(n)*100,2)) %>%
  filter(Outcome == 'Injury')
ggplot(data = byHourSet, 
       aes(x = as.numeric(Hour), y = Percent)) +
  geom_path(color = 'blue') +
  geom_point(size = 1.5, color = 'blue') +
  geom_point(size = 1, color = 'white') +
  xlab('Hour') + ylim(20,35) +
  ggtitle('Percent of Accidents w. Injuries per Hour') +
  scale_x_continuous(breaks = as.numeric(byHourSet$Hour),
                     labels = as.character(0:23)) +
  theme_bw()

#' 
#' Let's inspect the distribution of traffic accidents per day:
#' 
## ----echo = T------------------------------------------------------------
dataSet <- unite(data = dataSet,
                 col = Date,
                 Year, Month, Day,
                 sep = "-")

#' 
#' Check whether there are any days with no accidents that are not in December 2015. for which we have no data:
#' 
## ----echo = T------------------------------------------------------------
distData <- dataSet %>% 
  group_by(Date) %>%
  summarise(Frequency = n())
distData$Date <- as.Date(distData$Date)
dates2015 <- seq(as.Date("2015/1/1"), as.Date("2015/12/31"), "days")
w <- which(dates2015 %in% distData$Date)
obs <- rep(0, length(dates2015))
obs[w] <- distData$Frequency
distData <- data.frame(
  Date = dates2015,
  Frequency = obs)
length(which(distData$Frequency == 0)) # December data, right

#' 
## ----echo = T------------------------------------------------------------
which(distData$Frequency == 0) # December data, right

#' 
## ----echo = T------------------------------------------------------------
distData <- distData[distData$Frequency > 0, ]
ggplot(distData, aes(x = Frequency)) +
  geom_bar(stat = 'count', fill = "blue", alpha = .35, width = .5) +
  xlab("Number of Accidents Per Day") + ylab("Frequency") + 
  ggtitle('Belgrade, 2015: Traffic Accidents Daily Frequency') +
  theme_bw()

#' 
#' Could this be a Poisson distribution?
#' 
## ----echo = T------------------------------------------------------------
sampleStats <- lapply(1:10000, function(x) {
  l <- list()
  s <- sample(distData$Frequency, 50, replace = T)
  l$mean <- mean(s)
  l$var <- var(s)
  l
})
meanStats <- sapply(sampleStats, function(x) x$mean)
varStats <- sapply(sampleStats, function(x) x$var)
poissonFrame <- data.frame(mean = meanStats,
                           var = varStats)
ggplot(poissonFrame, aes(x = mean, y = var)) +
  geom_point(size = .1, color = "black") +
  geom_smooth(method = "lm", size = .25, se = T, color = "blue", alpha = .35) +
  theme_bw()


#' 
#' I don't think so.
#' 
#' ### 4. Data Enrichment
#' 
#' Is there anything that could help us predict the number of accidents per day? Here's a proposal: let' grab the weather data for Belgrade, 2015/01/01 - 2015/11/01, and see if there's anything potentially useful in the data set:  
#' 
#' (NOTE: `bgdWeather2015.csv` was obtained from the following lines that do not evaluate in this version of the notebook; the data are already prepared and stored locally.)
#' 
#' 
## ----echo = T, message = T, error = T------------------------------------
library(weatherData)
checkBGD <- checkSummarizedDataAvailability(station_id = 'LYBE', 
                                            start_date = '2015-01-01', 
                                            end_date = '2015-11-30',
                                            station_type = 'id')

#' 
#' After checking the reported URL in the browser, it turns out the data are there in fact:
#' 
## ----echo = T, eval = F--------------------------------------------------
## bgd2015URL <- 'https://www.wunderground.com/history/airport/LYBE/2015/1/1/CustomHistory.html?dayend=30&monthend=11&yearend=2015&req_city=NA&req_state=NA&req_statename=NA&format=1'
## bgdWeather2015 <- read.csv(bgd2015URL,
##                            header  = T,
##                            stringsAsFactors = F,
##                            check.names = F)
## # clean-up a bit:
## colnames(bgdWeather2015) <- gsub("<br />","", colnames(bgdWeather2015), fixed = T)
## bgdWeather2015$WindDirDegrees <- gsub("<br />","", bgdWeather2015$WindDirDegrees, fixed = T)

#' 
## ----echo = T, eval = F, error = F, results = 'asis'---------------------
## kable(head(bgdWeather2015))

#' 
#' Let's clean up this, `CloudCover` first:
#' 
## ----echo = T, eval = F--------------------------------------------------
## table(bgdWeather2015$CloudCover)

#' 
## ----echo = T, eval = F--------------------------------------------------
## bgdWeather2015$CloudCover[which(is.na(bgdWeather2015$CloudCover))] <- 0

#' 
#' `Events`:
#' 
## ----echo = T, eval = F--------------------------------------------------
## table(bgdWeather2015$Events)

#' 
## ----echo = T, eval = F--------------------------------------------------
## bgdWeather2015$Events[!grepl("^[[:alpha:]]", bgdWeather2015$Events)] <- 'None'

#' 
#' `Max Gust SpeedMPH` goes to `Yes/No`:
#' 
## ----echo = T, eval = F--------------------------------------------------
## table(bgdWeather2015$`Max Gust SpeedMPH`)

#' 
## ----echo = T, eval = F--------------------------------------------------
## bgdWeather2015$WindGust <- ifelse(is.na(bgdWeather2015$`Max Gust SpeedMPH`), 'No', 'Yes')

#' 
#' Fix the `CET` column to match our `Date` (`YYYY-MM-DD` format):
#' 
## ----echo = T, eval = F--------------------------------------------------
## bgdWeather2015$CET <- sapply(bgdWeather2015$CET, function(x) {
##   x <- strsplit(x, split = '-', fixed = T)
##   if (nchar(x[[1]][2])<2) {
##     x[[1]][2] <- paste0('0', x[[1]][2])
##   }
##   if (nchar(x[[1]][3])<2) {
##     x[[1]][3] <- paste0('0', x[[1]][3])
##   }
##   return(paste(unlist(x), collapse = "-"))
## })

#' 
#' And save:
#' 
## ----echo = T, eval = F--------------------------------------------------
## write.csv(bgdWeather2015, 'bgdWeather2015.csv')

#' 
#' (NOTE: loading `bgdWeather2015.csv` here). Now left join `bgdWeather2015` to `distData` and prepare for modeling:
#' 
## ----echo = T------------------------------------------------------------
bgdWeather2015 <- read.csv('bgdWeather2015.csv',
                           header = T,
                           stringsAsFactors = F,
                           check.names = F,
                           row.names = 1)
bgdWeather2015$CET <- as.Date(bgdWeather2015$CET)
modelSet <- left_join(distData, bgdWeather2015,
                      by = c("Date" = "CET"))
modelSet$`Max Gust SpeedMPH` <- NULL
modelSet$Events <- factor(modelSet$Events)
levels(modelSet$Events)

#' 
## ----echo = T------------------------------------------------------------
levels(modelSet$Events) <- c('None', setdiff(levels(modelSet$Events), 'None'))
levels(modelSet$WindGust) <- c('No', 'Yes')

#' 
#' Remove spaces in column names:
#' 
## ----echo = T------------------------------------------------------------
colnames(modelSet) <- gsub("\\s+", "", colnames(modelSet))

#' 
#' Fix `$WindDirDegrees` which is a `character`:
#' 
## ----echo = T------------------------------------------------------------
modelSet$WindDirDegrees <- as.numeric(modelSet$WindDirDegrees)

#' 
#' and save:
#' 
## ----echo = T------------------------------------------------------------
write.csv(modelSet, 'modelDataSet.csv')

#' 
#' ### 5. Modeling
#' 
#' Add some new features:
#' 
## ----echo = T------------------------------------------------------------
modelSet$DaySeverity <- cut(modelSet$Frequency,
                            breaks = quantile(modelSet$Frequency),
                            right = T,
                            include.lowest = T,
                            labels = as.numeric(1:4))
table(modelSet$DaySeverity)

#' 
#' `modelSet$DaySeverity` becomes an ordered factor:
#' 
## ----echo = T------------------------------------------------------------
modelSet$DaySeverity <- factor(modelSet$DaySeverity, ordered = T)
tail(modelSet$DaySeverity)

#' 
#' Introduce `DayofWeek` feature:
#' 
## ----echo = T------------------------------------------------------------
modelSet$DayofWeek <- factor(weekdays(modelSet$Date))
levels(modelSet$DayofWeek) <- c('Monday','Tuesday','Wednesday',
                                'Thursday', 'Friday', 'Saturday',
                                'Sunday')

#' 
#' Inspect the effect of `DayofWeek`:
#' 
## ----echo = T, results = 'asis'------------------------------------------
dayFrame <- modelSet %>% 
  group_by(DayofWeek) %>%
  summarise(Frequency = sum(Frequency)) %>% 
  arrange(desc(Frequency))
kable(dayFrame)

#' 
#' Relevel `DayofWeek`:
#' 
## ----echo = T------------------------------------------------------------
modelSet$DayofWeek <- factor(modelSet$DayofWeek, 
                             levels = dayFrame$DayofWeek[order(dayFrame$Frequency)])
levels(modelSet$DayofWeek)

#' 
#' 
#' Inspect `Events`:
#' 
## ----echo = T------------------------------------------------------------
eventFrame <- modelSet %>%
  group_by(Events) %>%
  summarise(Frequency = sum(Frequency)) %>% 
  arrange(desc(Frequency))
kable(eventFrame)

#' 
#' Relevel `Events`:
#' 
## ----echo = T------------------------------------------------------------
modelSet$Events <- factor(modelSet$Events,
                          levels = eventFrame$Events[order(eventFrame$Frequency)])
levels(modelSet$Events)

#' 
#' 
#' #### 5A. GLM: Negative Binomial Regression: Accident Frequency
#' 
## ----echo = T, results = 'asis'------------------------------------------
# - model: Poisson GLM
mData <- modelSet %>% 
  dplyr::select(-Date, -starts_with("Max"), -starts_with("Min"), -DaySeverity)
mData %>% 
  group_by(DayofWeek) %>%
  summarise(Mean = round(mean(Frequency),2), 
            Var = round(var(Frequency),2)) %>%
  kable()

#' 
#' Overdispersion present; use Negative Binomial Regression in place of Poisson for count data:
#' 
## ----echo = T------------------------------------------------------------
library(MASS)
mFit <- glm.nb(Frequency ~ .,
               data = mData)
# - inspect model
summary(mFit)

#' 
#' Predict:
#' 
## ----echo = T------------------------------------------------------------
predictFrame <- data.frame(Observation = rep(1:length(mData$Frequency), 2),
                           Value = c(predict(mFit, type = "response"),
                                     mData$Frequency),
                           Type = c(rep("Predicted", length(mData$Frequency)),
                                    rep("Observed", length(mData$Frequency))),
                           stringsAsFactors = F)
ggplot(predictFrame, aes(x = Observation, y = Value, group = Type, color = Type)) + 
  geom_line(size = .15) + 
  scale_color_manual(values = c("lightblue", "black")) +
  theme_bw() + 
  theme(legend.key = element_blank()) +
  theme(legend.title = element_blank()) +
  ggtitle("GLM Negative Binomial: Predicted vs. Observed")
  

#' 
## ----echo = T------------------------------------------------------------
predictFrame <- spread(predictFrame,
                       key = Type,
                       value = Value)
ggplot(predictFrame, aes(x = Predicted, y = Observed)) + 
  geom_point(size = .5, color = "blue") + 
  theme_bw() + 
  ggtitle("GLM Negative Binomial: Predicted vs. Observed")

#' 
## ----echo = T------------------------------------------------------------
# - coefficients
mCoeff <- as.data.frame(summary(mFit)$coefficients)
w <- which(mCoeff$`Pr(>|z|)` < .05)
rownames(mCoeff)[w]

#' 
## ----echo = T, results = 'asis'------------------------------------------
w <- setdiff(w, 1) # - supress the intercept from output
important <- data.frame(predictor = rownames(mCoeff)[w],
                        coefficient = round(exp(mCoeff$Estimate)[w], 2),
                        stringsAsFactors = F)
kable(important[order(-important$coefficient), ])

#' 
#' 
#' #### 5B. Ordinal Logistic Regression: Day Severity (i.e. Frequency categorized)
#' 
## ----echo = T------------------------------------------------------------
mData <- modelSet %>% 
  dplyr::select(-Date, -starts_with("Max"), -starts_with("Min"), -Frequency)
library(ordinal)
# - model
mFit <- clm(DaySeverity ~ .,
            data = mData)
# - inspect model
summary(mFit)

#' 
#' Refine:
#' 
## ----echo = T------------------------------------------------------------
mCoeff <- as.data.frame(summary(mFit)$coefficients)
w <- which(mCoeff$`Pr(>|z|)` < .05)
w <- setdiff(w, 1:3) # supress the intercepts from the output
rownames(mCoeff)[w]

#' 
## ----echo = T------------------------------------------------------------
mData <- modelSet %>% 
  dplyr::select(DaySeverity, MeanWindSpeedMPH, CloudCover, DayofWeek)
# - model
mFit <- clm(DaySeverity ~ .,
            data = mData)
# - inspect model
summary(mFit)

#' 
## ----echo = T, results = 'asis'------------------------------------------
mCoeff <- as.data.frame(summary(mFit)$coefficients)
w <- which(mCoeff$`Pr(>|z|)` < .05)
w <- setdiff(w, 1:3) # - supress the intercepts from output
important <- data.frame(predictor = rownames(mCoeff)[w],
                        coefficient = round(exp(mCoeff$Estimate)[w], 2),
                        stringsAsFactors = F)
kable(important[order(-important$coefficient), ])

#' 
## ----echo = T------------------------------------------------------------
# - e.g. Hit rate (Accuracy of Classification)
countHits <- sum(
  as.numeric(predict(mFit, type = "class")$fit) == as.numeric(modelSet$DaySeverity))
dataPoints <- length(modelSet$DaySeverity)
hitRate <- countHits/dataPoints
paste0("Correct classification rate: ", round(hitRate*100, 2), "%")

#' 
#' 
#' ***
#' 
#' ### 6. Air vs Road Transportation Safety
#' 
#' According to the [Aviation Safety Network](https://aviation-safety.net/)'s data, there were **16 fatal** airliner accidents, and **560 airliner accidents in total** in 2015. [Source: Aviation Safety Network](https://aviation-safety.net/graphics/infographics/ASN_infographic_2015.jpg).
#' 
#' How many air carrier departures took place in 2015? We will use the {wbstats} package to access the [World Data Bank](http://databank.worldbank.org/data/home.aspx) to find out. The primary data source for this indicator is the [International Civil Aviation Organization](http://www.icao.int/Pages/default.aspx); the indicator can be accessed from [World Data Bank](http://databank.worldbank.org/data/reports.aspx?source=2&type=metadata&series=IS.AIR.DPRT). The indicator code in World Data Bank is: **IS.AIR.DPRT** (Description: *Registered carrier departures worldwide are domestic takeoffs and takeoffs abroad of air carriers registered in the country.*).
#' 
## ----echo = T, results = 'asis'------------------------------------------
# Indicator: IS.AIR.DPRT
# - access World Data Bank
library(wbstats)
airDeps <- data.frame(wb(
  indicator = 'IS.AIR.DPRT'
))
kable(head(airDeps))

#' 
#' Not all codes in the `iso2c` field represent world countries, meaning: the data must be overlapping:
#' 
## ----echo = T------------------------------------------------------------
unique(airDeps$iso2c)

#' 
#' We need to extract only country data; load the {ISOcodes} package to access ISO3166 country codes:
#' 
## ----echo = T------------------------------------------------------------
library(ISOcodes)
data("ISO_3166_1")
head(ISO_3166_1$Alpha_2, 30)

#' 
#' Extract only country data from `airDeps`:
#' 
## ----echo = T, results = 'asis'------------------------------------------
totFlights <- airDeps %>%
  filter(iso2c %in% ISO_3166_1$Alpha_2, date == '2015')
print(paste0('Countries: ', length(unique(totFlights$iso2c))))

#' 
#' Let's have a look at the data set now:
#' 
## ----echo = T, results = 'asis'------------------------------------------
kable(head(totFlights, 20))

#' 
#' Good. How many flights there were in 2015?
#' 
## ----echo = T------------------------------------------------------------
sum(totFlights$value)

#' 
#' This number corresponds with the World Data Bank report on the `IS.AIR.DPRT` indicator: [check out](http://data.worldbank.org/indicator/IS.AIR.DPRT).
#' 
#' What was the probability of boarding a flight that will end in a *fatal accident* in 2015? Let's see: there were 16 flights (Aviation Safety Network data) that had a fatal outcome in 2015, and 32960402 flights in total (World Bank Data):
#' 
## ----echo = T------------------------------------------------------------
pAirFatal <- 16/32960402
pAirFatal

#' 
## ----echo = T------------------------------------------------------------
print(paste0("Percent Fatal: ", pAirFatal*100))

#' 
#' 
#' *** 
#' 
#' [Goran S. Milovanovic](http//www.exactness.net), [Data Science Serbia](http//www.datascience.rs), 01/24/2017, Belgrade, Serbia
#' 
#' ![](../img/DataScienceSerbia_Logo.png)
#' 
#' ***
