Data preparation
================
Yuejun Wu
4/18/2018

``` r
library(lubridate)
```

    ## 
    ## Attaching package: 'lubridate'

    ## The following object is masked from 'package:base':
    ## 
    ##     date

``` r
library(readr)
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:lubridate':
    ## 
    ##     intersect, setdiff, union

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

read raw data in w/ columns needed
==================================

``` r
cm.data <- read.csv("data/Chicago_Crimes_2008_to_2011.csv")[,c("Date","Primary.Type","Year")]
```

``` r
str(cm.data)
```

    ## 'data.frame':    2688712 obs. of  3 variables:
    ##  $ Date        : Factor w/ 572371 levels "01/01/2008 01:00:00 AM",..: 447623 450354 450526 451886 452339 452119 453965 453841 453816 455244 ...
    ##  $ Primary.Type: Factor w/ 30 levels "1134","ARSON",..: 11 11 11 11 11 11 11 11 11 11 ...
    ##  $ Year        : int  2008 2008 2008 2008 2008 2008 2008 2008 2008 2008 ...

read in weather data
====================

``` r
weather_08_10 <- read_csv("data/08.1.1_10.12.31weather.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   STATION = col_character(),
    ##   NAME = col_character(),
    ##   DATE = col_date(format = ""),
    ##   AWND = col_double(),
    ##   PRCP = col_double(),
    ##   TAVG = col_character(),
    ##   TMAX = col_integer(),
    ##   TMIN = col_integer()
    ## )

``` r
weather_11<-read_csv("data/11.1.1_11.12.31weather.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   STATION = col_character(),
    ##   NAME = col_character(),
    ##   DATE = col_date(format = ""),
    ##   AWND = col_double(),
    ##   PRCP = col_double(),
    ##   TAVG = col_character(),
    ##   TMAX = col_integer(),
    ##   TMIN = col_integer()
    ## )

``` r
# combine two dataset together to prepare for combine with crime data
weather.data <- merge(weather_08_10, weather_11,all = TRUE)
```

``` r
weather.data$TAVG = (weather.data$TMAX + weather.data$TMIN)/2
```

``` r
# process data format in cm.data

# sapply(weather.data$DATE, class) know what data type 
cm.temp<-as.POSIXct(cm.data$Date,format="%m/%d/%Y %I:%M:%S %p",tz=Sys.timezone())
cm.data$Date <- cm.temp

cm.data <-
  cm.data %>%
  mutate(Year  = factor(year(Date), levels=2008:2011),
         Month = factor(month(Date), levels=1:12),
         #Hour  = factor(hour(Date), levels=0:23),
         Day = day(Date),
         dayDate = as.POSIXct(round(Date, units = "days"))
  )
```

``` r
# convert character vectors to factors

cm.data$Primary.Type <- as.factor(cm.data$Primary.Type)
unique(cm.data$Primary.Type)
```

    ##  [1] HOMICIDE                         SEX OFFENSE                     
    ##  [3] BATTERY                          DECEPTIVE PRACTICE              
    ##  [5] CRIM SEXUAL ASSAULT              THEFT                           
    ##  [7] MOTOR VEHICLE THEFT              CRIMINAL DAMAGE                 
    ##  [9] NARCOTICS                        OTHER OFFENSE                   
    ## [11] PROSTITUTION                     BURGLARY                        
    ## [13] OFFENSE INVOLVING CHILDREN       ROBBERY                         
    ## [15] ASSAULT                          CRIMINAL TRESPASS               
    ## [17] PUBLIC PEACE VIOLATION           WEAPONS VIOLATION               
    ## [19] KIDNAPPING                       GAMBLING                        
    ## [21] LIQUOR LAW VIOLATION             INTERFERENCE WITH PUBLIC OFFICER
    ## [23] ARSON                            STALKING                        
    ## [25] INTIMIDATION                     OBSCENITY                       
    ## [27] NON-CRIMINAL                     PUBLIC INDECENCY                
    ## [29] OTHER NARCOTIC VIOLATION         1134                            
    ## 30 Levels: 1134 ARSON ASSAULT BATTERY BURGLARY ... WEAPONS VIOLATION

``` r
# delete rows based on columns, only need non-violent crime
attach(cm.data)
crime_data <- cm.data[which(Primary.Type == "THEFT"| Primary.Type == "NON-CRIMINAL"| Primary.Type == "MOTOR VEHICLE THEFT" | Primary.Type == "DECEPTIVE PRACTICE" | Primary.Type == "BURGLARY" | Primary.Type == "PUBLIC PEACE VIOLATION" | Primary.Type == "INTERFERENCE WITH PUBLIC OFFICER" | Primary.Type == "PUBLIC INDECENCY" | Primary.Type == "INTIMIDATION" | Primary.Type == "BATTERY" | Primary.Type == "ASSAULT"| Primary.Type == "OBSCENITY"),]
detach(cm.data)
```

``` r
# change column name
colnames(weather.data)[3] <- "dayDate"

#remove unnecessary columns
weather.data <- weather.data[c(-1,-2,-7,-8)]
```

``` r
# change data type, prepare for merge
crime_data$dayDate <- as.Date(crime_data$dayDate)
```

``` r
# join two files together based on date
crime_data <- left_join(crime_data,weather.data,by = "dayDate")
crime_data <- crime_data[-7]
```

``` r
# export csv 
#write.csv(crime_data, file = "crime_data.csv")
```
