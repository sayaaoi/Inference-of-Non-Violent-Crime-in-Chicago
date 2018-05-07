Data\_Explore
================
Yuejun Wu
4/21/2018

``` r
# read in data
library(readr)
library(zoo)
```

    ## 
    ## Attaching package: 'zoo'

    ## The following objects are masked from 'package:base':
    ## 
    ##     as.Date, as.Date.numeric

``` r
cm_data <- read_csv("crime_data_full.csv")
```

    ## Warning: Missing column names filled in: 'X1' [1]

    ## Parsed with column specification:
    ## cols(
    ##   X1 = col_integer(),
    ##   `Unemployment Rate` = col_double(),
    ##   Year = col_integer(),
    ##   Month = col_integer(),
    ##   temperature = col_double(),
    ##   crime_count = col_integer(),
    ##   AQI = col_double()
    ## )

``` r
attach(cm_data)
```

``` r
cm_data <- cm_data[-1]
cm_data$date <- paste(cm_data$Year, cm_data$Month, sep="-")
cm_data$date <- as.yearmon(cm_data$date)
```

``` r
# Summary statistics
cm_data$Year <- factor(cm_data$Year)
levels(cm_data$Year) <- c("2008","2009","2010","2011")

cm_data$Month <- factor(cm_data$Month)
levels(cm_data$Month) <- c(1:12)
summary(cm_data[1:6])
```

    ##  Unemployment Rate   Year        Month     temperature     crime_count   
    ##  Min.   : 5.400    2008:12   1      : 4   Min.   :16.61   Min.   :13217  
    ##  1st Qu.: 8.075    2009:12   2      : 4   1st Qu.:35.35   1st Qu.:21074  
    ##  Median :10.050    2010:12   3      : 4   Median :51.58   Median :37866  
    ##  Mean   : 9.287    2011:12   4      : 4   Mean   :50.17   Mean   :33691  
    ##  3rd Qu.:10.500              5      : 4   3rd Qu.:66.42   3rd Qu.:41240  
    ##  Max.   :11.400              6      : 4   Max.   :79.09   Max.   :49382  
    ##                              (Other):24                                  
    ##       AQI       
    ##  Min.   :33.69  
    ##  1st Qu.:40.30  
    ##  Median :44.56  
    ##  Mean   :45.19  
    ##  3rd Qu.:49.57  
    ##  Max.   :58.38  
    ## 

``` r
# Coveriance of variables
pairs(cm_data[,c(1,4,5,6)])
```

![](Data%20Explore1_figs/Data%20exploration-unnamed-chunk-4-1.png)

``` r
cor(cm_data[,c(1,4,5,6)])
```

    ##                   Unemployment Rate temperature crime_count         AQI
    ## Unemployment Rate        1.00000000   0.1059765  -0.3768388 -0.03971818
    ## temperature              0.10597651   1.0000000   0.3546377 -0.28385183
    ## crime_count             -0.37683878   0.3546377   1.0000000 -0.18509405
    ## AQI                     -0.03971818  -0.2838518  -0.1850940  1.00000000

``` r
library(ggplot2)
plot(x = cm_data$date, y = cm_data$crime_count, main = "Crime Counts Over Time", xlab = "datetime", ylab = "Number of Non-violent Crime")
```

![](Data%20Explore1_figs/Data%20exploration-unnamed-chunk-5-1.png)

``` r
plot(x = Month, y = crime_count, ylab = "Number of Non-violent Crime", main = "Crime Counts by Month")
```

![](Data%20Explore1_figs/Data%20exploration-unnamed-chunk-5-2.png)

``` r
# boxplot by year
plot(crime_count ~ as.factor(Year), ylab = "Number of Non-violent Crime", xlab = "Year")
```

![](Data%20Explore1_figs/Data%20exploration-unnamed-chunk-5-3.png)

``` r
# draw line plot for temperature
# convert factor to numeric for convenience 
cm_data$temp <- as.numeric(cm_data$Year) 
nYear <- max(cm_data$temp)

# get the range for the x and y axis 
xrange <- range(cm_data$temperature) 
yrange <- range(cm_data$crime_count) 

# set up the plot 
plot(xrange, yrange, type="n", xlab="Temperature(°F)",
    ylab="Number of Non-violent Crime" ) 
colors <- rainbow(nYear) 
linetype <- c(1:nYear) 
plotchar <- seq(18,18+nYear,1)

# add lines 
for (i in 1:nYear) { 
  year <- subset(cm_data, temp==i) 
  lines(year$temperature, year$crime_count, type="b", lwd=1.5,
    lty=linetype[i], col=colors[i], pch=plotchar[i]) 
} 

# add a title and subtitle 
title("Number of Non-violent Crime with increasing temperature")

# add a legend 
legend(xrange[1], yrange[2], legend = c("2008","2009","2010","2011"), cex=0.8, col=colors,
    pch=plotchar, lty=linetype, title="Year")
```

![](Data%20Explore1_figs/Data%20exploration-unnamed-chunk-5-4.png)

``` r
plot(x = cm_data$date, y = cm_data$temperature, xlab = "datetime", ylab = "Temperature(°F)", main = "Temperature by datetime")
```

![](Data%20Explore1_figs/Data%20exploration-unnamed-chunk-6-1.png)

``` r
# plot for unemployment rate
# convert factor to numeric for convenience 
cm_data$temp <- as.numeric(cm_data$Year) 
nYear <- max(cm_data$temp)

# get the range for the x and y axis 
xrange <- range(cm_data$`Unemployment Rate`[cm_data$Year == "2008"]) 
yrange <- range(cm_data$crime_count) 

# set up the plot 
plot(xrange, yrange, type="n", xlab="Unemployment Rate(%)",
    ylab="Number of Non-violent Crime" ) 
colors <- rainbow(nYear) 
plotchar <- seq(18,18+nYear,1)

# add lines 
# for (i in 1:nYear) { 
  year <- subset(cm_data, temp==1) 
  lines(year$`Unemployment Rate`, year$crime_count, type="b", lwd=1.5,
    lty=linetype[1], col=colors[1], pch=plotchar[1]) 
# } 

# add a title and subtitle 
title("Number of Non-violent Crime by unemployment rate")

# add a legend 
legend(xrange[1], yrange[1]+7000, legend = "2008", cex=0.8, col=colors,
    pch=plotchar, lty=linetype, title="Year")
```

![](Data%20Explore1_figs/Data%20exploration-unnamed-chunk-7-1.png)

``` r
# convert factor to numeric for convenience 
cm_data$temp <- as.numeric(cm_data$Year) 
nYear <- max(cm_data$temp)

# get the range for the x and y axis 
xrange <- range(cm_data$`Unemployment Rate`[cm_data$Year == "2009"]) 
yrange <- range(cm_data$crime_count) 

# set up the plot 
plot(xrange, yrange, type="n", xlab="Unemployment Rate(%)",
    ylab="Number of Non-violent Crime" ) 
colors <- rainbow(nYear) 
plotchar <- seq(18,18+nYear,1)

# add lines 
year <- subset(cm_data, temp==2) 
lines(year$`Unemployment Rate`, year$crime_count, type="b", lwd=1.5,
  lty=linetype[2], col=colors[2], pch=plotchar[2]) 

# add a title and subtitle 
title("Number of Non-violent Crime by unemployment rate")

# add a legend 
legend(xrange[1], yrange[1]+7000, legend = "2009", cex=0.8, col=colors[2],
    pch=plotchar, lty=linetype, title="Year")
```

![](Data%20Explore1_figs/Data%20exploration-unnamed-chunk-8-1.png)

``` r
# convert factor to numeric for convenience 
cm_data$temp <- as.numeric(cm_data$Year) 
nYear <- max(cm_data$temp)

# get the range for the x and y axis 
xrange <- range(cm_data$`Unemployment Rate`[cm_data$Year == "2010"]) 
yrange <- range(cm_data$crime_count) 

# set up the plot 
plot(xrange, yrange, type="n", xlab="Unemployment Rate(%)",
    ylab="Number of Non-violent Crime" ) 
colors <- rainbow(nYear) 
plotchar <- seq(18,18+nYear,1)

# add lines 
year <- subset(cm_data, temp==3) 
lines(year$`Unemployment Rate`, year$crime_count, type="b", lwd=1.5,
  lty=linetype[3], col=colors[3], pch=plotchar[3]) 

# add a title and subtitle 
title("Number of Non-violent Crime by unemployment rate")

# add a legend 
legend(xrange[1], yrange[2], legend = "2010", cex=0.8, col=colors[3],
    pch=plotchar, lty=linetype, title="Year")
```

![](Data%20Explore1_figs/Data%20exploration-unnamed-chunk-9-1.png)

``` r
# convert factor to numeric for convenience 
cm_data$temp <- as.numeric(cm_data$Year) 
nYear <- max(cm_data$temp)

# get the range for the x and y axis 
xrange <- range(cm_data$`Unemployment Rate`[cm_data$Year == "2011"]) 
yrange <- range(cm_data$crime_count) 

# set up the plot 
plot(xrange, yrange, type="n", xlab="Unemployment Rate(%)",
    ylab="Number of Non-violent Crime" ) 
colors <- rainbow(nYear) 
plotchar <- seq(18,18+nYear,1)

# add lines 
year <- subset(cm_data, temp==4) 
lines(year$`Unemployment Rate`, year$crime_count, type="b", lwd=1.5,
  lty=linetype[4], col=colors[4], pch=plotchar[4]) 

# add a title and subtitle 
title("Number of Non-violent Crime by unemployment rate")

# add a legend 
legend(xrange[1], yrange[2], legend = "2011", cex=0.8, col=colors[4],
    pch=plotchar, lty=linetype, title="Year")
```

![](Data%20Explore1_figs/Data%20exploration-unnamed-chunk-10-1.png)

``` r
# plot for AQI
# convert factor to numeric for convenience 
cm_data$temp <- as.numeric(cm_data$Year) 
nYear <- max(cm_data$temp)

# get the range for the x and y axis 
xrange <- range(cm_data$AQI) 
yrange <- range(cm_data$crime_count) 

# set up the plot 
plot(xrange, yrange, type="n", xlab="Air Quality Index",
    ylab="Number of Non-violent Crime" ) 
colors <- rainbow(nYear) 
linetype <- c(1:nYear) 
plotchar <- seq(18,18+nYear,1)

# add lines 
for (i in 1:nYear) { 
  year <- subset(cm_data, temp==i) 
  lines(year$AQI, year$crime_count, type="b", lwd=1.5,
    lty=linetype[i], col=colors[i], pch=plotchar[i]) 
} 

# add a title and subtitle 
title("Number of Non-violent Crime with increasing Air Quality Index")

# add a legend 
legend(xrange[1], yrange[1]+15000, legend = c("2008","2009","2010","2011"), cex=0.8, col=colors,
    pch=plotchar, lty=linetype, title="Year")
```

![](Data%20Explore1_figs/Data%20exploration-unnamed-chunk-11-1.png)

``` r
cm_data$month_num <- as.numeric(cm_data$Month)

mod <- lm(crime_count~ `Unemployment Rate`+Year+temperature + AQI, data = cm_data)
summary(mod)
```

    ## 
    ## Call:
    ## lm(formula = crime_count ~ `Unemployment Rate` + Year + temperature + 
    ##     AQI, data = cm_data)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -12748  -1100   -290   1171   5231 
    ## 
    ## Coefficients:
    ##                      Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)          37891.73    5771.67   6.565 6.80e-08 ***
    ## `Unemployment Rate`    -83.89     653.20  -0.128   0.8984    
    ## Year2009             -3226.84    2869.32  -1.125   0.2673    
    ## Year2010             -6700.23    3101.31  -2.160   0.0366 *  
    ## Year2011            -23994.53    2818.31  -8.514 1.32e-10 ***
    ## temperature            208.78      23.97   8.711 7.15e-11 ***
    ## AQI                   -119.81      71.16  -1.684   0.0998 .  
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 2889 on 41 degrees of freedom
    ## Multiple R-squared:  0.9344, Adjusted R-squared:  0.9248 
    ## F-statistic: 97.36 on 6 and 41 DF,  p-value: < 2.2e-16

``` r
library(faraway)
vif(mod)
```

    ## `Unemployment Rate`            Year2009            Year2010 
    ##            8.897093            8.880263           10.374251 
    ##            Year2011         temperature                 AQI 
    ##            8.567331            1.121910            1.127391

``` r
par(mfrow = c(2,2))
plot(mod)
```

![](Data%20Explore1_figs/Data%20exploration-unnamed-chunk-13-1.png)
