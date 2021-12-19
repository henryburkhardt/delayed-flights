# delayed-flights
Will my flight be delayed? Built with R (the statistical programing language), this app predicts the delay of a flight between any two US cities.

<i>A note about the data: the data used in this project is publicly available from the [Bureau of Transportation Statistics website](https://www.bts.gov/topics/airlines-airports-and-aviation). I used only data from the first month of January 2017 because I didn't have the capacity to work with larger CSV files. However, this code is totally scalable to work with data from a full year, or even multiple years to give more accurate predictions. </i>

## Introduction

This markdown file has two purposes: (1) creating a useful dataframe
from the raw data imported below and functions to query that data and
(2) creating map visualizations from this data. The second file in this
directory, “app.R” is a user interface that uses the functions from this
markdown file and returns their results to a Shiny App.

*A note on the UI: The user interface built in ./app.R relies on
variables and functions that are defined in this markdown document. All
the chunks in this markdown must be run before app.R will run properly.*

The user interface ([app.R](./app.R)) looks like this:
![](plots/UiExample.jpg)

If the code below is not of interest, be sure to at least check out the [super cool visualizations](#ggplot-object-setup) generated from it!

``` r
#installing packages
library(readr)
library(tidyverse)
library(ggplot2)
library(ggmap)
library(maps)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

#set up API key to acces the Google geocoding service
register_google(key ="AIzaSyDuUOPyEGmFroTmJtNCv5VLXM3YPeloagQ") 

#reading flight data 
fd <- read_csv("./data_sets/2017_Flight_data.csv")

#getting previoulsy compiled geodata
if (file.exists("./data_sets/geo_lookup_city.csv")){
  geo_lookup_city <- read_csv("./data_sets/geo_lookup_city.csv")
}
```

## Creating a More Useful Dataframe

The goal of this new dataframe is to compute summary statistics for
every single flight in the 2017_flight_data.csv file. The original file
has over 400,000 rows (one for every flight in the one month period of
the dataframe), the new dataframe will have one row for every flight
between two cities. For example, every flight between Minneapolis, MN
and Denver, CO in the original data will be condensed into one row, and
the selected statistics will be summarized into the single row. Thus a
probability and average delay time can be assigned to every flight. This
new dataframe is called `fd_heatmap_group` (it’s purpose was originally
to create a map of each unique flight path).

### Creating New Dataframe and Grouping by `uniqueID`

``` r
#selecting the variable I want in the new DF
fd_heatmap <- select(fd,DepDelayMinutes,OriginCityName, DestCityName, OriginAirportID, DestAirportID, Cancelled, DepDel15, DepDelay, Cancelled, UniqueCarrier)

#creating a unique id number for every flight between two specific cities
fd_heatmap$uniqueID <- as.numeric(paste(fd_heatmap$OriginAirportID, fd_heatmap$DestAirportID, sep = ''))

#combining and summarizing the data using the tidyverse group_by() method
fd_heatmap_group <- fd_heatmap %>% group_by(uniqueID) %>% 
  summarise(
    avgDelay = mean(DepDelayMinutes, na.rm  = TRUE), #the average minutes of delay 
    OriginCityName = first(OriginCityName), #Origin City name 
    DestCityName = first(DestCityName),#Destination city name
    quantity = n(), # the number of flights in each group  
    noDelay = length(which(cur_data()$DepDelayMinutes==0)),#amount not delayed
    delay15 = sum(cur_data()$DepDel15, na.rm = TRUE)#amount delayed 15+ minutes
  )

#converting decimal to percent strings. this becomes important later when we want to return a probablity to the user in the form of a percent charecter string.
fd_heatmap_group$delayOdds <- paste(round((1-(fd_heatmap_group$noDelay/fd_heatmap_group$quantity))*100), "%", sep = "")
fd_heatmap_group$delayOddsRaw <- (fd_heatmap_group$noDelay/fd_heatmap_group$quantity)
fd_heatmap_group$delay15Odds <- paste(round((fd_heatmap_group$delay15/fd_heatmap_group$quantity)*100, 0), "%", sep = "")
fd_heatmap_group$popularity <- fd_heatmap_group$quantity/max(fd_heatmap_group$quantity)
```

### Assigning Lon/Lat Coordinates to Every City

Because we eventually want to be creating maps of flights, the each
airport defined in the `fd` data needs to be assigned a latitude and
longitude. This will allow us to progamatically draw as many plane
flights as we want. Google’s Geocoding API was selected as the best
method. The for loop below calls the API (linked with my Google Cloud
account) on all of the unique cities in the `fd` data.

``` r
#this chunk should only be run as nesecary. A csv file is saved after the first run
allCities <- unique(c(fd_heatmap$OriginCityName, fd_heatmap$DestCityName))

lat <- c()
lon <- c()

for (i in 1:length(allCities)) {
  geocode <- geocode(allCities[i]) #calls google API
  lon[i] <- as.numeric(geocode[2])
  lat[i] <- as.numeric(geocode[1])
}

geo_lookup_city <- as.data.frame(cbind(allCities, lon, lat)) 

#Fixing city names for the two instacnes that the Google API could not locate. 
geo_lookup_city[geo_lookup_city$allCities == "Montrose/Delta, CO", "lon"] <- as.numeric(geocode("Montrose, CO")[2])
geo_lookup_city[geo_lookup_city$allCities == "Montrose/Delta, CO", "lat"] <- as.numeric(geocode("Montrose, CO")[1])

geo_lookup_city[geo_lookup_city$allCities == "Guam, TT", "lon"] <- as.numeric(geocode("Guam")[2])
geo_lookup_city[geo_lookup_city$allCities == "GUam, TT", "lat"] <- as.numeric(geocode("Guam")[1])

write_csv(geo_lookup_city, file="./geo_lookup_city.csv") 
#api can be slow so results are saved as a csv, doesn't have to be run everytime
```

### Assigning Lat/Lon Coordinates to Every Flight

Each city in `fd_heatmap_group` is assigned its corresponding lat and
lon from the previously generated `geo_lookup_city` .

``` r
fd_heatmap_group$orgin_lon <- geo_lookup_city$lon[match(fd_heatmap_group$OriginCityName, geo_lookup_city$allCities)]
fd_heatmap_group$orgin_lat <- geo_lookup_city$lat[match(fd_heatmap_group$OriginCityName, geo_lookup_city$allCities)]

fd_heatmap_group$destination_lon <- geo_lookup_city$lon[match(fd_heatmap_group$DestCityName, geo_lookup_city$allCities)]
fd_heatmap_group$destination_lat <- geo_lookup_city$lat[match(fd_heatmap_group$DestCityName, geo_lookup_city$allCities)]
```

### Creating a Subset for Continental US Flights

Now that the geodata of each city is known, `fd_heatmap_group` can be
queried geographically. The data is subsested geographically here so
that we can plot map objects that only include the contenital us. The
data is compared against a “bounding box” of lat/lon coordinates around
the us.

``` r
#Cordinate bounding box for us lon = 25, 49 / lat = -126, -66
fd_heatmap_group_continetnal <- fd_heatmap_group[
  between(fd_heatmap_group$orgin_lon, 25,49)&
  between(fd_heatmap_group$orgin_lat, -126,-66)&
  between(fd_heatmap_group$destination_lon, 25,49)&
  between(fd_heatmap_group$destination_lat, -126,-66),]
```

## Creating Functions

### flightUi()

This first function is the back bone of making this data useful. It’s
meant to be used in the command line and prompts the user to enter a
departure and arrival city name. It checks to make sure both inputs
exist in the data, and then returns the row in `fd_heatmap_group` that
corresponds to the given flight path.

``` r
#type flightUi() into the command line after running this chunk to test it out.

flightUi <- function(){
  deptCity <- readline(prompt="DEPARTURE CITY: ")
  if (!(deptCity %in% fd_heatmap_group$OriginCityName)){
    cat("city not found")
    return()
    }

  arrvCity <- readline(prompt="ARRIVAL CITY: ")
  if (!(arrvCity %in% fd_heatmap_group$DestCityName)){
    cat("city not found")
    return()
  }
  
  tripData <- fd_heatmap_group[(fd_heatmap_group$DestCityName==arrvCity)&(fd_heatmap_group$OriginCityName==deptCity),]
  
  return(tripData)
}
```

### flightUi2()

This function is almost identical to `flightUi()` except that it takes
the departure and arrival city as inputs to the function instead of
gathering input through the command line. This function becomes
incredible useful in the Shiny app, when our goal is to operate on
inputs provided by a user through the UI.

``` r
flightUi2 <- function(deptCity,arrvCity){
  if (!(deptCity %in% fd_heatmap_group$OriginCityName)){
    cat("city not found")
    return()
    }
  if (!(arrvCity %in% fd_heatmap_group$DestCityName)){
    cat("city not found")
    return()
  }
  
  tripData <- fd_heatmap_group[(fd_heatmap_group$DestCityName==arrvCity)&(fd_heatmap_group$OriginCityName==deptCity),]
  
  return(tripData)
}

flightUi2("Minneapolis, MN","Denver, CO")
```

    ## # A tibble: 1 x 15
    ##     uniqueID avgDelay OriginCityName  DestCityName quantity noDelay delay15
    ##        <dbl>    <dbl> <chr>           <chr>           <int>   <int>   <dbl>
    ## 1 1348711292     13.7 Minneapolis, MN Denver, CO        453     252     101
    ## # ... with 8 more variables: delayOdds <chr>, delayOddsRaw <dbl>,
    ## #   delay15Odds <chr>, popularity <dbl>, orgin_lon <dbl>, orgin_lat <dbl>,
    ## #   destination_lon <dbl>, destination_lat <dbl>

### destPlot()

`destPlot()` takes a start and end city (from the cities in
`fd_heatmap_group` and returns a ggplot map object with a line between
the two cities plotted over a map of the United States. This function
uses `flightUi2()` to get the start and end coordinates of the two
cities, and then uses those coordinates to define a line and two points
on the a ggplot object. This function what creates the dynamic map in
the app.R UI.

``` r
#initialize ggplot base objects for function to work
world <- ne_countries(scale = "medium", returnclass = "sf", country = "united states of america")

map <- ggplot(world) + geom_sf() + coord_sf(xlim = c(-126, -66), ylim = c(25, 49), expand = FALSE) + theme_nothing() 
```

``` r
destPlot <- function(start, end){
  subset <- flightUi2(start, end)
  return(map + geom_segment(
  aes(x=orgin_lat, y=orgin_lon, 
      xend=destination_lat, yend=destination_lon),
  show.legend = FALSE, 
  data = subset, color = "blue") +
    geom_point(aes(orgin_lat, orgin_lon), data = subset) + 
    geom_text(aes(orgin_lat, orgin_lon, label=OriginCityName), data = subset) +
    geom_point(aes(destination_lat, destination_lon), data = subset) + 
    geom_text(aes(destination_lat, destination_lon, label=DestCityName), data = subset)
)
}

destPlot("Minneapolis, MN","Denver, CO")
```

![](./plots/destPlotEx.png)

## Creating Visualizations

### ggplot Object Setup

``` r
world <- ne_countries(scale = "medium", returnclass = "sf", country = "united states of america")

map <- ggplot(world) + geom_sf() + coord_sf(xlim = c(-126, -66), ylim = c(25, 49), expand = FALSE) + theme_nothing() 
```

This first map is of the flights with the greatest `avgDelay` or average
delay in minutes.

``` r
map + geom_segment(
  aes(x=orgin_lat, y=orgin_lon, 
      xend=destination_lat, yend=destination_lon,
      color=avgDelay, alpha = avgDelay),
  show.legend = FALSE, 
  data = fd_heatmap_group_continetnal
)
```

![](./plots/heatMap1.png)

This last map is of the most popular flights in the US, the transparency
of the line being determined by `qauntity` or the number of flights in a
given group.

``` r
map + geom_segment(
  aes(x=orgin_lat, y=orgin_lon, 
      xend=destination_lat, yend=destination_lon, alpha = quantity),
  show.legend = FALSE, 
  data = fd_heatmap_group_continetnal, 
  color = "#418733"
)
```

![](./plots/heatMap2.png)
