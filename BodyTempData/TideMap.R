require("leaflet")
require("here")
library(sf)
library(sp)
library(dplyr)
library(stringr)
require(leaflegend)
library("osmdata")
library("fontawesome")
library(htmltools) # tools to support html workflow
require("leaflet.extras")
library(tidyverse) 
require("leaflet.providers")
require("devtools")
library(readxl)

##Lk Collection month map


setwd("~/Documents/R/BodyTempData")
getwd()

#load data
mapData <- read_xlsx("sitelocationmap.xlsx")

#take out samples with no lat and lon (if you dont she will get upset and not do anything)
mapRlkdat=subset(mapRlkdat, run_Lk_Assay == "Y")
mapRlkdat=subset(mapRlkdat, Lat != " ")
mapRlkdat=subset(mapRlkdat, Lat != "NA")
mapRlkdat=subset(mapRlkdat, Lat != "NEAq")
mapRlkdat=subset(mapRlkdat, Lon != " ")
mapRlkdat=subset(mapRlkdat, Lon != "NA")
mapRlkdat=subset(mapRlkdat, Lon != "NEAq")
mapRlkdat=subset(mapRlkdat, Lk_AMP_result != "NA")
mapRlkdat=subset(mapRlkdat, Sample_. != "69")
mapRlkdat=subset(mapRlkdat, Sample_. != "63")




#format lat and lon columns 
mapRlkdat=subset(mapRlkdat, longitude !=" ")
mapRlkdat=subset(mapRlkdat, latitude !=" ")
mapRlkdat$longitude=as.numeric(mapRlkdat$longitude)
mapRlkdat$longitude=as.numeric(mapRlkdat$latitude)

pointslkR = st_as_sf(mapRlkdat, coords = c("Longitude", "Latitude"), crs = 4326)



#wanted different colors so made a palette that will assign a color to each collection month factor
pal<- colorFactor(
  palette = "Dark2",
  domain = pointslkR$Year)

#you SHOULD NOT add a jitter, as you want your points to be exactly where they were reported
#add jitter to better visualize points on map
#pointsjit= (st_jitter(points, .005))

#transform lat and lon columns- this actually might not be important now that im looking at it again lol
#coordinatesmaplk = st_coordinates(st_transform(pointslkR, 4326))

#map
m=leaflet() %>% addTiles() %>%  addScaleBar() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addCircleMarkers(
    data = pointslkR,
    # set the radius of the circles
    radius = 1,
    opacity= .7,
    color=~pal(Year),
    # create custom labels when you hover over points
    label = paste(
      "Month: ",
      pointslkR$Month, "<br>",
      "Season",
      pointslkR$Season, "<br>",
      "Year: ",
      pointslkR$Year, "<br>")
    
    %>%
      lapply(htmltools::HTML),
  )    %>%
  addLegend("bottomleft", colors = c("green",  "pink", "purple", "orange", "cadetblue"), labels = c("2023", "2022", "2021", "2019", "2018"),title = "Sample Sighting Year", opacity = .8)
#add a legend to your map

#my shit
library(leaflet)


#general map site locations
leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  setView(lng = -70.634568, lat = 42.655204, zoom = 13) %>%
  addMarkers(data = mapData, lng = ~lng, lat = ~lat)

#more in depth view of loblolly

mapDataLob <- read_xlsx("loblollylatlong.xlsx")

leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  setView(lng = -70.591944, lat = 42.640833, zoom = 20) %>%
  addMarkers(data = mapDataLob, lng = ~lng, lat = ~lat, group = ~group)

#more in depth view of seaside

mapDataSea <- read_xlsx("seasidelatlong.xlsx")

leaflet() %>%
  addProviderTiles(providers$Esri.WorldImagery) %>%
  setView(lng = -70.650333, lat = 42.685028, zoom = 20) %>%
  addMarkers(data = mapDataSea, lng = ~lng, lat = ~lat, group = ~group)

  