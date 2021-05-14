
## geospatial mapping in R
## Fri May 14, 2021

# install packages--only need to do this once
remotes::install_github("wfmackey/absmapsdata")
install.packages(c("tidyverse", "sf", "tidygeocoder", "leaflet"), dependencies = TRUE)

library(tidyverse)
library(sf)
library(absmapsdata)
library(tidygeocoder)
library(leaflet)

# load in postcode polygon data from absmapsdata library
post_data <- postcode2016
str(post_data)
View(post_data)

# load in Covid data and delete null values
covid_data <- read_csv("data/covid_data.csv")
covid_data <- covid_data

# take a look
glimpse(covid_data)
View(covid_data)

# load in location data
location_data <- read_csv("data/location_data.csv")

# take a look
glimpse(location_data)
View(location_data)

# geocode and tally location
coords <- location_data %>%
  geocode(location, method = "osm", country = "Australia") %>%
  group_by(location, lat, long) %>%
  count()

# take a look
glimpse(coords)
View(coords)

# merge all datasets
covid_post_data <- left_join(post_data, covid_data, by = "postcode_num_2016")
View(covid_post_data)

# choose custom value cutoffs for color bins
bins <- c(0, 25, 50, 75, 100, Inf)
#bins <- c(0, 50, 100, Inf)

# choose color palettes
pal <- colorBin("Purples", domain = covid_post_data$cases, bin = bins, reverse = FALSE)

# subset/filter data by Victorian postcodes only 
covid_post_data %>%
  
  # remove NA values
  na.omit() %>%
  
  # specify coordinate reference system
  st_transform(crs = "+proj=longlat +datum=WGS84") %>%
  
  # subset dataset by Victorian postcodes and cases > 0 only
  filter(postcode_num_2016 >= 3000 & postcode_num_2016 <= 3999 |
           postcode_num_2016 >= 8000 & postcode_num_2016 <= 8999 &
           cases >= 0) %>%
  
  # leaflet mapping
  leaflet() %>%
  
  # set default zoom level around Melbourne CBD
  setView(lng = 144.9631, lat = -37.8136, 9.5) %>%
  
  # add all the base aesthetic maps desired and give them unique names
  addProviderTiles(provider = "CartoDB.Positron", group = "Grey") %>% 
  addProviderTiles(provider = "CartoDB.DarkMatter", group = "Dark") %>%
  addProviderTiles(provider = "Esri.NatGeoWorldMap", group = "Nat Geo") %>%
  
  # add choropleth layer and adjust color, weight, opacity, and stroke
  addPolygons(fillColor = ~pal(cases),
              weight = 0.5,
              opacity = 1,
              color = "white",
              dashArray = "1",
              fillOpacity = 0.7,
              stroke = TRUE,
              group = "Postcodes",
              # highlight borders as you move over the map with a mouse
              highlight = highlightOptions(weight = 2,
                                           color = "black",
                                           dashArray = "",
                                           fillOpacity = 0.3,
                                           bringToFront = TRUE),
              
              # popup/hover box text
              label = ~paste0("Postcode: ", postcode_num_2016, " Active cases: ", cases)) %>%
  
  # map legend attributes
  addLegend(pal = pal, values = ~as.numeric(cases), opacity = 0.7, 
            title = "Active Covid-19 cases", position = "bottomright") %>%
  
  # minimap as a popout
  addMiniMap(tiles = "CartoDB.Positron",
             zoomAnimation = TRUE,
             toggleDisplay = TRUE,
             minimized = TRUE,
             position = "bottomleft",
             zoomLevelOffset = -6,
             width = 200,
             height = 200) %>%
  
  # add second layer with circle markers 
  # call new dataset with long/lat columns
  addCircleMarkers(data = coords,
                   lng = ~long,
                   lat = ~lat,
                   fillColor = "orange",
                   stroke = TRUE,
                   color = "orange",
                   radius = ~n * 2,
                   fillOpacity = 1,
                   popup = ~paste0(location, "<br>", n),
                   group = "Crime") %>%
  
  # layer control menu properties
  addLayersControl(baseGroups = c("Grey", "Dark", "Nat Geo"),
                   overlayGroups = c("Postcodes", "Crime"),
                   options = layersControlOptions(collapsed = TRUE),
                   position = "topright") %>%
  
  # add an empty tile for custom attribution with link to data if desired
  addTiles(urlTemplate = "", attribution ='<a href="https://www.abs.gov.au/"> Data from ABS and my imagination!</a>') %>%
  
  # add easy buttons as deisred
  addEasyButton(easyButton(icon = "fa-globe", title = "Zoom to Level 5",
                           onClick = JS("function(btn, map){ map.setZoom(5); }"))) %>%
  addEasyButton(easyButton(icon = "fa-crosshairs", title = "Locate Me",
                           onClick = JS("function(btn, map){ map.locate({setView: true}); }"))) %>%
  
  # hide Crime layer in default map
  hideGroup("Crime")

                
                