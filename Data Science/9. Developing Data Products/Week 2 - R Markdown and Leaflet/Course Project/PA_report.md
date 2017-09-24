# Peer-graded Assignment: R Markdown and Leaflet
    


## Executive Summary

This assignment is to showcase one of many use cases of the `leaflet` interactive mapping visualization package. We will discuss 5 of the big cities in China to implement population visualisation in this case. 

## Required Libraries

The following packages are required to reproduce.


```r
require(ggmap); require(leaflet); require(magrittr); require(data.table)
```

## Data Preparation

The total area (km<sup>2</sup>) and population density (/km<sup>2</sup>) data are acquired from [wikipedia](https://en.wikipedia.org/wiki/List_of_cities_proper_by_population). 
First we load the data into a dataframe.


```r
City <- c('Shanghai', 'Beijing', 'Guangzhou', 'Nanjing', 'Chongqing')

Total.area <- c(6340.5, 16410.54, 7434.4, 4713.85, 5473)

Pop.dens <- c(3826, 1311, 1759, 1737, 1496)
```

Next we need to obtain the coordinate of the cities. This is being done by leveraging on `ggmap` package. However, the google API will be triggered everytime the code is being executed. Therefore, we will just manually copy the result from the API and save it in R.


```r
lon <- c(121.47583008, 116.41113281, 113.27041626, 118.80065918, 106.92443848)

lat <- c(31.21749936, 39.8928799, 23.12394255, 32.05464469, 29.41567547)
```

Now we can create a dataframe that contains all the information that we need to visualize it in `leaflet`.


```r
mydata <- data.table(city = City, total.area = Total.area, pop.dens = Pop.dens, lon = lon, lat = lat)

## Transform the population density value to the range of 0 and 1 for color mapping
mydata[, color:= (pop.dens - min(pop.dens))/diff(range(pop.dens))]

## Create the pop up description and urbanisation rate
mydata[, popup:= paste0(city, '<br>Population Density: ', pop.dens, '/km2', '<br>Total Area ', total.area, 'km2')]
```

## Visualisation in Leaflet

Visualize the population data in leaflet.


```r
## Define a color function to map the population density to a color

f.color <- colorRamp(c(c('green', 'red')))

leaflet(mydata) %>% addTiles() %>% addCircles(weight = 1, radius = 30000, color = f.color(mydata$color) %>% rgb(maxColorValue = 256)) %>% addMarkers(popup = mydata$popup)
```

```
## Assuming 'lon' and 'lat' are longitude and latitude, respectively
## Assuming 'lon' and 'lat' are longitude and latitude, respectively
```

<!--html_preserve--><div id="htmlwidget-4c6e78aaaa3d11e1eff6" style="width:672px;height:480px;" class="leaflet html-widget"></div>
<script type="application/json" data-for="htmlwidget-4c6e78aaaa3d11e1eff6">{"x":{"options":{"crs":{"crsClass":"L.CRS.EPSG3857","code":null,"proj4def":null,"projectedBounds":null,"options":{}}},"calls":[{"method":"addTiles","args":["//{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",null,null,{"minZoom":0,"maxZoom":18,"maxNativeZoom":null,"tileSize":256,"subdomains":"abc","errorTileUrl":"","tms":false,"continuousWorld":false,"noWrap":false,"zoomOffset":0,"zoomReverse":false,"opacity":1,"zIndex":null,"unloadInvisibleTiles":null,"updateWhenIdle":null,"detectRetina":false,"reuseTiles":false,"attribution":"&copy; <a href=\"http://openstreetmap.org\">OpenStreetMap<\/a> contributors, <a href=\"http://creativecommons.org/licenses/by-sa/2.0/\">CC-BY-SA<\/a>"}]},{"method":"addCircles","args":[[31.21749936,39.8928799,23.12394255,32.05464469,29.41567547],[121.47583008,116.41113281,113.27041626,118.80065918,106.92443848],30000,null,null,{"lineCap":null,"lineJoin":null,"clickable":true,"pointerEvents":null,"className":"","stroke":true,"color":["#FE0000","#00FE00","#2DD100","#2BD300","#13EB00"],"weight":1,"opacity":0.5,"fill":true,"fillColor":["#FE0000","#00FE00","#2DD100","#2BD300","#13EB00"],"fillOpacity":0.2,"dashArray":null},null,null,null,null,null,null]},{"method":"addMarkers","args":[[31.21749936,39.8928799,23.12394255,32.05464469,29.41567547],[121.47583008,116.41113281,113.27041626,118.80065918,106.92443848],null,null,null,{"clickable":true,"draggable":false,"keyboard":true,"title":"","alt":"","zIndexOffset":0,"opacity":1,"riseOnHover":false,"riseOffset":250},["Shanghai<br>Population Density: 3826/km2<br>Total Area 6340.5km2","Beijing<br>Population Density: 1311/km2<br>Total Area 16410.54km2","Guangzhou<br>Population Density: 1759/km2<br>Total Area 7434.4km2","Nanjing<br>Population Density: 1737/km2<br>Total Area 4713.85km2","Chongqing<br>Population Density: 1496/km2<br>Total Area 5473km2"],null,null,null,null,null,null]}],"limits":{"lat":[23.12394255,39.8928799],"lng":[106.92443848,121.47583008]}},"evals":[],"jsHooks":[]}</script><!--/html_preserve-->
