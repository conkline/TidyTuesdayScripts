library(tidyverse)
library(lubridate)
library(geonames)
library(leaflet)
library(sp)
library(colorspace)
library(htmlwidgets)
library(htmltools)

#read in data
tdf_stages <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_stages.csv')

#extract stage starts/ends for 1900 - 1910 and 2010 - 2020
tdf_stages <- tdf_stages %>%
  mutate(year = year(Date)) %>%
  filter(year <= 1910 | year >= 2010) %>%
  mutate(Destination = as.character(Destination)) %>%
  mutate(Origin = as.character(Origin))

#replace resorts/lakes with closest city
replace_name <- function(old_name, new_name){
  
  tdf_stages$Destination[which(tdf_stages$Destination == old_name)] <- new_name
  tdf_stages$Origin[which(tdf_stages$Origin == old_name)] <- new_name
  assign('tdf_stages', tdf_stages, envir = .GlobalEnv) #yes this is lazy but I am also lazy
  
}

replace_name("Vielha Val d'Aran", "Vielha")
replace_name("Maubourguet Pays du Val d'Adour", "Maubourguet")
replace_name("Passage du Gois", "Noirmoutier")
replace_name("Station des Rousses", "Les Rousses")
replace_name("Morzine-Avoriaz", "Avoriaz")
replace_name("Lac de Payolle", "Campan")
replace_name("Andorra-Arcalis", "Ordino")
replace_name("Chalet Reynard", "Sault")
replace_name("La Caverne du Pont-d'Arc", "Vallon-Pont-d'Arc")
replace_name("Finhaut-Émosson", "Lac du Vieux Emosson")
replace_name("La Pierre Saint-Martin", "Arette")
replace_name("Plateau de Beille", "Ariege")
replace_name("La Toussuire - Les Sybelles", "Fontcouverte-la Toussuire")
replace_name("La Toussuire-Les Sybelles", "Fontcouverte-la Toussuire")
replace_name("Arenberg Porte du Hainaut", "Hainaut")
replace_name("Saint-Lary Pla d'Adet", "Saint-Lary-Soulan")
replace_name("Mont Ventoux", "Carpentras")
replace_name("Col du Galibier-Serre Chevalier", "Saint-Michel-de-Maurienne")
replace_name("Gérardmer La Mauselaine", "Vosges")
replace_name("Annonay-Davézieux", "Davézieux")

city_names <- unique(c(tdf_stages$Origin, tdf_stages$Destination))
lat_lon <- data.frame("city" = character(), "lng" = numeric(), "lat" = numeric())

#get lat/lon
for (c in city_names[216:length(city_names)]){
  search_result <- GNsearch(c)[1, c(2, 15)]
  lat_lon <- rbind(lat_lon, cbind(city = c, search_result))
}

#merge lat/lon data
tdf_stages_final <- tdf_stages %>%
  left_join(lat_lon, by = c("Origin" = "city")) %>%
  left_join(lat_lon, by = c("Destination" = "city")) %>%
  mutate(lat.x = as.numeric(lat.x)) %>%
  mutate(lng.x = as.numeric(lng.x)) %>%
  mutate(lat.y = as.numeric(lat.y)) %>%
  mutate(lng.y = as.numeric(lng.y))

  
#plot
csp <- choose_palette()
pal <- colorNumeric(palette = csp(length(unique(tdf_stages_final$year))),
                    domain = tdf_stages_final$year)

tag.map.title <- tags$style(HTML("
  .leaflet-control.map-title { 
    transform: translate(-50%,20%);
    position: fixed !important;
    left: 35%;
    text-align: left;
    padding-left: 10px; 
    padding-right: 10px; 
    font-weight: bold;
    font-size: 20px;
    color: #FFFDDB;
  }
"))

title <- tags$div(
  tag.map.title, HTML("How have Tour de France stops changed between 1900-1910 and 2010-2020?")
)  

leaflet(data = tdf_stages_final, 
        options = leafletOptions(zoomControl = FALSE)) %>%
  addProviderTiles("NASAGIBS.ViirsEarthAtNight2012") %>%
  setView(lng = 2.3488, lat = 48.85341, zoom = 3.7) %>%
  addCircleMarkers(lng = ~lng.x, lat = ~lat.x,
                   stroke = FALSE, fillOpacity = 0.7,
                   radius = 4, color = ~pal(year)) %>%
  addLegend("bottomleft", pal = pal, values = ~year,
            labFormat = labelFormat(big.mark = ""),
            title = "Year") %>%
  addControl(title, position = "topleft", className="map-title")
  
