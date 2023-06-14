library(tidyverse)
library(eurostat)
# install.packages("ggthemes")
library(leaflet)
library(sf)
library(scales)
library(cowplot)
library(ggthemes)
library(RColorBrewer)

map_df = read.csv(".//data//compound_interest_housing.csv")
map_df

SHP_0 <- get_eurostat_geospatial(resolution = 10, 
                        nuts_level = 0, 
                        year = 2016)
SHP_0 %>% 
  ggplot() +
  geom_sf()

EU28 <- eu_countries %>% 
  select(geo = code, name)

SHP_28 <- SHP_0 %>% 
  select(geo = NUTS_ID, geometry) %>% 
  inner_join(EU28, by = "geo") %>% 
  arrange(geo) %>% 
  st_as_sf()

SHP_28 %>% 
  ggplot() +
  geom_sf() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65))

mapdata_new <- left_join(SHP_28, map_df, by="geo", relationship = "many-to-many")



# Create a continuous palette function
pal <- colorQuantile("Blues", mapdata_new$substr_house_prices_wo_hicp, n = 9)

popup_content <- paste0( "Country_information_box: "
                         , mapdata_new$name 
                         , "<br>"
                         , "<a href='"
                         , mapdata_new$name
                         , "' target='_blank'>"
                         , "Click Here</a>")

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=mapdata_new,
              fillOpacity = 0.6, # Przezroczystość
              stroke = TRUE, # Borders visible
              color = "grey", # Border color
              weight = 1,
              
              fillColor = ~pal(mapdata_new$substr_house_prices_wo_hicp),
              popup = popup_content,
              popupOptions = popupOptions(maxWidth ="600px", closeOnClick = TRUE),
              )
