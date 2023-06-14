library(tidyverse)
library(eurostat)
library(leaflet)
library(sf)
library(scales)
library(cowplot)
library(ggthemes)

library(RColorBrewer)
R.Version()

# Load results from project_1
map_df = read.csv(".//data//compound_interest_housing.csv")
map_df

# Load map/polygon data from eurostat
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

# Join datasets

mapdata_new <- left_join(SHP_28, map_df, by="geo", relationship = "many-to-many")
merged_df <- left_join(SHP_28, merged_df, by="geo", relationship = "many-to-many")
merged_df

# Delete Greece (null values)
mapdata_new <- mapdata_new[-9,]
mapdata_new
mapdata_new$wiki <- paste0( "https://en.wikipedia.org/wiki/", mapdata_new$name )

# Create a continuous palette function
qpal <- colorQuantile("YlOrBr", mapdata_new$substr_house_prices_wo_hicp, n = 5)

qpal_colors <- unique(qpal(sort(mapdata_new$substr_house_prices_wo_hicp))) # hex codes
qpal_labs <- quantile(mapdata_new$substr_house_prices_wo_hicp, seq(0, 1, .2)) # depends on n from pal
qpal_labs <- paste(lag(qpal_labs), qpal_labs, sep = " - ")[-1] # first lag is NA

popup_content <- paste0( "<div style='text-align: center'>"
                         , "_______________________________<br>"
                         , "Country: ", mapdata_new$name, "<br><br>"
                         , "</div>"
                         
                         , "House prices index: ", mapdata_new$substr_house_prices_wo_hicp, "<br>"
                         
                         , "Compound interest equivalent: ", mapdata_new$compound_interest, "%<br>"
                         
                         , "<div style='text-align: center'>"
                         , "<a href='", mapdata_new$wiki, "' target='_blank'>", "<br>"
                         , "Click here to view wiki</a>", "<br>"
                         , "</div>" )

map <- leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data=mapdata_new,
              fillOpacity = 0.6, # Przezroczystość
              stroke = TRUE, # Borders visible
              color = "grey", # Border color
              weight = 1,
              
              fillColor = ~qpal(mapdata_new$substr_house_prices_wo_hicp),
              popup = popup_content,
              popupOptions = popupOptions(maxWidth ="100%", closeOnClick = TRUE),
              ) %>% 
  setView( lat = 53, lng = 14, zoom = 4) %>%
  addLegend("bottomright", colors = qpal_colors,
            title = "<span style='white-space: pre-line;'> Real house prices \n index (2022) </span>",
            labels = qpal_labs,
            opacity = 1)

map
