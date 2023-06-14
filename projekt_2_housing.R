<<<<<<< HEAD
library(dplyr)
# library(tidyverse)
# install.packages("tidyverse")
library(ggplot2)
library(colorRamps)
# countries = c( 'PL', 'DE', 'CZ', 'NL', 'RO')

map_df = read.csv(".//data//compound_interest_housing.csv")
map_df

ggplot(map_df, aes(x = TIME_PERIOD, y = compound_interest, color = geo)) +
  geom_point(aes(x=TIME_PERIOD, y=compound_interest)) +
  geom_text(data = map_df %>% 
              group_by(geo) %>% 
              slice(n()),
            aes(label = geo, hjust = -0.2, size = 4)) +
  scale_x_continuous(breaks=seq(2010,2024,2)) +
  labs(x = "Year", y = 'wartość mieszkania jako % składany powyżej inflacji')


# filter data to have both coordinates and value
geo_list <- c("Belarus", "Greece", "Latvia", "Albania", 
              "Switzerland", "Bosnia and Herzegovina", "Ukraine", 
              "UK", "Turkey", "Serbia", "Kosovo", "Moldova", "North Macedonia", 
              "Montenegro", "Cyprus", "Malta", "Georgia", "Armenia")

mapdata <- map_data("world")
mapdata1 <- left_join(mapdata, map_df, by="region", relationship = "many-to-many")

mapdata2 <- mapdata1 %>% filter(!is.na(mapdata1$compound_interest)| mapdata1$region %in% geo_list)

mapdata2

map1 <- ggplot(mapdata2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = compound_interest), color = "black") +
  #geom_text(aes(label = region), size = 3, nudge_y = 1) +  # Add labels
  #scale_fill_gradient(name="compound interest", low = "white", high = "black", na.value = "yellow") +
  scale_fill_viridis_c(name="compound interest", option = "plasma", trans = "sqrt", na.value = "grey") + # colorblind-friendly palette
  labs(title = "World Map with Compound Interest")  # Set plot title
map1

=======
library(dplyr)
library(tidyverse)
# install.packages("colorRamps")
library(ggplot2)
library(colorRamps)
# countries = c( 'PL', 'DE', 'CZ', 'NL', 'RO')

map_df = read.csv(".//data//compound_interest_housing.csv")
map_df

ggplot(map_df, aes(x = TIME_PERIOD, y = compound_interest, color = geo)) +
  geom_point(aes(x=TIME_PERIOD, y=compound_interest)) +
  geom_text(data = map_df %>% 
              group_by(geo) %>% 
              slice(n()),
            aes(label = geo, hjust = -0.2, size = 4)) +
  scale_x_continuous(breaks=seq(2010,2024,2)) +
  labs(x = "Year", y = 'wartość mieszkania jako % składany powyżej inflacji')


# filter data to have both coordinates and value
geo_list <- c("Belarus", "Greece", "Latvia", "Albania", 
              "Switzerland", "Bosnia and Herzegovina", "Ukraine", 
              "UK", "Turkey", "Serbia", "Kosovo", "Moldova", "North Macedonia", 
              "Montenegro", "Cyprus", "Malta", "Georgia", "Armenia")

mapdata <- map_data("world")
mapdata1 <- left_join(mapdata, map_df, by="region", relationship = "many-to-many")
mapdata2 <- mapdata1 %>% filter(!is.na(mapdata1$compound_interest)| mapdata1$region %in% geo_list)

mapdata2

map1 <- ggplot(mapdata2, aes(x = long, y = lat, group = group)) +
  geom_polygon(aes(fill = compound_interest), color = "black") +
  #geom_text(aes(label = region), size = 3, nudge_y = 1) +  # Add labels
  #scale_fill_gradient(name="compound interest", low = "white", high = "black", na.value = "yellow") +
  scale_fill_viridis_c(name="compound interest", option = "plasma", trans = "sqrt", na.value = "grey") + # colorblind-friendly palette
  labs(title = "World Map with Compound Interest")  # Set plot title
map1

>>>>>>> 9cd7ec6d23058242fa1b48bf76dd80c927b5ef48
