
library(dplyr)
# install.packages("lifecycle")
library(ggplot2)
countries = c( 'PL', 'DE', 'CZ', 'NL', 'RO')

df = read.csv(".//data//prc_hicp_aind_page_linear.csv")
df[,c("geo", "TIME_PERIOD", "OBS_VALUE")]


df1 = read.csv(".//data//prc_hpi_a__custom_3617733_page_linear.csv")
df1[,c("geo", "TIME_PERIOD", "OBS_VALUE")]

colnames(df1)


df2 = read.csv(".//data//sdg_08_10_page_linear.csv")
df2[,c("geo", "TIME_PERIOD", "OBS_VALUE")]

colnames(df2)

df3 = read.csv(".//data//tec00114_page_linear.csv")
df3[,c("geo", "TIME_PERIOD", "OBS_VALUE")]

colnames(df3)
print(df3)

# ##################################################
# Single Country GDP graph

year_country_gdp <- df3 %>% select( TIME_PERIOD, geo, OBS_VALUE)
year_country_gdp <- na.omit(year_country_gdp)

colnames(year_country_gdp)

df3 %>% group_by(geo) %>% str()

str(year_country_gdp)

year_country_gdp <- filter(year_country_gdp, geo %in% countries)

# Plot
ggplot(year_country_gdp, aes(x = TIME_PERIOD, y = OBS_VALUE, color = geo, label = geo)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = geo), hjust = -0.1, size = 3)+
  labs(x = "Rok", y = 'PKB per capita w PPP [PPS_EU27_2020=100]') +
  scale_x_continuous(breaks=seq(2010,2024,2))

year_country_gdp
# ##################################################
# House price index HPI
df1
house_price_index <- df1 %>% select( TIME_PERIOD, geo, OBS_VALUE)
house_price_index <- na.omit(house_price_index)

colnames(house_price_index)

df1 %>% group_by(geo) %>% str()

str(house_price_index)

house_price_index <- filter(house_price_index, geo %in% countries)

# Plot
ggplot(house_price_index, aes(x = TIME_PERIOD, y = OBS_VALUE, color = geo, label = geo)) +
  geom_line() +
  geom_point() +
  geom_text(data = house_price_index %>% 
              group_by(geo) %>% 
              slice(n() - 1),
            aes(label = geo, hjust = -1, size = 4))+
  scale_x_continuous(breaks=seq(2010,2024,2))
  labs(x = "Rok", y = 'Indeks Cen nieruchomości [cena z 2015 roku = 100]')


house_price_index
# ######################################

# HICP - Harmonised Index for Consumer Prices
df
hicp_index <- df %>% select( TIME_PERIOD, geo, OBS_VALUE)
hicp_index <- na.omit(hicp_index)

colnames(hicp_index)

df %>% group_by(geo) %>% str()

str(hicp_index)

hicp_index <- filter(hicp_index, geo %in% countries)

# Plot
ggplot(hicp_index, aes(x = TIME_PERIOD, y = OBS_VALUE, color = geo, label = geo)) +
  geom_line() +
  geom_point() +
  geom_text(data = hicp_index %>% 
              group_by(geo) %>% 
              slice(n()),
            aes(label = geo, hjust = -0.2, size = 4)) +
  labs(x = "Rok", y = 'Indeks inflacji konsumenckiej HICP [2015 = 100]') +
  scale_x_continuous(breaks=seq(2010,2024,2))


hicp_index

# ########################
# Show data discounting inflation rate

# Merge the two data frames using the 'country' and 'date' columns
merged_df <- merge(house_price_index, hicp_index, by = c("geo", "TIME_PERIOD"))

merged_df
# Create a new column that divides 'value1' by 'value2'
merged_df$house_prices_wo_hicp <- merged_df$OBS_VALUE.x / merged_df$OBS_VALUE.y*100

merged_df$TIME_PERIOD
merged_df$compound_growth <- 1 * (1 + 0.02) ^ (1:(merged_df$TIME_PERIOD-2015))

# View the resulting merged data frame with the divided values
merged_df
merged_df <- na.omit(merged_df)

colnames(merged_df)

merged_df %>% group_by(geo) %>% str()

str(merged_df)

merged_df <- filter(merged_df, geo %in% countries)

# Plot
ggplot(merged_df, aes(x = TIME_PERIOD, y = house_prices_wo_hicp, color = geo)) +
  geom_line(linetype="dotted", size=1) +
  geom_point(aes(x=TIME_PERIOD, y=house_prices_wo_hicp)) +
  geom_text(data = merged_df %>% 
              group_by(geo) %>% 
              slice(n()),
            aes(label = geo, hjust = -0.2, size = 4)) +
  stat_function(fun=function(x) 100*(1.04)^(x-2015), aes(colour = "4% Compounding")) +
  stat_function(fun=function(x) 100*(1.07)^(x-2015), aes(colour = "7% Compounding")) +
  scale_x_continuous(breaks=seq(2010,2024,2)) +
  labs(x = "Year", y = 'Indeks cen nieruchomości zdyskontowany o wartość inflacji [2015 = 100]')

=======
library(dplyr)
# install.packages("ggplot2")
library(ggplot2)
countries = c( 'PL', 'DE', 'CZ', 'NL', 'RO')

df = read.csv(".//data//prc_hicp_aind_page_linear.csv")
df[,c("geo", "TIME_PERIOD", "OBS_VALUE")]


df1 = read.csv(".//data//prc_hpi_a__custom_3617733_page_linear.csv")
df1[,c("geo", "TIME_PERIOD", "OBS_VALUE")]

colnames(df1)


df2 = read.csv(".//data//sdg_08_10_page_linear.csv")
df2[,c("geo", "TIME_PERIOD", "OBS_VALUE")]

colnames(df2)

df3 = read.csv(".//data//tec00114_page_linear.csv")
df3[,c("geo", "TIME_PERIOD", "OBS_VALUE")]

colnames(df3)
print(df3)

# ##################################################
# Single Country GDP graph

year_country_gdp <- df3 %>% select( TIME_PERIOD, geo, OBS_VALUE)
year_country_gdp <- na.omit(year_country_gdp)

colnames(year_country_gdp)

df3 %>% group_by(geo) %>% str()

str(year_country_gdp)

year_country_gdp <- filter(year_country_gdp, geo %in% countries)

# Plot
ggplot(year_country_gdp, aes(x = TIME_PERIOD, y = OBS_VALUE, color = geo, label = geo)) +
  geom_line() +
  geom_point() +
  geom_text(aes(label = geo), hjust = -0.1, size = 3)+
  labs(x = "Rok", y = 'PKB per capita w PPP [PPS_EU27_2020=100]') +
  scale_x_continuous(breaks=seq(2010,2024,2))

year_country_gdp
# ##################################################
# House price index HPI
df1
house_price_index <- df1 %>% select( TIME_PERIOD, geo, OBS_VALUE)
house_price_index <- na.omit(house_price_index)

colnames(house_price_index)

df1 %>% group_by(geo) %>% str()

str(house_price_index)

house_price_index <- filter(house_price_index, geo %in% countries)

# Plot
ggplot(house_price_index, aes(x = TIME_PERIOD, y = OBS_VALUE, color = geo, label = geo)) +
  geom_line() +
  geom_point() +
  geom_text(data = house_price_index %>% 
              group_by(geo) %>% 
              slice(n() - 1),
            aes(label = geo, hjust = -1, size = 4))+
  scale_x_continuous(breaks=seq(2010,2024,2))
  labs(x = "Rok", y = 'Indeks Cen nieruchomości [cena z 2015 roku = 100]')


house_price_index
# ######################################

# HICP - Harmonised Index for Consumer Prices
df
hicp_index <- df %>% select( TIME_PERIOD, geo, OBS_VALUE)
hicp_index <- na.omit(hicp_index)

colnames(hicp_index)

df %>% group_by(geo) %>% str()

str(hicp_index)

hicp_index <- filter(hicp_index, geo %in% countries)

# Plot
ggplot(hicp_index, aes(x = TIME_PERIOD, y = OBS_VALUE, color = geo, label = geo)) +
  geom_line() +
  geom_point() +
  geom_text(data = hicp_index %>% 
              group_by(geo) %>% 
              slice(n()),
            aes(label = geo, hjust = -0.2, size = 4)) +
  labs(x = "Rok", y = 'Indeks inflacji konsumenckiej HICP [2015 = 100]') +
  scale_x_continuous(breaks=seq(2010,2024,2))


hicp_index

# ########################
# Show data discounting inflation rate

# Merge the two data frames using the 'country' and 'date' columns
merged_df <- merge(house_price_index, hicp_index, by = c("geo", "TIME_PERIOD"))

merged_df
# Create a new column that divides 'value1' by 'value2'
merged_df$house_prices_wo_hicp <- merged_df$OBS_VALUE.x / merged_df$OBS_VALUE.y*100

merged_df$TIME_PERIOD
merged_df$compound_growth <- 1 * (1 + 0.02) ^ (1:(merged_df$TIME_PERIOD-2015))

# View the resulting merged data frame with the divided values
merged_df
merged_df <- na.omit(merged_df)

colnames(merged_df)

merged_df %>% group_by(geo) %>% str()

str(merged_df)

merged_df <- filter(merged_df, geo %in% countries)

# Plot
ggplot(merged_df, aes(x = TIME_PERIOD, y = house_prices_wo_hicp, color = geo)) +
  geom_line(linetype="dotted", size=1) +
  geom_point(aes(x=TIME_PERIOD, y=house_prices_wo_hicp)) +
  geom_text(data = merged_df %>% 
              group_by(geo) %>% 
              slice(n()),
            aes(label = geo, hjust = -0.2, size = 4)) +
  stat_function(fun=function(x) 100*(1.04)^(x-2015), aes(colour = "4% Compounding")) +
  stat_function(fun=function(x) 100*(1.07)^(x-2015), aes(colour = "7% Compounding")) +
  scale_x_continuous(breaks=seq(2010,2024,2)) +
  labs(x = "Year", y = 'Indeks cen nieruchomości zdyskontowany o wartość inflacji [2015 = 100]')