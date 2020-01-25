## Package and some annotations

#Replace age 1 represent for 5-14 years
#Replace age 2 represent for 15-24 years
#Replace age 3 represent for 25-34 years
#Replace age 4 represent for 35-54 years
#Replace age 5 represent for 55-74 years
#Replace age 6 represent for 75+ years
library(tidyverse)
library(dplyr)
library(ggplot2)
library(countrycode)
library(rworldmap)
library(animation)
library(gganimate)
library(leaflet)
library(rgeos)
library(rworldmap)
library(maps)
library(tidyverse)
library(rworldxtra)


## Preprocessing

data <- read.csv("master.csv",stringsAsFactors = FALSE )
data <- as.data.frame(data)
#rename 嚜盧ountry into country
names(data)[1] <- "country"
# convert country into continent
data$continent <- countrycode(sourcevar = data[,"country"],
                              origin = "country.name",
                              destination = "continent")

data <- data %>%
  filter(year != 2016)


## Suicide

data_suirate<- data %>%
  select(year, suicides_million_pop) %>%
  group_by(year) %>%
  summarise(sum_sui=sum(suicides_million_pop))

ggplot(data_suirate, aes(x=year, y=sum_sui))+
  geom_line(position = "stack", color = "dodgerblue3", size = 1.5)+
  geom_point(size = 2, color = "orangered")+
  labs(title = "Suicide Rate")+
  theme_grey(base_size = 20)


data_suirate<- data %>%
  select(year, suicides_no) %>%
  group_by(year) %>%
  summarise(sum_sui=sum(suicides_no))

ggplot(data_suirate, aes(x=year, y=sum_sui))+
  geom_col( fill = "dodgerblue3")+
  labs(title = "Suicide Number")+
  theme_grey(base_size = 20)


## Suicide associated with sex

data_year_sex <- data %>%
  select(year, sex, suicides_no) %>%
  arrange(year, sex) %>%
  group_by(year,sex) %>%
  summarise(sum_suicides_no = sum(suicides_no))

ggplot(data_year_sex, aes(x=year,y=sum_suicides_no,fill = sex))+
  geom_col(position = "fill") +
  #scale_fill_brewer(palette = "Set1")+
   scale_fill_manual(values=c("dodgerblue3", "darkorange"))+
  labs(title = "Suicide Rate Male versus Female")+
  theme_grey(base_size = 20)

data_year_sex <- data %>%
  select(year, sex, suicides_million_pop) %>%
  arrange(sex, year) %>%
  group_by(sex,year) %>%
  summarise(sum_suicides_rate = sum(suicides_million_pop))

ggplot(data_year_sex, aes(year,sum_suicides_rate, color = sex))+
  geom_line(size = 1.5)+
  scale_colour_manual(values=c("dodgerblue3", "darkorange")) +
  geom_point(size = 2)+
  labs(title = "Suicide Rate Male versus Female")+
  theme_grey(base_size = 20)


## Suicide associated with age

data_year_sui <- data %>%
  select(year, age, suicides_million_pop) %>%
  arrange(year, age) %>%
  group_by(year, age) %>%
  summarise(sum = sum(suicides_million_pop))


ggplot(data_year_sui, aes(x=year, y=sum, fill=age))+
  geom_col(position = "fill")+
  labs(title = "Suicide propotion of all age")+
  theme_grey(base_size = 20)

data_eachage <- data %>%
  select(year, suicides_million_pop, age) %>%
  group_by(age, year) %>%
  summarise(sum_suirate=sum(suicides_million_pop))

ggplot(data_eachage, aes(year, sum_suirate, color = age)) +
  geom_line() +
  geom_point() +
  facet_grid(age~.)+
  labs(title = "Suicide Number in each Age", color = "Age")+
  theme_grey(base_size = 20)


## Suicides associated with gdp
data_gdp <- data %>%
  select(year, gdp_per_capita, suicides_million_pop, population)

temp <- ggplot(data_gdp, aes(x=gdp_per_capita, y=suicides_million_pop, size = population))+
 geom_point()+
 transition_time(year)+
 labs(title = 'Date: {frame_time}')+
 theme_grey(base_size = 20)
 animate(temp, fps = 2)


## Suicides in different continent

data_con <- data %>%
 select(year, suicides_no, continent) %>%
 group_by(year, continent) %>%
 summarise(sum_con = sum(suicides_no))
ggplot(data_con, aes(continent, sum_con, fill = continent))+
 geom_col()+
 scale_fill_brewer(palette = "Blues") +
 transition_time(year) +
 labs(title = "Date: {frame_time}")+
 theme_grey(base_size = 20)

data_con <- data %>%
  select(continent, year, suicides_million_pop) %>%
  group_by(continent, year) %>%
  summarise(sum_sui_rate = sum(suicides_million_pop))

ggplot( data_con, aes(year, sum_sui_rate, col = continent)) +
  geom_line( size = 1.5)+
  labs(title = "Suicide rate each continent")+
  theme_grey(base_size = 20)


## Suicides number of different country in 2000

data_country <- data %>%
  filter(year == 2000) %>%
  select(country, suicides_million_pop) %>%
  group_by(country) %>%
  summarise(sum_sui_rate = sum(suicides_million_pop)) %>%
  arrange(desc(sum_sui_rate))

data_country$country <- factor(data_country$country,
                               ordered = T,
                               levels = rev(data_country$country))

ggplot(data_country, aes(country,sum_sui_rate, fill=country)) +
  geom_col()+
  scale_fill_viridis_d()+
  coord_flip()+
  theme(legend.position = "none")+
  labs(title = "Suicide rate each country")


## Suicide number on world map

saveGIF({
 for( i in 1987:2015 ){
    data_continent <- data %>%
      filter(year == i) %>%
      select(suicides_no, country) %>%
      group_by(country) %>%
      summarise(sum_con=sum(suicides_no))

   #oceanCol="lightblue", missingCountryCol="grey65"
    par(mai=c(0,0,0.5,0),xaxs="i",yaxs="i")
    sPDF <- joinCountryData2Map( data_continent, joinCode = "NAME" ,nameJoinColumn = "country")
    mapCountryData( sPDF,
                    mapTitle = i,
                    nameColumnToPlot="sum_con",
                    catMethod = c(0, 5000, 10000, 15000, 20000, 25000, 30000, 35000, 40000, 45000),
                    colourPalette = "white2Black",)
 }}
 ,interval = .2
 ,movie.name="test.gif"
 )

knitr::include_graphics("test.gif")


## Suicide number on world map with leaflet

# Get world map
wmap <- getMap(resolution = 'high')

# Get centroids of each countries
centroids <- gCentroid(wmap, byid = TRUE)
df <- as.data.frame(centroids)

# Since there are some countries with different name in two data,
# we adjust them to the same names.
rownames(df)[rownames(df) == "The Bahamas"] <- "Bahamas"
rownames(df)[rownames(df) == "United States of America"] <- "United States"
rownames(df)[rownames(df) == "Russia"] <- "Russian Federation"
rownames(df)[rownames(df) == "Cape Verde"] <- "Cabo Verde"
rownames(df)[rownames(df) == "South Korea"] <- "Republic of Korea"
rownames(df)[rownames(df) == "Macau S.A.R"] <- "Macau"
rownames(df)[rownames(df) == "Republic of Serbia"] <- "Serbia"
rownames(df)[rownames(df) == "Saint Vincent and the Grenadines"] <- "Saint Vincent and Grenadines"

# Load suicide data
master <- read.csv('master.csv')
names(master)[1] <- "country"
total_countries <- (unique(master['country']))
total_countries <- total_countries %>% pull(country)

# Calculate suicides per million population
data_year_sui <- master %>%
  select(country, year, age, suicides_million_pop) %>%
  arrange(year, age) %>%
  group_by(country, year) %>%
  summarise(sum = sum(suicides_million_pop))

data_year_sui$country = as.character(data_year_sui$country)

# Save longitude and latitude
country_cor <- data.frame(country=character(),
                          lat=double(),
                          lng=double(),
                          suicides=character(),
                          stringsAsFactors=FALSE)

for(name in total_countries) {
  if(name %in% rownames(df)) {
    col <- data_year_sui %>% filter(country == name)
    content = name
    for(i in seq(nrow(col))) {
      row <- paste((col$year)[i], (col$sum)[i])
      content = paste(sep = "<br/>", content, row)
    }
    country_cor <- country_cor %>% add_row(country = name, lat = df[name, 'y'], lng = df[name, 'x'], suicides = content)
  }
}


# Create Interactive map
leaflet(data=country_cor) %>%
  addTiles() %>%
  addMarkers (lat=country_cor$lat, lng=country_cor$lng, popup=country_cor$suicides)
