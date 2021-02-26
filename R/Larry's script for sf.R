
#1. json / dataframe
#json file get from web and transfrom it to the dataframe version for later use

# setwd("C:/Users/du/Desktop/citibikedata_dot/")
# install.packages("jsonlite")
library(jsonlite)
# library(tibble)
library(dplyr)
# stationinfo <- fromJSON("station_information.json")
stationinfo <- fromJSON("https://gbfs.citibikenyc.com/gbfs/en/station_information.json")

stationinfo<-stationinfo$data

stationinfo<-stationinfo$stations
stationinfo <- as_data_frame(stationinfo)
library(sf)
stationinfo_sf = st_as_sf(stationinfo, coords = c("lon", "lat"), 
                 crs = 4326)
plot(stationinfo_sf$geometry)

head(stationinfo_sf)



#2. sf and spatial join


#read sf
setwd("~/Desktop/nynta_20d")
nta <- st_read("nynta.shp")
# Simple feature collection with 195 features and 7 fields
# sf which means simple feature

#spatial join

nta<-st_transform(nta,4326)
plot(ntastationplot$geometry)
st_crs(nta) == st_crs(stationinfo_sf)

ntastation<-st_join(nta,stationinfo_sf)
nta$count<-lengths(st_intersects(nta, stationinfo_sf))
library(viridis)
library(ggplot2)
ggplot() +
  geom_sf(data = nta, aes(fill =count  )) +
  scale_fill_viridis(discrete = F, direction = 1, option="magma")

# ntastation<-st_join(stationinfo_sf,nta)
library(dplyr)
ntastationplot<-ntastation%>%
  group_by(NTACode)%>%
  summarize(capacity = mean(capacity))


ntastationplot



#3. ggplot for sf



# install.packages("viridis")


ggplot() +
  geom_sf(data = ntastationplot, aes(fill =capacity  )) +
  scale_fill_viridis(discrete = F, direction = 1, option="magma")


p <- ggplot() +
  geom_sf(data = ntastationplot, aes(fill = capacity) , size=0, alpha=0.9) +
  theme_void() +
  scale_fill_viridis(breaks=c(10,15,20,25,30,35,40,50,100), name="citi bike capacity", guide = guide_legend( keyheight = unit(2, units = "mm"), keywidth=unit(12, units = "mm"), label.position = "bottom", title.position = 'top', nrow=1) ) +
  labs(
    title = "NYC citi bike station capacity by NTA",
    subtitle = "NYC planning",
    caption = "Data: NYC citi bike open data"
  ) +
  theme(
    text = element_text(color = "#22211d"),
    plot.background = element_rect(fill = "#f5f5f2", color = NA),
    panel.background = element_rect(fill = "#f5f5f2", color = NA),
    legend.background = element_rect(fill = "#f5f5f2", color = NA),
    
    plot.title = element_text(size= 22, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.4, l = 2, unit = "cm")),
    plot.subtitle = element_text(size= 17, hjust=0.01, color = "#4e4d47", margin = margin(b = -0.1, t = 0.43, l = 2, unit = "cm")),
    plot.caption = element_text( size=12, color = "#4e4d47", margin = margin(b = 0.3, r=-99, unit = "cm") ),
    
    legend.position = c(0.7, 0.09)
  )
p


