# translate to the datastructure of the python one

setwd("C:/Users/du/Desktop/citibikedata_dot/")
install.packages("jsonlite")
library(jsonlite)
library(tibble)
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



#read sf
setwd("~/Desktop/nynta_20d")st_read()
nta <- st_read("nynta.shp")
# Simple feature collection with 195 features and 7 fields
# sf which means simple feature

#spatial join

nta<-st_transform(nta,4326)
plot(ntastationplot$geometry)
st_crs(nta) == st_crs(stationinfo_sf)

ntastation<-st_join(nta,stationinfo_sf)

# ntastation<-st_join(stationinfo_sf,nta)
ntastationplot<-ntastation%>%
  group_by(NTACode)%>%
  summarize(stationcount=n())


# install.packages("viridis")
library(viridis)

library(ggplot2)

ggplot() +
  geom_sf(data = ntastationplot, aes(fill =stationcount )) +
  scale_fill_viridis(discrete = F, direction = 1, option="magma")
