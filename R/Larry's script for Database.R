# SQLite
library(RSQLite)
con=dbConnect(SQLite(),'C:/Users/mayij/Desktop/test')
dbListTables(con)


df=read.csv('https://raw.githubusercontent.com/mayijun1203/SKILLGH/master/R/psam_h36.csv',
            stringsAsFactors=F,
            colClasses=c(PUMA='character',ST='character'))
dbWriteTable(con,'pums',df)
dbListTables(con)


df=dbReadTable(con,'pums')


df=dbGetQuery(con, 'SELECT SERIALNO,PUMA FROM pums LIMIT 100')


dbDisconnect(con)






# SpatiaLite
library(RSQLite)
library(sf)
con=dbConnect(SQLite(),'C:/Users/mayij/Desktop/testspatial.sqlite')
dbListTables(con)

df=st_read('https://raw.githubusercontent.com/mayijun1203/SKILLGH/master/CARTO/subwayridership.geojson')
df=st_set_crs(df,4326)
st_write(df,con,'subway')
dbListTables(con)


df=st_read(con,'subway')
plot(df)

df=st_read(con,query='SELECT CplxID,geometry FROM subway LIMIT 100')
plot(df)


dbDisconnect(con)











##cross validation 2

##weekly update
setwd("C:/Users/du/Desktop/NEWLY/2019/")

library(RSQLite)
library(dplyr)
# Create an ephemeral in-memory RSQLite database
con <- dbConnect(RSQLite::SQLite(), "CITI20190307.sqlite3")
dbListTables(con)

# 
# dbSendQuery(con,"DELETE FROM trip where startdate >='2019-10-31'")

tripsdailyall2 <- dbGetQuery(con, "SELECT startdate as startdate, count(*) From trip Group by startdate")

# tripsdailyall <- dbGetQuery(con, "SELECT count(*) From trip")
dbDisconnect(con)





conn = sqlite3.connect(path + 'maroctod2019.sqlite3')
