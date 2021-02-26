

###
# translate to the datastructure of the python one

setwd("C:/Users/du/Desktop/citibikedata_dot/")
library(jsonlite)
library(tibble)
library(dplyr)
# stationinfo <- fromJSON("station_information.json")
stationinfo <- fromJSON("https://gbfs.citibikenyc.com/gbfs/en/station_information.json")

stationinfo<-stationinfo$data

stationinfo<-stationinfo$stations
stationinfo <- as_data_frame(stationinfo)

setwd("C:/Users/du/Desktop/citibikedata_dot/dotRcitibike")
# # # df<-read.csv("CitiBike_2020_DCP_0608.csv")
# # df<-read.csv("CitiBike_2020_DCP_0615.csv")
# # df<-read.csv("CitiBike_2020_DCP_0622.csv")
# df<-read.csv("CitiBike_2020_DCP_0629.csv")
# 
# # df<-read.csv("CitiBike_2020_DCP_0706.csv")
# df<-read.csv("CitiBike_2020_DCP_0713.csv")
# df<-read.csv("CitiBike_2020_DCP_0727.csv")


# df<-read.csv("CitiBike_2020_DCP_0810.csv")
# 
# 
# # # 
# # # 
# # 
# # 8/10-8/16
# # df1<-read.csv("0808_0816.csv")
# # df1$date<-format(strptime(df1$start_time,"%Y/%m/%d %H:%M"),'%m/%d')
# # max(df1$date)
# # 
# # df1<-df1%>%
# #   filter(date>="08/10" & date<="08/16") 
# # unique(df1$date)
# # 
# # ### 8/17-831
# # df2<-read.csv("817_831.csv")
# # df2$date<-format(strptime(df2$start_time,"%Y/%m/%d %H:%M"),'%m/%d')
# # max(df2$date)
# # df2<-df2%>%
# #   filter(date>="08/17" & date<="08/30") 
# # unique(df2$date)
# # # 
# # 0831-0914
# df3<-read.csv("831_914.csv")
# df3$date<-format(strptime(df3$start_time,"%Y/%m/%d %H:%M"),'%m/%d')
# unique(df3$date)
# max(df3$date)
# df3<-df3%>%
#   filter(date>="09/01" & date<="09/13")
# 
# # 0831-0914
# df4<-read.csv("914-921.csv")
# df4$date<-format(strptime(df4$start_time,"%Y/%m/%d %H:%M"),'%m/%d')
# unique(df4$date)
# max(df4$date)
# df4<-df4%>%
#   filter(date>="09/14" & date<="09/20")
# 
# 0914-0928
# df5<-read.csv("0914-0928.csv")
# df5$date<-format(strptime(df5$start_time,"%Y/%m/%d %H:%M"),'%m/%d')
# unique(df5$date)
# max(df5$date)
# df5<-df5%>%
#   filter(date>="09/21" & date<="09/27")
# dftripsdaily<-df5%>%
#   group_by(date)%>%
#   summarise(trips=n())
# 
# 
# # 
# # 
# # 
# # ###
# # most recent
# df4<-read.csv("lastweek.csv")
# df4$date<-format(strptime(df4$start_time,"%Y/%m/%d %H:%M"),'%m/%d')
# unique(df4$date)
# max(df4$date)
# df4<-df4%>%
#   filter(date>="09/14")
# 
# 
# unique(df1$date)
# unique(df2$date)
# # unique(df3$date)
# # unique(df4$date)
# df<-rbind(df3,df4,df5)
# 
# unique(df$date)
# # 
# ###
# df5<-read.csv("CitiBike_2020_DCP_0630.csv")
# df5$date<-format(strptime(df5$start_time,"%Y/%m/%d %H:%M"),'%m/%d')
# unique(df5$date)
# max(df5$date)
# df5<-df5%>%
#   filter(date>max(df4$date))
# df6<-df5[,-10]
# 
# dfall<-rbind(df1,df2,df3,df4,df6)
# write.csv(dfall,"dot04_0628.csv")

# # dfall3<-rbind(dfall,df)
# write.csv(dfall3,"dot04_0712.csv")



df<-read.csv("1116.csv")





df$date<-format(strptime(df$start_time,"%Y/%m/%d %H:%M"),'%m/%d')
# df<-df[,-10]
dftripsdaily<-df%>%
  group_by(date)%>%
  summarise(trips=n())

#update the total citi bike slide


# df<-dfall3


# "file in the sharepoint:
# https://nyco365.sharepoint.com/:x:/r/sites/NYCPLANNING/transportation/_layouts/15/Doc.aspx?sourcedoc=%7B31E89BA6-9FEF-4EBA-AABC-ABA4AB26BD31%7D&file=p5%20citi%20bike%20trip%20total.xlsx&wdOrigin=OFFICECOM-WEB.MAIN.MRU&action=default&mobileredirect=true
# 
# "


stationinfo <- as_data_frame(stationinfo)
df$logicalTerminal<- as.character(df$logicalTerminal)
dfjoin1<-left_join(df,stationinfo,by=c("logicalTerminal"="short_name"))%>%
  rename("startstationid"="station_id","startstationname"="name",'startstationlat'="lat",'startstationlong'="lon")
df$logicalTerminal.1<- as.character(df$logicalTerminal.1)
dfjoin2<-left_join(dfjoin1,stationinfo,by=c("logicalTerminal.1"="short_name"))%>%
  rename("tripduration"="duration","starttime"="start_time","endtime"="end_time","endstationid"="station_id","endstationname"="name",'endstationlat'="lat",'endstationlong'="lon")

dotdatatranslated<-dfjoin2[,c('tripduration', 'starttime', 'endtime', 'startstationid', 'startstationname', 'startstationlat','startstationlong', 'endstationid', 'endstationname', 'endstationlat', 'endstationlong')]
dotdatatranslated<-dotdatatranslated%>%
  filter(endtime!="\\N" )
dotdatatranslated$date<-format(strptime(dotdatatranslated$starttime,"%Y/%m/%d %H:%M"),'%m/%d')
unique(dotdatatranslated$date)
# dotdatatranslated<-dotdatatranslated%>%
#   filter(date>="08/03")
dotdatatranslated<-dotdatatranslated[,-12]
write.csv(dotdatatranslated,"dotdatatranslated.csv")

# 
# test22<-dotdatatranslated%>%
#   filter(endstationid=="3948")












##cross validation

##weekly update
setwd("C:/Users/du/Desktop/citibikedata_dot/dotRcitibike")

library(RSQLite)
library(dplyr)
# Create an ephemeral in-memory RSQLite database
con <- dbConnect(RSQLite::SQLite(), "citibikedata_dot.sqlite3")
dbListTables(con)


tripsdailyall <- dbGetQuery(con, "SELECT startdate as startdate, count(*) From trip Group by startdate")

dbDisconnect(con)












##cross validation 2

##weekly update
setwd("C:/Users/du/Desktop/citibikedata_dot/ODanalysis/")

library(RSQLite)
library(dplyr)
# Create an ephemeral in-memory RSQLite database
con <- dbConnect(RSQLite::SQLite(), "CITI2020phase2.sqlite3")
dbListTables(con)


tripsdailyall2 <- dbGetQuery(con, "SELECT startdate as startdate, count(*) From trip Group by startdate")

# tripsdailyall <- dbGetQuery(con, "SELECT count(*) From trip")
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
