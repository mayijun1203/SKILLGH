
#set working directory
setwd("C:\\Users\\Wencong\\Desktop\\WFH\\skill share")

#load packages 
library(dplyr)
library(stringr)
library(data.table)
library(ggplot2)

#Function to only read selected columns from PUMS data file
readSelectCol<-function(inputFilePath,selectColumns){
  #First find out the column indexes from column names by reading just the first row#
  firstLine<-read.csv(file =inputFilePath , nrows=1, header = T)
  col_index<-c()
  for(i in 1:length(selectColumns)){
    colPattern<-paste0("^",selectColumns[i],"$")
    col_index<-append(col_index, grep(colPattern,colnames(firstLine)))}
  return(fread(inputFilePath, select = col_index))}
 

# Select only wanted columns and 80 replicate weights
selectColumns<-c("SERIALNO","SPORDER","PUMA","ST","PWGTP","COW","JWMNP",'JWRIP',"JWTR","ESR","JWAP","JWDP","OCCP","POWPUMA","POWSP","NAICSP")
pwgtp<-c()
for(i in 1:80){
  pwgtp_name<-paste0("PWGTP",i)
  selectColumns<-append(selectColumns,pwgtp_name)}


#read data
nyPUMS<-readSelectCol("data\\psam_p36.csv", selectColumns)

  
#filter workers
worker<-subset(nyPUMS,ESR==1)
#worker<-nyPUMS%>%filter(ESR==1)
  

#calculate SE and MOE from replicate weights
  worker_df <- as.data.frame(worker)
  a<-grep("^PWGTP1$",colnames(worker))
  b<-grep("^PWGTP80$",colnames(worker))
  c<-grep("^PWGTP$",colnames(worker))
  worker_df$SE<-sapply(1:nrow(worker_df), function(x){sqrt(sum((worker_df[x, c(a:b)] - worker_df[x, c])^2)/20)})
  worker_df$MOE<-worker_df$SE*1.645
  
  
#create a county variable
worker$county<-substr(worker$PUMA,1, str_length(worker$PUMA)-2)
nycworker<-subset(worker, county %in% c('37','38','39','40','41'))

#decode the counties
borough<-read.csv("data//borough.csv",stringsAsFactors = F)
nycworker<-merge(nycworker, borough, by.x="county", by.y="CountyID", all.x = T)

#double check the results
  table(nycworker$county, nycworker$Borough)
  
#use a processed dataset
  worker_sample<-read.csv("sample worker data file.csv", stringsAsFactors = F)

#mode by POW by industry
  ModebyPOW<-worker_sample%>%group_by(powBoro, Mode2, IndustryCluster)%>%summarise(Estimate=sum(PWGTP), MOE=sqrt(sum(MOE^2)))
  ggplot(ModebyPOW, aes(fill=Mode2, y=Estimate, x=powBoro)) +  geom_bar(position="stack", stat="identity")+scale_fill_discrete(name="Commute Mode")+ggtitle("Means of Transportation by Place of Work Borough") +
    xlab("Place of Work") + ylab("Number of Workers")+ scale_y_continuous(labels=function(x) format(x, big.mark = ",", scientific = FALSE), limits = c(0, 20000))+ scale_fill_brewer(palette="Set3")
  