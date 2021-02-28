# Tips
# Download and read API file
download.file(url="https://api.census.gov/data/2019/acs/acs5?get=NAME,group(B25004)&for=tract:*&in=state:36%20county:005,047,061,081,085",
              destfile='C:/Users/mayij/Desktop/temp.csv')
df=read.csv('C:/Users/mayij/Desktop/temp.csv')





# Read data
df=read.csv('https://raw.githubusercontent.com/mayijun1203/SKILLGH/master/R/psam_h36.csv',
            stringsAsFactors=F,
            colClasses=c(PUMA='character',ST='character'))





# Chunks
tt=length(nchar(readLines('C:/Users/mayij/Desktop/psam_p36.csv')))-1
cs=10000
df=data.frame()

h=as.character(read.csv('C:/Users/mayij/Desktop/psam_p36.csv',header=F,nrows=1))

for (i in 1:ceiling(tt/cs)){
  dt=read.csv('C:/Users/mayij/Desktop/psam_p36.csv',
              header=F,
              skip=1+(i-1)*cs,
              nrows=cs)
  colnames(dt)=h
  df=rbind(df,dt)
}







# data.table
library(data.table)
library(dplyr)
selectColumns=c("SERIALNO","SPORDER","PUMA","ST","PWGTP","COW","JWMNP",'JWRIP',
                "JWTRNS","ESR","JWAP","JWDP","OCCP","POWPUMA","POWSP","NAICSP",
                paste0('PWGTP',1:80))
nyPUMS=fread(file='C:/Users/mayij/Desktop/psam_p36.csv', 
             select=selectColumns,
             colClasses=c(PUMA='character',ST='character'))
nyPUMS=as.data.frame(nyPUMS)
worker=subset(nyPUMS,ESR==1)



# Looping
# For loop
starttime=Sys.time()
workerfl=worker[1:1000,]
for (i in 1:nrow(workerfl)){
  workerfl[i,'SE']=sqrt(sum((workerfl[i,paste0('PWGTP',1:80)]-workerfl[i,'PWGTP'])^2)/20)
  workerfl[i,'MOE']=workerfl[i,'SE']*1.645
}
print(Sys.time()-starttime)
# 15 secs


# Apply
starttime=Sys.time()
workerap=worker[1:1000,]
workerap$SE<-sapply(1:nrow(workerap),
                    function(x){sqrt(sum((workerap[x,paste0('PWGTP',1:80)]-workerap[x,'PWGTP'])^2)/20)})
workerap$MOE=workerap$SE*1.645
print(Sys.time()-starttime)
# 15 secs


# Vectorization
starttime=Sys.time()
workervc=worker[1:1000,]
worker_moe=workervc %>%
  select(c(paste0('PWGTP',1:80),'PWGTP'))
worker_moe[paste0('PWGTP',1:80)]=(worker_moe[paste0('PWGTP',1:80)]-worker_moe[,'PWGTP'])^2
worker_moe$RS=rowSums(worker_moe[paste0('PWGTP',1:80)])
worker_moe$SE=sqrt(worker_moe$RS/20)
worker_moe$MOE=worker_moe$SE*1.645
workervc=cbind(workervc,worker_moe[c('SE','MOE')])
print(Sys.time()-starttime)
# 0.1 secs


all.equal(workerfl,workerap)
all.equal(workerfl,workervc)










# Filter
library(tidyverse)
df=read.csv('C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/psam_h36.csv')
k1=df[df['BROADBND']==1,c('PUMA','BROADBND')]
k2=subset(df,BROADBND==1, c('PUMA','BROADBND'))
k3=df %>% select(PUMA,BROADBND) %>% filter(BROADBND==1)
head(k1)
head(k2)
head(k3)







