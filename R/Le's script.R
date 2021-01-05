library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)

setwd("C:\\Users\\Wencong\\Desktop\\WFH\\COVID impact\\Reopening\\Phase 2")

#Function to only read selected columns from large PUMS data
readSelectCol <- function(inputFilePath, selectColumns) {
  #First find out the column indexes from column names by reading just the first row#
  firstLine <- read.csv(file = inputFilePath ,
                        nrows = 1,
                        header = T)
  col_index <- c()
  for (i in 1:length(selectColumns)) {
    colPattern <- paste0("^", selectColumns[i], "$")
    col_index <-
      append(col_index, grep(colPattern, colnames(firstLine)))
  }
  return(fread(inputFilePath, select = col_index))
}

# Select only wanted columns and 80 replicate weights
selectColumns <-
  c(
    "SERIALNO",
    "SPORDER",
    "PUMA",
    "ST",
    "PWGTP",
    "COW",
    "JWMNP",
    'JWRIP',
    "JWTR",
    "ESR",
    "JWAP",
    "JWDP",
    "OCCP",
    "POWPUMA",
    "POWSP",
    "NAICSP"
  )
pwgtp <- c()
for (i in 1:80) {
  pwgtp_name <- paste0("PWGTP", i)
  selectColumns <- append(selectColumns, pwgtp_name)
}

#read NY PUMS data
NY <-
  readSelectCol(
    "C:\\Users\\Wencong\\Desktop\\WFH\\COVID impact\\ACS\\PUMS\\psam_p36.csv",
    selectColumns
  )

NY <-
  readSelectCol(
    'C:/Users/mayij/Desktop/psam_p36.csv',
    selectColumns
  )


# Yijun
df1 = read.csv('C:/Users/mayij/Desktop/psam_p36.csv', stringsAsFactors =
                 FALSE)
df1[1, 'PUMA'] = NA
k1 = df1[df1['PUMA'] == 4004, 0:10]
k2 = subset(df1, PUMA == 4004, 0:10)
k3 = df1 %>% select(0:10) %>% filter(PUMA == 4004)

df2 = read.csv(
  'C:/Users/mayij/Desktop/psam_p36.csv',
  colClasses = c('PUMA' = 'character'),
  stringsAsFactors = FALSE
)
k2 = subset(df2, PUMA == 4004, 0:10)






# select workers who either live or work in NYC
worker <-
  subset(NY, ESR == 1 &
           (
             substr(PUMA, 1, str_length(PUMA) - 2) %in% c('37', '38', '39', '40', '41') |
               (POWSP == 36 & POWPUMA %in% c('3700', '3800', '3900', '4000', '4100'))
           ))


#calculate SE from 80 replicate weights
worker_df <- as.data.frame(worker)
a <- grep("^PWGTP1$", colnames(worker))
b <- grep("^PWGTP80$", colnames(worker))
c <- grep("^PWGTP$", colnames(worker))
worker_df$SE <-
  sapply(1:nrow(worker_df), function(x) {
    sqrt(sum((worker_df[x, c(a:b)] - worker_df[x, c]) ^ 2) / 20)
  })
worker_df$MOE <- worker_df$SE * 1.645



worker_df2 <- as.data.frame(worker)
a <- grep("^PWGTP1$", colnames(worker))
b <- grep("^PWGTP80$", colnames(worker))
c <- grep("^PWGTP$", colnames(worker))
for (i in c(a:b)){
  worker_df2[i]=(worker_df2[i]-worker_df2[c])^2
}
worker_df2['SE2']=sqrt(rowSums(worker_df2[c(a:b)])/20)
worker_df2$MOE2 <- worker_df2$SE2 * 1.645




#data summary
#mode by POW by industry
ModebyPOW <-
  wo %>% group_by(powBoro, Mode2, IndustryCluster) %>% summarise(Estimate =
                                                                    sum(PWGTP), MOE = sqrt(sum(MOE ^ 2)))
ggplot(ModebyPOW, aes(fill = Mode2, y = Estimate, x = powBoro)) +  geom_bar(position =
                                                                              "stack", stat = "identity") + scale_fill_discrete(name = "Commute Mode") +
  ggtitle("Means of Transportation by Place of Work Borough") +
  xlab("Place of Work") + ylab("Number of Workers") + scale_y_continuous(
    labels = function(x)
      format(x, big.mark = ",", scientific = FALSE),
    limits = c(0, 2000000)
  ) + scale_fill_brewer(palette = "Set3") + geom_errorbar(aes(ymin = Estimate -
                                                                MOE, ymax = Estimate + MOE), position = "identity")

#residence and workplace
OD <-
  nyc %>% group_by(resBoro, powBoro) %>% summarise(Estimate = sum(PWGTP), MOE =
                                                     sqrt(sum(MOE ^ 2)))
ggplot(OD, aes(fill = resBoro, y = Estimate, x = powBoro)) +  geom_bar(position =
                                                                         "stack", stat = "identity") + scale_y_continuous(
                                                                           labels = function(x)
                                                                             format(x, big.mark = ",", scientific = FALSE),
                                                                           limits = c(0, 320000)
                                                                         ) + scale_fill_brewer(palette = "Set3")
