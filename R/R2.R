library(dplyr)


# Help
? read.csv


# Assign value to variable
k <- 1
k = 1
k = c(1, 2, 3)
k = c(1, 2, 'a')
k = list(1, 2, 'a')


# Read data
data.frame()
df = read.csv('C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/psam_h36.csv')


# Structure
head(df)
tail(df)
str(df)
summary(df)


# Slicing
df1 = df[1:10, 1:2]
df1 = df[c(1, 3, 5), c('PUMA', 'BROADBND')]
df1 = df[c('PUMA', 'BROADBND')]
df1 = df$PUMA


# Filtering
df2 = df[df['PUMA'] == 4004, ]
df2 = subset(df, PUMA == 4004)
df2 = filter(df, PUMA == 4004)
df2 = df %>% filter(PUMA == 4004)


#	Select columns
df3 = df[, c('PUMA', 'BROADBND')]
df3 = select(df, PUMA, BROADBND)
df3 = df %>% select(PUMA, BROADBND)


# Create column
df$INCWGTP = df$ADJINC * df$WGTP
df4 = mutate(df, INCWGTP = ADJINC * WGTP)
df4 = df %>% mutate(INCWGTP = ADJINC * WGTP)


# Piping
df5 = df %>% filter(PUMA == 4004) %>% select(PUMA, ADJINC, WGTP)
df5 = df %>% filter(PUMA == 4004) %>% select(PUMA, ADJINC, WGTP) %>% mutate(INCWGTP = ADJINC * WGTP)


# Group by and summarize
df6 = df %>% group_by(PUMA) %>% summarize(WEIGHT = sum(WGTP))


# Join
df71=df %>% filter(PUMA == 4004) %>% select(SERIALNO, ADJINC)
df72=df %>% filter(PUMA == 4004) %>% select(SERIALNO, WGTP)
df7=merge(df71,df72,by='SERIALNO',all=T)
df7=inner_join(df71,df72,by='SERIALNO')


# For loop
df6$PUMA100 = NaN
for (i in 1:5) {
  df6[i, 'PUMA100'] = df6[i, 'PUMA'] * 100
}


# if else
df6$PUMACAT = NaN
for (i in 1:10) {
  if (df6[i, 'PUMA'] == 401) {
    df6[i, 'PUMACAT'] = 1
  }
  else if (df6[i, 'PUMA'] == 402) {
    df6[i, 'PUMACAT'] = 2
  }
  else
    df6[i, 'PUMACAT'] = 0
}


# Write data
write.csv(df6,
          'C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/df6.csv')






# Tips
# Read data
df = read.csv(
  'C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/psam_h36.csv',
  stringsAsFactors = F,
  colClasses = c(PUMA = 'character', ST = 'character'),
)

h = as.character(
  read.csv(
    'C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/psam_h36.csv',
    header = FALSE,
    nrows = 1,
    stringsAsFactors = FALSE,
  )
)
dt = read.csv(
  'C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/psam_h36.csv',
  header = FALSE,
  stringsAsFactors = FALSE,
  skip = 1000,
  nrows = 1000
)
colnames(dt) = h


# Subset
k1 = df[df['BROADBND'] == 1, c('PUMA', 'BROADBND')]
k2 = subset(df, BROADBND == 1, c('PUMA', 'BROADBND'))
k3 = df %>% select(PUMA, BROADBND) %>% filter(BROADBND == 1)


# ifelse
df4 = df %>% select(PUMA) %>% mutate(PUMA4004 = ifelse(PUMA == 4004, 1, 0))
df4 = df %>% select(PUMA) %>% mutate(PUMACAT = ifelse(PUMA == 4004, 1, ifelse(PUMA ==
                                                                                4010, 2, 0)))
