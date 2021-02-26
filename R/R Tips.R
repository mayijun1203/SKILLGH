# Tips
# Read data
df = read.csv(
  'C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/psam_h36.csv',
  stringsAsFactors = F,
  colClasses = c(PUMA = 'character', ST = 'character'),
)


# Chunks
h = as.character(
  read.csv(
    'C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/psam_h36.csv',
    header = F,
    nrows = 1,
    stringsAsFactors = F,
  )
)
dt = read.csv(
  'C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/psam_h36.csv',
  header = F,
  stringsAsFactors = F,
  skip = 1000,
  nrows = 1000
)
colnames(dt) = h


# Subset
library(tidyverse)
df = read.csv('C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/psam_h36.csv')
k1 = df[df['BROADBND'] == 1, c('PUMA', 'BROADBND')]
k2 = subset(df, BROADBND == 1, c('PUMA', 'BROADBND'))
k3 = df %>% select(PUMA, BROADBND) %>% filter(BROADBND == 1)
head(k1)
head(k2)
head(k3)



# Loop
# Vectorized operations
# Apply
# For loop

# Download and read API file



