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






# Data Viz
# Chart
library(tidyverse)
url='https://new.mta.info/document/20441'
df=read.csv(url,stringsAsFactors=F)
df = df %>%
  mutate(Date = as.Date(Date, '%m/%d/%Y')) %>%
  mutate(Subway = as.integer(Subways..Total.Estimated.Ridership)) %>%
  mutate(Bus = as.integer(Buses..Total.Estimated.Ridership)) %>%
  arrange(Date) %>%
  select(Date, Subway, Bus) %>% 
  gather(key=Type,value=Ridership,c('Subway','Bus'))



# ggplot2
# qplot
qplot(x=Date,y=Ridership,data=df,geom='point')

# ggplot
fig=ggplot()+
  geom_line(data=df,mapping=aes(x=Date,y=Ridership,color=Type),size=0.5)+
  theme_minimal()+
  scale_color_manual(values=c('steelblue', 'tomato'))+
  scale_x_date(date_breaks='2 months',date_minor_breaks='1 month',date_labels="%b %Y",expand=c(0.01,0.01))+
  scale_y_continuous(n.breaks=5,labels=scales::label_number_si(0.1),expand=c(0.05,0.05))+
  labs(title=paste0('Subway and Bus Estimated Ridership ',format(min(df$Date),'%m/%d/%Y'),' - ',format(max(df$Date), '%m/%d/%Y')),
       caption='Source: MTA',
       color='')+
  theme(plot.title=element_text(family='sans',face='bold',size=14,hjust=0.5),
        plot.caption=element_text(size=10),
        legend.text=element_text(size=13),
        axis.title=element_text(size=12),
        axis.text=element_text(size=11),
        legend.position = 'top')
fig
ggsave('C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/mta.pdf',plot=fig,width=11,height=8.5,dpi=300)



# plotly
library(plotly)
fig=ggplotly(fig)
fig
htmlwidgets::saveWidget(fig,'C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/mta.html',selfcontained = T)





fig=px.line(df,
            x='Date',
            y=['Subway','Bus'],
            color_discrete_sequence=['tomato','steelblue'],
            title='<b>Subway and Bus Estimated Ridership '+df.iloc[0,0].strftime('%m/%d/%Y')+' - '+df.iloc[-1,0].strftime('%m/%d/%Y')+' (Source: '+"</b><a href='https://new.mta.info/coronavirus/ridership'>MTA</a>"+'<b>)</b>',
            template='plotly_white')
fig.update_layout(
  title={'font':{'family':'arial',
    'size':20,
    'color':'black'},
    'x':0.5,
    'xanchor':'center'},
  legend={'orientation':'h',
    'title':{'text':''},
    'font':{'family':'arial',
      'size':16,
      'color':'black'},
    'x':0.5,
    'xanchor':'center',
    'y':1,
    'yanchor':'bottom'},
  xaxis={'title':{'text':'Date',
    'font':{'family':'arial',
      'size':14,
      'color':'black'}},
    'tickfont':{'family':'arial',
      'size':12,
      'color':'black'},
    'fixedrange':True,
    'showgrid':True},
  yaxis={'title':{'text':'Ridership',
    'font':{'family':'arial',
      'size':14,
      'color':'black'}},
    'tickfont':{'family':'arial',
      'size':12,
      'color':'black'},
    'rangemode':'nonnegative',
    'fixedrange':True,
    'showgrid':True},
  dragmode=False,
  hovermode='x unified'
)
fig.update_traces(
  line={'width':2},
  hovertemplate='%{y:#.3s}'
)
fig.write_html(path+'index.html',include_plotlyjs='cdn')


























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
