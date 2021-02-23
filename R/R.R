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
  gather(key=Type,value=Ridership,c('Subway','Bus')) %>%
  mutate(Type2=factor(Type,levels=c('Subway','Bus')))



# ggplot2
library(ggplot2)

# qplot
qplot(data=df,x=Date,y=Ridership,color=Type,geom='line')

# ggplot
p=ggplot()+
  theme_minimal()+
  geom_line(data=df,mapping=aes(x=Date,y=Ridership,color=Type2),size=0.5)+
  scale_color_manual(values=c('Bus'='steelblue', 'Subway'='tomato'))+
  scale_x_date(date_breaks='2 months',date_minor_breaks='1 month',date_labels="%b %Y",expand=c(0.01,0.01))+
  scale_y_continuous(n.breaks=5,labels=scales::label_number_si(0.1),expand=c(0.05,0.05))+
  labs(title=paste0('Subway and Bus Estimated Ridership ',format(min(df$Date),'%m/%d/%Y'),' - ',format(max(df$Date), '%m/%d/%Y')),
       caption='Source: MTA',
       color='')+
  theme(plot.title=element_text(family='sans',face='bold',size=14,hjust=0.5),
        plot.caption=element_text(size=10),
        legend.text=element_text(size=13),
        legend.position='top',
        axis.title=element_text(size=12),
        axis.text=element_text(size=11))
p
ggsave('C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/mta.pdf',plot=p,width=11,height=8.5,dpi=300)







# plotly
# from ggplot
library(plotly)
p=ggplotly(p)
p
htmlwidgets::saveWidget(p,'C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/mta.html')





# from scratch
p=plot_ly(data=df,x=~Date,y=df~Ridership,color=~Type,type='scatter',mode='lines')
p



p=plot_ly()%>%
  add_trace(data=subset(df,Type=='Subway'),
            x=~Date,
            y=~Ridership,
            type='scatter',
            mode='lines',
            name='Subway',
            line=list(width=2,
                      color='tomato'),
            hovertemplate='%{y:#.3s}') %>%
  add_trace(data=subset(df,Type=='Bus'),
            x=~Date,
            y=~Ridership,
            type='scatter',
            mode='lines',
            name='Bus',
            line=list(width=2,
                      color='steelblue'),
            hovertemplate='%{y:#.3s}') %>%
  layout(title=list(text=paste0('<b>Subway and Bus Estimated Ridership ',format(min(df$Date),'%m/%d/%Y'),' - ',format(max(df$Date), '%m/%d/%Y'),' (Source: ',"</b><a href='https://new.mta.info/coronavirus/ridership'>MTA</a>",'<b>)</b>'),
                    font=list(family='arial',
                              size=20,
                              color='black'),
                    x=0.5,
                    xanchor='center'),
         xaxis=list(title=list(text='Date',
                               font=list(family='arial',
                                         size=14,
                                         color='black')),
                    tickfont=list(family='arial',
                                  size=12,
                                  color='black'),
                    fixedrange=T,
                    showgrid=T),
         yaxis=list(title=list(text='Ridership',
                               font=list(family='arial',
                                         size=14,
                                         color='black')),
                    tickfont=list(family='arial',
                                  size=12,
                                  color='black'),
                    rangemode='nonnegative',
                    fixedrange=T,
                    showgrid=T,
                    zeroline=T,
                    zerolinecolor="#eee",
                    zerolinewidth=3),
         legend=list(orientation='h',
                     font=list(family='arial',
                               size=16,
                               color='black'),
                     x=0.5,
                     xanchor='center',
                     y=1,
                     yanchor='bottom'),
         margin=list(l=100,
                     r=100,
                     t=100,
                     b=100),
         dragmode=F,
         hovermode='x unified')
p
htmlwidgets::saveWidget(p,'C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/mta.html')

















# Spatial
library(tidyverse)
library(sf)
df=st_read('C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/CARTO/subwayridership.geojson')
df=st_set_crs(df,4326)

zcta=st_read('C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/CARTO/zcta.geojson')
zcta=st_set_crs(zcta,4326)
zcta$test=1:nrow(zcta)

df=st_join(df,zcta,join=st_intersects,left=T)
df$DiffPctCat2=factor(df$DiffPctCat,levels=c('<=5%','6%~10%','11%~15%','16%~20%','>20%'))


#ggplot2
library(ggplot2)
p=ggplot()+
  theme_void()+
  geom_sf(data=zcta,color=NA,fill='grey90')+
  geom_sf(data=df,mapping=aes(size=E202010,shape=Borough,color=DiffPctCat2))+
  coord_sf(xlim=st_bbox(df)[c('xmin','xmax')],ylim=st_bbox(df)[c('ymin','ymax')])+
  scale_size_continuous(range=c(1,5))+
  scale_shape_manual(values=c('Bk'=15,'Bx'=16,'M'=17,'Q'=18,'S'=4))+
  scale_color_brewer(type='seq',palette='YlGnBu')+
  labs(title='Subway Ridership',
       size='Oct 2020 Entries',
       shape='Boro',
       color='Percent Change')+
  theme(plot.title=element_text(family='sans',face='bold',size=14,hjust=0.5),
        legend.position='right',
        legend.text=element_text(size=13))

p
ggsave('C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/subway.pdf',plot=p,width=11,height=8.5,dpi=300)






# plotly
library(plotly)
p=plot_ly(data=df,
            color=~DiffPctCat2,
            type='scattermapbox',
            mode='markers',
            marker=list(symbol='circle')) %>%
  layout(mapbox=list(style='carto-positron',
                     center=list(lon=(st_bbox(df)['xmin']+st_bbox(df)['xmax'])/2,
                                 lat=(st_bbox(df)['ymin']+st_bbox(df)['ymax'])/2),
                     zoom=9.5))
p










# Polygon
# Continuous
library(sf)
library(geojsonsf)
library(rjson)
library(plotly)
zcta=st_read('C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/CARTO/zcta.geojson')
zcta=st_set_crs(zcta,4326)
zcta$test=1:nrow(zcta)
zctajs=fromJSON(sf_geojson(zcta))

p=plot_ly() %>%
  add_trace(type='choroplethmapbox',
            name='',
            geojson=zctajs,
            featureidkey='properties.ZCTA5CE10',
            locations=zcta$ZCTA5CE10,
            z=zcta$test,
            colorscale='Viridis',
            marker=list(line=list(color='white',
                                  width=0.1)),
            colorbar=list(lenmode='fraction',
                          len=1,
                          y=0.5,
                          yanchor='middle',
                          title=list(text='Test',
                                     font=list(family='arial',
                                               size=16,
                                               color='black')),
                          tickfont=list(family='arial',
                                        size=12,
                                        color='black')),
            reversescale=T,
            hovertemplate='ZCTA: %{location}<br>Test: %{z:#.2f}') %>%
  layout(mapbox=list(style='carto-positron',
                     center=list(lon=(st_bbox(zcta)['xmin']+st_bbox(zcta)['xmax'])/2,
                                 lat=(st_bbox(zcta)['ymin']+st_bbox(zcta)['ymax'])/2),
                     zoom=8),
         title=list(text='<b>ZCTA Test</b>',
                    font=list(family='arial',
                              size=20,
                              color='black'),
                    x=0.5,
                    xanchor='center'),
         margin=list(l=50,
                     r=50,
                     t=50,
                     b=50))
p
htmlwidgets::saveWidget(p,'C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/subway_continuous.html')





# Discrete
library(sf)
library(geojsonsf)
library(rjson)
library(plotly)
zcta=st_read('C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/CARTO/zcta.geojson')
zcta=st_set_crs(zcta,4326)
zcta$test=1:nrow(zcta)
zcta$test2=rep(c(1,2,3),162)
zctaa=zcta[zcta$test2==1,]
zctaajs=fromJSON(sf_geojson(zctaa))
zctab=zcta[zcta$test2==2,]
zctabjs=fromJSON(sf_geojson(zctab))
zctac=zcta[zcta$test2==3,]
zctacjs=fromJSON(sf_geojson(zctac))

p=plot_ly() %>%
  add_trace(type='choroplethmapbox',
            name='',
            geojson=zctaajs,
            featureidkey='properties.ZCTA5CE10',
            locations=zctaa$ZCTA5CE10,
            z=zctaa$test2,
            colorscale=list(c(0,'rgba(255,0,0,0.8)'),list(1,'rgba(255,0,0,0.8)')),
            marker=list(line=list(color='white',
                                  width=0.1)),
            colorbar=list(thicknessmode='pixels',
                          thickness=30,
                          lenmode='pixels',
                          len=30,
                          y=0.55,
                          yanchor='middle',
                          outlinewidth=0,
                          tickvals=c(1),
                          ticktext=c('a'),
                          ticklen=0,
                          tickfont=list(family='arial',
                                        size=16,
                                        color='black')),
            hovertemplate='ZCTA: %{location}<br>Test: %{z:#.2f}') %>%
  add_trace(type='choroplethmapbox',
            name='',
            geojson=zctabjs,
            featureidkey='properties.ZCTA5CE10',
            locations=zctab$ZCTA5CE10,
            z=zctab$test2,
            colorscale=list(c(0,'rgba(255,255,0,0.8)'),list(1,'rgba(255,255,0,0.8)')),
            marker=list(line=list(color='white',
                                  width=0.1)),
            colorbar=list(thicknessmode='pixels',
                          thickness=30,
                          lenmode='pixels',
                          len=30,
                          y=0.5,
                          yanchor='middle',
                          outlinewidth=0,
                          tickvals=c(2),
                          ticktext=c('b'),
                          ticklen=0,
                          tickfont=list(family='arial',
                                        size=16,
                                        color='black')),
            hovertemplate='ZCTA: %{location}<br>Test: %{z:#.2f}') %>%
  add_trace(type='choroplethmapbox',
            name='',
            geojson=zctacjs,
            featureidkey='properties.ZCTA5CE10',
            locations=zctac$ZCTA5CE10,
            z=zctac$test2,
            colorscale=list(c(0,'rgba(0,0,255,0.8)'),list(1,'rgba(0,0,255,0.8)')),
            marker=list(line=list(color='white',
                                  width=0.1)),
            colorbar=list(thicknessmode='pixels',
                          thickness=30,
                          lenmode='pixels',
                          len=30,
                          y=0.45,
                          yanchor='middle',
                          outlinewidth=0,
                          tickvals=c(3),
                          ticktext=c('c'),
                          ticklen=0,
                          tickfont=list(family='arial',
                                        size=16,
                                        color='black')),
            hovertemplate='ZCTA: %{location}<br>Test: %{z:#.2f}') %>%
  layout(mapbox=list(style='carto-positron',
                     center=list(lon=(st_bbox(zcta)['xmin']+st_bbox(zcta)['xmax'])/2,
                                 lat=(st_bbox(zcta)['ymin']+st_bbox(zcta)['ymax'])/2),
                     zoom=8),
         title=list(text='<b>ZCTA Test</b>',
                    font=list(family='arial',
                              size=20,
                              color='black'),
                    x=0.5,
                    xanchor='center'),
         margin=list(l=50,
                     r=50,
                     t=50,
                     b=50))
p
htmlwidgets::saveWidget(p,'C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/subway_discrete.html')






















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
