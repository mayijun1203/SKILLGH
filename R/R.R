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
# quick mapping
p=plot_ly(data=df,x=~Date,y=~Ridership,split=~Type2,type='scatter',mode='lines')
p



# with layers
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
                    zerolinecolor='#eee',
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

















# Mapping
library(tidyverse)
library(sf)
df=st_read('https://raw.githubusercontent.com/mayijun1203/SKILLGH/master/CARTO/subwayridership.geojson')
df=st_set_crs(df,4326)

zcta=st_read('https://raw.githubusercontent.com/mayijun1203/SKILLGH/master/CARTO/zcta.geojson')
zcta=st_set_crs(zcta,4326)
zcta$test=1:nrow(zcta)

df=st_join(df,zcta,join=st_intersects,left=T)
df$DiffPctCat2=factor(df$DiffPctCat,levels=c('<=5%','6%~10%','11%~15%','16%~20%','>20%'))

plot(df)



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
# Point
# Discrete Value
library(sf)
library(plotly)
df=st_read('https://raw.githubusercontent.com/mayijun1203/SKILLGH/master/CARTO/subwayridership.geojson')
df=st_set_crs(df,4326)
df$DiffPctCat2=factor(df$DiffPctCat,levels=c('<=5%','6%~10%','11%~15%','16%~20%','>20%'))
df$HoverText=paste0('<b>Station: </b>',
                    df$CplxName,
                    '<br>',
                    '<b>Sept 2020: </b>',
                    df$E202009,
                    '<br>',
                    '<b>Oct 2020: </b>',
                    df$E202010,
                    '<br>',
                    '<b>% Change: </b>',
                    round(df$DiffPct*100,2),
                    '%')

p=plot_ly() %>%
  add_sf(type='scattermapbox',
         name='<=5%',
         data=subset(df,DiffPctCat2=='<=5%'),
         mode='markers',
         marker=list(size=8,
                     color='#fff5eb'),
         hovertext=~HoverText,
         hoverinfo='text') %>%
  add_sf(type='scattermapbox',
         name='6%~10%',
         data=subset(df,DiffPctCat2=='6%~10%'),
         mode='markers',
         marker=list(size=8,
                     color='#fed2a6'),
         hovertext=~HoverText,
         hoverinfo='text') %>%
  add_sf(type='scattermapbox',
         name='11%~15%',
         data=subset(df,DiffPctCat2=='11%~15%'),
         mode='markers',
         marker=list(size=8,
                     color='#fd9243'),
         hovertext=~HoverText,
         hoverinfo='text') %>%
  add_sf(type='scattermapbox',
         name='16%~20%',
         data=subset(df,DiffPctCat2=='16%~20%'),
         mode='markers',
         marker=list(size=8,
                     color='#df4f05'),
         hovertext=~HoverText,
         hoverinfo='text') %>%
  add_sf(type='scattermapbox',
         name='>20%',
         data=subset(df,DiffPctCat2=='>20%'),
         mode='markers',
         marker=list(size=8,
                     color='#7f2704'),
         hovertext=~HoverText,
         hoverinfo='text') %>%
  layout(mapbox=list(style='carto-positron',
                     center=list(lon=(st_bbox(df)['xmin']+st_bbox(df)['xmax'])/2,
                                 lat=(st_bbox(df)['ymin']+st_bbox(df)['ymax'])/2),
                     zoom=9.5),
         title=list(text='<b>Discrete Point</b>',
                    font=list(size=20),
                    x=0.5,
                    xanchor='center'),
         legend=list(orientation='v',
                     font=list(size=16),
                     x=1,
                     xanchor='right',
                     y=1,
                     yanchor='top'),
         font=list(family='arial',
                   color='black'),
         margin=list(l=50,
                     r=50,
                     t=50,
                     b=50))
p
htmlwidgets::saveWidget(p,'C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/discrete_point.html')





# Continuous Value
library(sf)
library(plotly)
df=st_read('https://raw.githubusercontent.com/mayijun1203/SKILLGH/master/CARTO/subwayridership.geojson')
df=st_set_crs(df,4326)

p=plot_ly()%>%
  add_sf(type='scattermapbox',
         data=df,
         mode='markers',
         marker=list(color=~DiffPct,
                     cmin=-0.1,
                     cmax=0.3,
                     colorscale=list(c(0,'rgb(215,25,28)'),
                                     c(1/4,'rgb(250,250,250)'),
                                     c(1,'rgb(43,131,186)')),
                     colorbar=list(lenmode='fraction',
                                   len=1,
                                   y=0.5,
                                   yanchor='middle',
                                   title=list(text='Percent Change',
                                              font=list(size=16)),
                                   tickvals=c(-0.1,0,0.1,0.2,0.3),
                                   ticktext=c('<=-10%','0%','10%','20%','>=30%'),
                                   ticklen=5,
                                   tickfont=list(size=12),
                                   outlinewidth=0),
                     showscale=T),
         hovertext=~paste0('<b>Station: </b>',
                           df$CplxName,
                           '<br>',
                           '<b>Sept 2020: </b>',
                           df$E202009,
                           '<br>',
                           '<b>Oct 2020: </b>',
                           df$E202010,
                           '<br>',
                           '<b>% Change: </b>',
                           round(df$DiffPct*100,2),
                           '%'),
         hoverinfo='text') %>%
  layout(mapbox=list(style='carto-positron',
                     center=list(lon=(st_bbox(df)['xmin']+st_bbox(df)['xmax'])/2,
                                 lat=(st_bbox(df)['ymin']+st_bbox(df)['ymax'])/2),
                     zoom=9.5),
         title=list(text='<b>Continuous Point</b>',
                    font=list(size=20),
                    x=0.5,
                    xanchor='center'),
         font=list(family='arial',
                   color='black'),
         margin=list(l=50,
                     r=50,
                     t=50,
                     b=50))
p
htmlwidgets::saveWidget(p,'C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/continuous_point.html')








# Line
# Discrete Value
library(sf)
library(plotly)
sl=st_read('https://raw.githubusercontent.com/mayijun1203/SKILLGH/master/CARTO/subway_line.geojson')
sl=st_set_crs(sl,4326)
sl=mutate(sl,div2=factor(ifelse(is.na(div),'SIR',div),levels=c('BMT','IRT','IND','SIR')))
sl$HoverText=paste0('<b>Division: </b>',
                    sl$div2,
                    '<br>',
                    '<b>Route: </b>',
                    sl$oem_route,
                    '<br>',
                    '<b>Name: </b>',
                    sl$name)

cat=c('BMT','IRT','IND','SIR')
catcolor=c('blue','red','green','purple')

p=plot_ly()
for (i in 1:length(cat)){
  p=p %>%
    add_sf(type='scattermapbox',
           name=cat[i],
           data=subset(sl,div2==cat[i]),
           mode='lines',
           line=list(width=2,
                     color=catcolor[i]),
           hovertext=~HoverText,
           hoverinfo='text')
}
p=p %>%
  layout(mapbox=list(style='carto-positron',
                     center=list(lon=(st_bbox(sl)['xmin']+st_bbox(sl)['xmax'])/2,
                                 lat=(st_bbox(sl)['ymin']+st_bbox(sl)['ymax'])/2),
                     zoom=9.5),
         title=list(text='<b>Discrete Line</b>',
                    font=list(size=20),
                    x=0.5,
                    xanchor='center'),
         legend=list(orientation='v',
                     font=list(size=16),
                     x=1,
                     xanchor='right',
                     y=1,
                     yanchor='top'),
         font=list(family='arial',
                   color='black'),
         margin=list(l=50,
                     r=50,
                     t=50,
                     b=50))
p
htmlwidgets::saveWidget(p,'C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/discrete_line.html')





# Continuous Value
library(sf)
library(geojsonsf)
library(rjson)
library(plotly)
sl=st_read('https://raw.githubusercontent.com/mayijun1203/SKILLGH/master/CARTO/subway_line.geojson')
sl=st_set_crs(sl,4326)
sl=st_transform(sl,6539)
sl=st_buffer(sl,200,endCapStyle='FLAT')
sl=st_transform(sl,4326)
sljs=fromJSON(sf_geojson(sl))

p=plot_ly() %>%
  add_trace(type='choroplethmapbox',
            name='',
            geojson=sljs,
            featureidkey='properties.cartodb_id',
            locations=sl$cartodb_id,
            z=sl$shape_stle,
            zmin=0,
            zmax=5000,
            colorscale='Viridis',
            colorbar=list(lenmode='fraction',
                          len=1,
                          y=0.5,
                          yanchor='middle',
                          title=list(text='Shape Length',
                                     font=list(size=16)),
                          ticklen=5,
                          tickfont=list(size=12),
                          outlinewidth=0),
            reversescale=T,
            marker=list(line=list(width=0)),
            hovertemplate='<b>Segment ID: </b>%{location}<br><b>Shape Length: </b>%{z:#.2f}') %>%
  layout(mapbox=list(style='carto-positron',
                     center=list(lon=(st_bbox(sl)['xmin']+st_bbox(sl)['xmax'])/2,
                                 lat=(st_bbox(sl)['ymin']+st_bbox(sl)['ymax'])/2),
                     zoom=9.5),
         title=list(text='<b>Continuous Line</b>',
                    font=list(size=20),
                    x=0.5,
                    xanchor='center'),
         font=list(family='arial',
                   color='black'),
         margin=list(l=50,
                     r=50,
                     t=50,
                     b=50))
p
htmlwidgets::saveWidget(p,'C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/continuous_line.html')





# Polygon
# Discrete Value
library(sf)
library(geojsonsf)
library(rjson)
library(plotly)
zcta=st_read('https://raw.githubusercontent.com/mayijun1203/SKILLGH/master/CARTO/zcta.geojson')
zcta=st_set_crs(zcta,4326)
zcta$test=sample(c('a','b','c'),nrow(zcta),replace=T)

cat=c('a','b','c')
catval=c(1,2,3)
catcolor=c('rgba(215,25,28,0.8)','rgba(255,255,191,0.8)','rgba(43,131,186,0.8)')

p=plot_ly()
for (i in 1:length(cat)[1]){
  p=p %>%
    add_trace(type='choroplethmapbox',
              name=cat[i],
              geojson=fromJSON(sf_geojson(zcta[zcta$test==cat[i],])),
              featureidkey='properties.ZCTA5CE10',
              locations=zcta[zcta$test==cat[i],][['ZCTA5CE10']],
              z=catval[i],
              colorscale=list(c(0,catcolor[i]),list(1,catcolor[i])),
              colorbar=list(thicknessmode='pixels',
                            thickness=30,
                            lenmode='pixels',
                            len=30,
                            y=0.5+0.03*(length(cat)-2*i+1),
                            yanchor='middle',
                            outlinewidth=0,
                            tickvals=catval[i],
                            ticktext=cat[i],
                            ticklen=0,
                            tickfont=list(size=16)),
              marker=list(line=list(color='white',
                                    width=0.1)),
              hovertemplate='<b>ZCTA: </b>%{location}')
}
p=p %>%
  layout(mapbox=list(style='carto-positron',
                     center=list(lon=(st_bbox(zcta)['xmin']+st_bbox(zcta)['xmax'])/2,
                                 lat=(st_bbox(zcta)['ymin']+st_bbox(zcta)['ymax'])/2),
                     zoom=8),
         title=list(text='<b>Discrete Polygon</b>',
                    font=list(size=20),
                    x=0.5,
                    xanchor='center'),
         font=list(family='arial',
                   color='black'),
         margin=list(l=50,
                     r=50,
                     t=50,
                     b=50))
p
htmlwidgets::saveWidget(p,'C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/discrete_polygon.html')








# Continuous Value
library(sf)
library(geojsonsf)
library(rjson)
library(plotly)
zcta=st_read('https://raw.githubusercontent.com/mayijun1203/SKILLGH/master/CARTO/zcta.geojson')
zcta=st_set_crs(zcta,4326)
zcta$test=sample(1:100,nrow(zcta),replace=T)
zctajs=fromJSON(sf_geojson(zcta))

p=plot_ly() %>%
  add_trace(type='choroplethmapbox',
            name='',
            geojson=zctajs,
            featureidkey='properties.ZCTA5CE10',
            locations=zcta$ZCTA5CE10,
            z=zcta$test,
            colorscale='YlOrRd',
            colorbar=list(lenmode='fraction',
                          len=1,
                          y=0.5,
                          yanchor='middle',
                          title=list(text='Test',
                                     font=list(size=16)),
                          ticklen=5,
                          tickfont=list(size=12),
                          outlinewidth=0),
            marker=list(line=list(color='white',
                                  width=0.1),
                        opacity=0.8),
            reversescale=T,
            hovertemplate='<b>ZCTA: </b>%{location}<br><b>Test: </b>%{z:#.2f}') %>%
  layout(mapbox=list(style='carto-positron',
                     center=list(lon=(st_bbox(zcta)['xmin']+st_bbox(zcta)['xmax'])/2,
                                 lat=(st_bbox(zcta)['ymin']+st_bbox(zcta)['ymax'])/2),
                     zoom=8),
         title=list(text='<b>Continuous Polygon</b>',
                    font=list(size=20),
                    x=0.5,
                    xanchor='center'),
         font=list(family='arial',
                   color='black'),
         margin=list(l=50,
                     r=50,
                     t=50,
                     b=50))
p
htmlwidgets::saveWidget(p,'C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/R/continuous_polygon.html')





































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
k1 = df[df['BROADBND'] == 1, c('PUMA', 'BROADBND')]
k2 = subset(df, BROADBND == 1, c('PUMA', 'BROADBND'))
k3 = df %>% select(PUMA, BROADBND) %>% filter(BROADBND == 1)

