import os
import git
import datetime
import pytz
import pandas as pd
import numpy as np
import plotly.express as px
import plotly.graph_objects as go

timestamp=datetime.datetime.now(pytz.timezone('US/Eastern')).strftime('%m/%d/%Y')

try:
    # Subway and Bus Daily Ridership
    url='https://new.mta.info/document/20441'
    df=pd.read_csv(url,dtype=str)
    df['Date']=[datetime.datetime.strptime(x,'%m/%d/%Y') for x in df['Date']]
    df['Subway']=[int(x) for x in df['Subways: Total Estimated Ridership']]
    df['Bus']=[int(x) for x in df['Buses: Total Estimated Ridership']]
    df=df[['Date','Subway','Bus']].sort_values('Date').reset_index(drop=True)
    fig=px.line(data_frame=df,
                x='Date',
                y=['Subway','Bus'],
                color_discrete_sequence=['tomato','steelblue'],
                title='<b>Subway and Bus Estimated Ridership '+df.iloc[0,0].strftime('%m/%d/%Y')+' - '+df.iloc[-1,0].strftime('%m/%d/%Y')+' (Source: '+"</b><a href='https://new.mta.info/coronavirus/ridership'>MTA</a>"+'<b>)</b>',
                template='plotly_white')
    fig.update_layout(
        title={'font_size':20,
               'x':0.5,
               'xanchor':'center'},
        legend={'orientation':'h',
                'title_text':'',
                'font_size':16,
                'x':0.5,
                'xanchor':'center',
                'y':1,
                'yanchor':'bottom'},
        xaxis={'title':{'text':'<b>Date</b>',
                        'font_size':14},
               'tickfont_size':12,
               'fixedrange':True,
               'showgrid':True},
        yaxis={'title':{'text':'<b>Ridership</b>',
                        'font_size':14},
               'tickfont_size':12,
               'rangemode':'nonnegative',
               'fixedrange':True,
               'showgrid':True},
        font={'family':'Arial',
              'color':'black'},
        dragmode=False,
        hovermode='x unified'
    )
    fig.update_traces(
        line={'width':2},
        hovertemplate='%{y:#.3s}'
        )
    fig.write_html('./GITHUB/index.html',include_plotlyjs='cdn')
    
    print('done')

    repo=git.Repo('./')
    repo.git.add('.')
    repo.index.commit('autoupdate')
    origin=repo.remote(name='origin')
    origin.push()

except:
    print(timestamp+' ERROR!')    










