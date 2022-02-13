import os
import git
import datetime
import pytz
import pandas as pd


timestamp=datetime.datetime.now(pytz.timezone('US/Eastern')).strftime('%m%d%H%M%S')
df=pd.DataFrame([os.environ['t'])
df.to_csv('./GITHUB/'+timestamp+'.csv')



repo=git.Repo('./')
repo.git.add('.')
repo.index.commit('autoupdate')
origin=repo.remote(name='origin')
origin.push()


