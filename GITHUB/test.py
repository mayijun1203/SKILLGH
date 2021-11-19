import os
import git
import datetime
import pytz
import pandas as pd



# user=os.environ['user']
# key=os.environ['key']




# path='C:/Users/mayij/Desktop/DOC/GITHUB/SKILLGH/'
path='./GITHUB/'
timestamp=datetime.datetime.now(pytz.timezone('US/Eastern')).strftime('%m%d%Y')
df=pd.DataFrame()
df.to_csv(path+'/timestamp.csv')



repo=git.Repo(path)
repo.git.add('.')
repo.index.commit('autoupdate')
origin=repo.remote(name='origin')
origin.push()


