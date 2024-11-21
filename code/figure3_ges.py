import pandas as pd

from pgmpy.estimators import GES

df = pd.read_csv('temp_df.csv')
for col in df.columns:
    if df.loc[:, col].dtype == 'int64':
        df.loc[:, col] = df.loc[:, col].astype('str')

est = GES(df)
dag = est.estimate(scoring_method='cond-gauss')
