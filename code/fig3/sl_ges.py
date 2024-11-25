import sys

import pandas as pd
import networkx as nx

from pgmpy.estimators import GES

filename = sys.argv[1]
# filename = "temp/8gu4Q.csv"
df = pd.read_csv(filename)
for col in df.columns:
    if df[col].dtype == 'int64':
        df[col] = df[col].astype('category')

est = GES(df)
dag = est.estimate(scoring_method='cond-gauss')
dag_adj = nx.to_numpy_array(dag, nodelist=sorted(dag.nodes()), weight=1, dtype=int)
dag_adj = pd.DataFrame(dag_adj, columns=sorted(dag.nodes()), index=sorted(dag.nodes()))
dag_adj.to_csv('temp/adj_' + filename.split('/')[1])
