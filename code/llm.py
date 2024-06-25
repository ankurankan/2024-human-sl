import pandas as pd
from itertools import combinations

from pgmpy.estimators.CITests import ci_pillai
from pgmpy.base import DAG

def test_all(dag, data):
    cis = []
    for u, v in combinations(list(dag.nodes()), 2):
        u_parents = dag.get_parents(u)
        v_parents = dag.get_parents(v)
        cond_set = list(set(u_parents).union(v_parents))
        import pdb; pdb.set_trace()
        effect, p_value = ci_pillai(X=u, Y=v, Z=cond_set, data=data, boolean=False)
        cis.append(u, v, cond_set, effect, p_value)

    return pd.DataFrame(cis, columns=['u', 'v', 'z', 'effect', 'p_val'])

df = pd.read_csv("adult_proc.csv", index_col=0)
df.Age = pd.Categorical(
    df.Age,
    categories=["<21", "21-30", "31-40", "41-50", "51-60", "61-70", ">70"],
    ordered=True,
)
df.Education = pd.Categorical(
    df.Education,
    categories=[
        "Preschool",
        "1st-4th",
        "5th-6th",
        "7th-8th",
        "9th",
        "10th",
        "11th",
        "12th",
        "HS-grad",
        "Some-college",
        "Assoc-voc",
        "Assoc-acdm",
        "Bachelors",
        "Masters",
        "Doctorate",
    ],
    ordered=True,
)
df.HoursPerWeek = pd.Categorical(
    df.HoursPerWeek, categories=["<=20", "21-30", "31-40", ">40"], ordered=True
)

nodes = list(df.columns)
dag = DAG()
dag.add_nodes_from(nodes)


