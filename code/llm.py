import os
import pandas as pd
from itertools import combinations

import google.generativeai as genai
from pgmpy.estimators.CITests import ci_pillai
from pgmpy.base import DAG

genai.configure(api_key=os.environ['GEMINI_API_KEY'])
model = genai.GenerativeModel(model_name="gemini-1.5-flash")


def query_direction(u, v, descriptions):
    prompt = f""" You are an expert in social sciences. You are given two variables with the following descriptions:
        <A>: {descriptions[u]}
        <B>: {descriptions[v]}

        Which of the following two options is the most likely causal direction between them:
        1. <A> causes <B>
        2. <B> causes <A>

        Return a single letter answer between the choices above. I do not need the reasoning behind it. Do not add any formatting in the answer.
        """

    response = model.generate_content([prompt])
    if response.text.strip().lower() in ('a', '1'):
        print(f"Direction: {u} -> {v}")
        return (u, v)
    elif response.text.strip().lower() in ('b', '2'):
        print(f"Direction: {v} -> {u}")
        return (v, u)
    else:
        print(response.text)
        raise ValueError("LLM Failed")


def preprocess_data():
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
            "Prof-school",
            "Masters",
            "Doctorate",
        ],
        ordered=True,
    )
    df.HoursPerWeek = pd.Categorical(
        df.HoursPerWeek, categories=["<=20", "21-30", "31-40", ">40"], ordered=True
    )
    df.Workclass = pd.Categorical(df.Workclass, ordered=False)
    df.MaritalStatus = pd.Categorical(df.MaritalStatus, ordered=False)
    df.Occupation = pd.Categorical(df.Occupation, ordered=False)
    df.Relationship = pd.Categorical(df.Relationship, ordered=False)
    df.Race = pd.Categorical(df.Race, ordered=False)
    df.Sex = pd.Categorical(df.Sex, ordered=False)
    df.NativeCountry = pd.Categorical(df.NativeCountry, ordered=False)
    df.Income = pd.Categorical(df.Income, ordered=False)
    return (df)


def test_all(dag, data):
    cis = []
    for u, v in combinations(list(dag.nodes()), 2):
        u_parents = set(dag.get_parents(u))
        v_parents = set(dag.get_parents(v))

        if (v in u_parents):
            u_parents -= set([v])
            edge_present = True
        elif (u in v_parents):
            v_parents -= set([u])
            edge_present = True
        else:
            edge_present = False

        cond_set = list(set(u_parents).union(v_parents))
        effect, p_value = ci_pillai(X=u, Y=v, Z=cond_set, data=data, boolean=False)
        cis.append([u, v, cond_set, edge_present, effect, p_value])

    return pd.DataFrame(cis, columns=['u', 'v', 'z', 'edge_present', 'effect', 'p_val'])

def simulate_human(data, descriptions, pval_thres=0.1, effect_thres=0.1):
    nodes = list(data.columns)
    dag = DAG()
    dag.add_nodes_from(nodes)

    while(True):
        all_effects = test_all(dag, data)

        edge_effects = all_effects[all_effects.edge_present == True]
        edge_effects = edge_effects[(edge_effects.effect < effect_thres) & (edge_effects.p_val > pval_thres)]
        remove_edges = list(edge_effects.loc[:, ('u', 'v')].to_records(index=False))
        print(f"Removing edges: {remove_edges}")
        for edge in remove_edges:
            dag.remove_edge(edge[0], edge[1])

        nonedge_effects = all_effects[all_effects.edge_present == False]
        nonedge_effects = nonedge_effects[(nonedge_effects.effect >= effect_thres) & (nonedge_effects.p_val <= pval_thres)]
        add_edges = list(nonedge_effects.loc[:, ('u', 'v')].to_records(index=False))
        add_edges_directed = []
        print(f"Adding: {add_edges}")

        for edge in add_edges:
            add_edges_directed.append(query_direction(edge[0], edge[1], descriptions))
        dag.add_edges_from(add_edges_directed)

        if (edge_effects.shape[0] == 0) and (nonedge_effects.shape[0] == 0):
            break
    return(dag)

descriptions = {'Age': 'The age of a person',
                'Workclass': 'The workplace where the person is employed such as Private industry, or self employed',
                'Education': "The highest level of education the person has finished.",
                'MaritalStatus': "The marital status of a person",
                "Occupation": "The kind of job the person does. For example, sales, craft repair, clerkial",
                "Relationship": "The relationship status of the person",
                "Race": "The ethnicity of the person",
                "Sex": "The sex or gender of the person",
                "HoursPerWeek": "The number of hours per week the person works",
                "NativeCountry": "The native country of the person",
                "Income": "The income i.e. amount of money the person makes"
                }

dag = simulate_human(preprocess_data(), descriptions)
