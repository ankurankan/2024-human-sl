import os
from ast import literal_eval
from itertools import combinations

import pandas as pd
import networkx as nx
from pgmpy.estimators import ExpertInLoop
from pgmpy.models import BayesianNetwork

from data import get_adult_df


def build_network():
    descriptions = {'Age': 'The age of a person',
                    'Workclass': 'The workplace where the person is employed such as Private industry, or self employed',
                    'Education': "The highest level of education the person has finished",
                    'MaritalStatus': "The marital status of the person",
                    "Occupation": "The kind of job the person does. For example, sales, craft repair, clerical",
                    "Relationship": "The relationship status of the person",
                    "Race": "The ethnicity of the person",
                    "Sex": "The sex or gender of the person",
                    "HoursPerWeek": "The number of hours per week the person works",
                    "NativeCountry": "The native country of the person",
                    "Income": "The income i.e. amount of money the person makes"
                    }

    est = ExpertInLoop(get_adult_df())
    dag = est.estimate(
            pval_threshold=0.05,
            effect_size_threshold=0.05,
            use_llm=True,
            llm_model="gemini/gemini-1.5-pro",
            variable_descriptions=descriptions,
            show_progress=True,
    )
    return(dag)

def make_plot():
    with open('plot_data/llm_edge_list.txt', 'r') as f:
        edge_list = literal_eval(f.readline())
    bn = BayesianNetwork(edge_list)
    bn.to_graphviz().draw('plots/llm_adult.pdf', prog='dot')


if __name__ == "__main__":
    dag = build_network()

    with open('plot_data/llm_edge_list.txt', 'w') as f:
        f.write(str(list(dag.edges())))

    make_plot()

