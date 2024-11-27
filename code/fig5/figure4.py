import sys
sys.path.append("../utils/")

import os
from ast import literal_eval
from itertools import combinations

import pandas as pd
import networkx as nx
import numpy as np
from pgmpy.models import BayesianNetwork

from data import get_adult_df

from expert import ExpertInLoop
from GES import GES


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
    dag, total_unexplained_effect, total_ll = est.estimate(
            pval_threshold=0.05,
            effect_size_threshold=0.05,
            use_llm=True,
            llm_model="gemini/gemini-1.5-pro",
            variable_descriptions=descriptions,
            show_progress=True,
    )
    return(dag, total_unexplained_effect, total_ll)

def build_ges():
    est = GES(get_adult_df())
    dag, total_unexplained_effect, total_ll = est.estimate(scoring_method='bic-cg')
    return(dag, total_unexplained_effect, total_ll)

def make_plot():
    with open('results/ges_edge_list.txt', 'r') as f:
        edge_list = literal_eval(f.readline())
    bn = BayesianNetwork(edge_list)
    bn.to_graphviz().draw('results/ges_adult.pdf', prog='dot')

    with open('results/llm_edge_list.txt', 'r') as f:
        edge_list = literal_eval(f.readline())
    bn = BayesianNetwork(edge_list)
    bn.to_graphviz().draw('results/expert_llm_adult.pdf', prog='dot')

if __name__ == "__main__":
    dag, total_unexplained_effect, total_ll = build_ges()
    with open('results/ges_edge_list.txt', 'w') as f:
        f.write(str(list(dag.edges())))
    with open('results/ges_unexplained_effect.txt', 'w') as f:
        f.write(str(np.array(total_unexplained_effect).tolist())[1:-1])
        f.write('\n')
        f.write(str(np.array(total_ll).tolist())[1:-1])

    dag, total_unexplained_effect, total_ll = build_network()
    with open('results/llm_edge_list.txt', 'w') as f:
        f.write(str(list(dag.edges())))
    with open('results/expert_unexplained_effect.txt', 'w') as f:
        f.write(str(np.array(total_unexplained_effect).tolist())[1:-1])
        f.write('\n')
        f.write(str(np.array(total_ll).tolist())[1:-1])

    make_plot()

