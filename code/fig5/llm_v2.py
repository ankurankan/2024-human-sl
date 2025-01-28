import os
import pandas as pd
import networkx as nx
from itertools import combinations

import google.generativeai as genai
from pgmpy.estimators import ExpertInLoop
from pgmpy.base import DAG


def preprocess_data():
    df = pd.read_csv("../utils/adult_proc.csv", index_col=0)
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
    df.Workclass = pd.Categorical(df.Workclass, ordered=False)
    df.MaritalStatus = pd.Categorical(df.MaritalStatus, ordered=False)
    df.Occupation = pd.Categorical(df.Occupation, ordered=False)
    df.Relationship = pd.Categorical(df.Relationship, ordered=False)
    df.Race = pd.Categorical(df.Race, ordered=False)
    df.Sex = pd.Categorical(df.Sex, ordered=False)
    df.NativeCountry = pd.Categorical(df.NativeCountry, ordered=False)
    df.Income = pd.Categorical(df.Income, ordered=False)
    return (df)

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

dag = ExpertInLoop(preprocess_data()).estimate(variable_descriptions=descriptions, effect_size_threshold=0.1)
with open('llm_adult.txt', 'w') as f:
    f.write(str(list(dag.edges())))
