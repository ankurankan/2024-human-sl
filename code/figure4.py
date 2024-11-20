import os
import pandas as pd
import networkx as nx
from itertools import combinations

from pgmpy.estimators import ExpertInLoop

from data import get_adult_df


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
