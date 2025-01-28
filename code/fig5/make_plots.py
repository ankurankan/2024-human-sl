from pgmpy.models import BayesianNetwork

with open('llm_adult.txt', 'r') as f:
    edge_list = eval(f.readline())

dag = BayesianNetwork(edge_list)
dag.to_graphviz().draw('llm_adult.png', prog='dot')
