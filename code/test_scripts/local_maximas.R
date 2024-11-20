library(dagitty)
library(bnlearn)

set.seed(42)

# Example 1: Hill-Climb cannot improve the score of the given initial graph.
#           As reversing any edge creates an incorrect V-structure.
d1 <- dagitty("dag{X -> Y <- Z <- A X -> Z}")
df1 <- simulateSEM(d1, N=1e3)

d1_score <- score(model2network(convert(d1, 'bnlearn')), df1, 'bic-g')
d1_est <- hc(df1, start=model2network("[X][Y|X][Z|X:Y][A|Z]"), score='bic-g')
d1_est_score <- score(d1_est, df1, 'bic-g')

# Example 2: 
d2 <- dagitty("dag{X -> Y <- Z}")
df2 <- simulateSEM(d2, N=1e3)

d2_score <- score(model2network(convert(d2, 'bnlearn')), df2, 'bic-g')
d2_est = hc(df2, start=model2network("[Y][X|Y][Z|Y]"), score='bic-g')
d2_est_score <- score(d2_est, df2, 'bic-g')

# Example 3:
d3 <- dagitty("dag{X -> E -> Z X -> Y -> Z}")
df3 <- simulateSEM(d3, N=1e3)

d3_score <- score(model2network(convert(d3, "bnlearn")), df3, "bic-g")
d3_est <- hc(df3, start=model2network("[E][Y|X:E][Z|E][X|E]"), score='bic-g')
# d3_est <- hc(df3, score='bic-g')
d3_est_score <- score(d3_est, df3, 'bic-g')

# Example 4:
d4 <- dagitty("dag{X -> Y -> Z -> A}")
df4 <- simulateSEM(d4, N=1e3)

d4_score <- score(model2network(convert(d4, "bnlearn")), df4, "bic-g")
d4_est <- hc(df4, start=model2network("[Y|X][Z|A][A][X]"), score="bic-g")
d4_est_score <- score(d4_est, df4, 'bic-g')
