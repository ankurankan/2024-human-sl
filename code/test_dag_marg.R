source('data.R')
source('ci_test.R')


# Test whether u -> v or u <- v is present in present_edges.
is_edge_present <- function(u, v, present_edges){
	if (nrow(present_edges) == 0){
		return(FALSE)
	}
	if (any((u == present_edges['v']) & (v == present_edges['w'])) | any((u == present_edges['w']) & (v == present_edges['v']))){
		return(TRUE)
	}
	else{ return(FALSE)
	}
}

# Iterates over each possible combination of variables in dag and computes their marginal canonical correlation.
compute_effects <- function(dag, data){
	all_possible_edges <- t(combn(names(dag), 2))

	present_edges <- edges(dag)
	if (nrow(present_edges) > 0){
		present_edges <- present_edges[, c('v', 'w')]
	}

	effects <- c()
	edge_present <- c()
	for (edge_index in 1:nrow(all_possible_edges)){
		u <- all_possible_edges[edge_index, ][1]
		v <- all_possible_edges[edge_index, ][2]

		effects <- c(effects, cancor(data[, u], data[, v])$cor)
		edge_present <- c(edge_present, is_edge_present(u, v, present_edges))
	}
	all_possible_edges <- cbind(all_possible_edges, effects, edge_present)
	colnames(all_possible_edges) <- c('u', 'v', 'Effect', 'Edge Present?')
	return(all_possible_edges)	
}

# DAG 1
dag <- dagitty("dag{ X -> Z <- Y }")
cont_data <- mixed_data_gen_multinom(dag=dag, var_types=list(X='cont', Y='cont', Z='cont'))$d

# No edges
edge_effects <- compute_effects(dag=dagitty('dag{ X Y Z}'), data=cont_data)
print(edge_effects)

# X -> Z edge
edge_effects <- compute_effects(dag=dagitty('dag{ X Y -> Z}'), data=cont_data)
print(edge_effects)

# X -> Z <- Y edge
edge_effects <- compute_effects(dag=dagitty('dag{ X -> Z <- Y}'), data=cont_data)
print(edge_effects)

# X -> Z <- Y  X-> Y
edge_effects <- compute_effects(dag=dagitty('dag{ X -> Z <- Y  X -> Y}'), data=cont_data)
print(edge_effects)

# X -> Y edge
edge_effects <- compute_effects(dag=dagitty('dag{ X -> Y Z}'), data=cont_data)
print(edge_effects)

# DAG 2
dag <- dagitty("dag{ A -> X -> Z <- Y A -> Y }")
cont_data <- mixed_data_gen_multinom(dag=dag, var_types=list(X='cont', Y='cont', Z='cont', A='cont'))$d

edge_effects <- compute_effects(dag=dagitty('dag{ A X Y -> Z}'), data=cont_data)
print(edge_effects)

edge_effects <- compute_effects(dag=dagitty('dag{ X Y -> Z <- A}'), data=cont_data)
print(edge_effects)

edge_effects <- compute_effects(dag=dagitty('dag{ X Y -> Z <- X A}'), data=cont_data)
print(edge_effects)

edge_effects <- compute_effects(dag=dagitty('dag{ Y -> Z <- X <- A}'), data=cont_data)
print(edge_effects)

# DAG 3 
dag <- dagitty("dag{ {x1 x2} -> a -> b -> {x4 x5} x1 -> x4}")
cont_data <- simulateSEM(dag, empirical=T)

edge_effects1 <- compute_effects(dag=dagitty('dag{ {x1 x2} -> a  b -> {x4 x5} }'), data=cont_data)
edge_effects2 <- compute_effects(dag=dagitty('dag{ {x1 x2} -> a -> b -> {x4 x5} }'), data=cont_data)


# Mediation Model
dag <- dagitty("dag{ X -> M -> Y }")
cont_data <- simulateSEM(dag, empirical=T)

edge_effects <- compute_effects(dag=dagitty('dag{ X M -> Y}'), data=cont_data)
print(edge_effects)
