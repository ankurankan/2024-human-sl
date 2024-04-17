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

# Iterates over each possible combination of variables in dag. 
# If edge present between X and Y, computes the effect of X _|_ Y | pa(X, Y)
# If edge not present between X and Y, computes the effect between R_{X | pa(X)} and R_{Y | pa(Y)}
compute_effects_cond <- function(dag, data){
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

		u_parents <- parents(dag, u)
		v_parents <- parents(dag, v)
		# if (is_edge_present(u, v, present_edges)){
		# 	common_parents <- intersect(u_parents, v_parents)

		# 	if (length(common_parents) == 0){
		# 		effects <- c(effects, round(cancor(data[, u], data[, v])$cor, digits=4))
		# 	}
		# 	else{
		# 		effects <- c(effects, round(cond_effects(u, v, common_parents, common_parents, data), digits=4))
		# 	}
		# }
		effects <- c(effects, round(cond_effects(u, v, u_parents, v_parents, data), digits=4))
		edge_present <- c(edge_present, is_edge_present(u, v, present_edges))
	}
	all_possible_edges <- cbind(all_possible_edges, effects, edge_present)
	colnames(all_possible_edges) <- c('u', 'v', 'Effect', 'Edge Present?')
	return(all_possible_edges)
}

# Iterates over each possible combination of variables in dag and computes their marginal canonical correlation.
compute_effects_marg <- function(dag, data){
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

test_dag <- function(true_dag, dag, effect_type){
	sim_data <- simulateSEM(true_dag, b.default=0.3, empirical=T)
	if (effect_type == 'marg'){
		return(compute_effects_marg(dag=dag, data=sim_data))
	}
	else if (effect_type == 'cond'){
		return(compute_effects_cond(dag=dag, data=sim_data))
	}
}

# DAG 1: Collider  Structure
dag1 <- dagitty("dag{ X -> Z <- Y }")

# DAG 2: Mediator
dag2 <- dagitty("dag{X -> M -> Y}")

# DAG 3: Mediator with direct effect
dag3 <- dagitty("dag{X -> M -> Y X -> Y}")

# DAG 4: Additional path between X and Y
dag4 <- dagitty("dag{ A -> X -> Z <- Y A -> Y }")

# DAG 5:
dag5 <- dagitty("dag{ {x1 x2} -> a -> b -> {x4 x5} x1 -> x4}")


# print(test_dag(true_dag=dag3, dag=dagitty('dag{X M -> Y}'), effect_type='marg'))
# print(test_dag(true_dag=dag3, dag=dagitty('dag{X M -> Y}'), effect_type='cond'))
print(test_dag(true_dag=dag5, dag=dagitty('dag{{x1 x2} -> a b -> {x4 x5} x1 -> x4}'), effect_type='marg'))
print(test_dag(true_dag=dag5, dag=dagitty('dag{{x1 x2} -> a b -> {x4 x5} x1 -> x4}'), effect_type='cond'))
