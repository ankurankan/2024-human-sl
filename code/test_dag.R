source('data.R')
source('ci_test.R')


# Test whether u -> v or u <- v is present in present_edges.
is_edge_present <- function(u, v, present_edges){
	if (any((u == present_edges['v']) & (v == present_edges['w'])) | any((u == present_edges['w']) & (v == present_edges['v']))){
		return(TRUE)
	}
	else{ return(FALSE)
	}
}

# Iterates over each possible combination of variables in dag. 
# If edge present between X and Y, computes the effect of X _|_ Y | pa(X, Y)
# If edge not present between X and Y, computes the effect between R_{X | pa(X)} and R_{Y | pa(Y)}
compute_effects <- function(dag, data){
	all_possible_edges <- t(combn(names(dag), 2))

	present_edges <- edges(dag)[, c('v', 'w')]

	effects <- c()
	for (edge_index in 1:nrow(all_possible_edges)){
		u <- all_possible_edges[edge_index, ][1]
		v <- all_possible_edges[edge_index, ][2]

		u_parents <- parents(dag, u)
		v_parents <- parents(dag, v)
		if (is_edge_present(u, v, present_edges)){
			common_parents <- intersect(u_parents, v_parents)

			if (length(common_parents) == 0){
				effects <- round(c(effects, cancor(data[, u], data[, v])$cor), digits=4)
			}
			else{
				effects <- c(effects, cond_effects(u, v, common_parents, common_parents, data))
			}
		}
		else{
			effects <- c(effects, cond_effects(u, v, u_parents, v_parents, data))
		}
	}
	all_possible_edges <- cbind(all_possible_edges, effects)
	return(all_possible_edges)
}


dag <- dagitty("dag{ X -> Z <- Y }")
cont_data <- mixed_data_gen_multinom(dag=dag, var_types=list(X='cont', Y='cont', Z='cont'))$d

nodes <- names(dag)
edges <- edges(dag)[, c('v', 'w')]

# No edge
# no_edge_effects <- compute_effects(dag=dagitty('dag{ X Y Z}'), data=cont_data)
# print(no_edge_effects)

# X -> Z edge
edge_effects <- compute_effects(dag=dagitty('dag{ X Y -> Z}'), data=cont_data)
print(edge_effects)


# X -> Z <- Y edge
edge_effects <- compute_effects(dag=dagitty('dag{ X -> Z <- Y}'), data=cont_data)
print(edge_effects)


# X -> Y edge
edge_effects <- compute_effects(dag=dagitty('dag{ X -> Y Z}'), data=cont_data)
print(edge_effects)
