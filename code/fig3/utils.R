gen_mixed_data <- function(n_nodes, edge_prob){
	varnames <- sapply(1:n_nodes, function(v) {
	    paste0("x", v)
	})
	
	dag <- pcalg::randomDAG(n=n_nodes, prob=edge_prob, lB=1, uB=1, V=varnames)
	dag <- pcalg::pcalg2dagitty(as(dag, "matrix"), labels = varnames, type = "dag")
	sim_data <- mixed_data_gen_multinom(n=500, dag=dag)
	# repeat{
	# 	sim_data <- try(simulateSEM(dag, empirical=T))
	# 	if (!(inherits(sim_data, "try-error"))){
	# 		break
	# 	}
	# }
	true_adj <- dag_to_adjmatrix(dag)
	return (list(true_dag=dag, true_adj=true_adj, sim_data=sim_data$d, var_types=sim_data$var_types))
}

gen_linear_data <- function(n_nodes, edge_prob){
	varnames <- sapply(1:n_nodes, function(v) {
	    paste0("x", v)
	})
	
	dag <- pcalg::randomDAG(n=n_nodes, prob=edge_prob, lB=1, uB=1, V=varnames)
	dag <- pcalg::pcalg2dagitty(as(dag, "matrix"), labels = varnames, type = "dag")

	repeat{
		sim_data <- try(dagitty::simulateSEM(dag, N=500))
		if (!(inherits(sim_data, "try-error"))){
			break
		}
	}

	true_adj <- dag_to_adjmatrix(dag)

	var_types <- list()
	for (variable in varnames){
		var_types[[variable]] <- "cont"
	}
	return (list(true_dag=dag, true_adj=true_adj, sim_data=sim_data, var_types=var_types))
}


# Takes a dagitty DAG and returns an adjacency matrix representation.
dag_to_adjmatrix <- function(daggity_obj) {
  	edg <- dagitty:::edges(daggity_obj)
  	node_names <- dagitty:::names.dagitty(daggity_obj)
  	ans_mat <- matrix(
  	  data = 0, nrow = length(node_names),
  	  ncol = length(node_names),
  	  dimnames = list(node_names, node_names)
  	)
	if (nrow(edg) != 0){
  		ans_mat[as.matrix(edg[c("v", "w")])] <- 1
	}
  	return(ans_mat)
}

# Takes a list of edges and returns a dagitty object.
edgelist_to_dagitty <- function(edgelist, all_vars){
	edgelist <- edgelist %>% discard(is.null)
	learned_dag <- dagitty(paste0("dag{", paste0(all_vars, collapse=" "), " ", paste0(lapply(edgelist, function(x) paste0(x, collapse=" -> ")), collapse=" ") ,"}"))
	return(learned_dag)
}

build_dag <- function(varnames, edges){
	edges <- as.data.frame(edges)
	edges <- edges[, c('v', 'w')]
	if (edges %>% nrow() == 0){
		new_dag <- dagitty(paste0("dag{ ", paste0(varnames, collapse=" "), " }"))
	}
	else{
		edge_str_vec <- as.vector(apply(edges, function(x){paste0(x, collapse=' -> ')}, MARGIN=1))
		edge_str <- paste0(edge_str_vec, collapse=" ")
		new_dag <- dagitty(paste0("dag{ ", paste0(varnames, collapse=" "), " ", edge_str, " }"))
	}
	return(new_dag)
}

add_edge <- function(dag, edge){
	varnames <- names(dag)
	current_edges <- edges(dag)
	if (current_edges %>% nrow() == 0){
		edges <- matrix(c(edge[1], edge[2], '->', NA, NA), ncol=5)
		colnames(edges) <- c('v', 'w', 'e', 'x', 'y')
	}
	else{
		edges <- rbind(current_edges, c(edge[1], edge[2], '->', NA, NA))
	}
	return(build_dag(varnames, edges))
}

remove_edges <- function(dag, edges){
	varnames <- names(dag)
	current_edges <- edges(dag)
	for (i in 1:nrow(edges)){
		current_edges <- current_edges %>% filter(!((v == edges[i, 'X']) & (w == edges[i, 'Y'])))
	}
	return(build_dag(varnames, current_edges))
}
