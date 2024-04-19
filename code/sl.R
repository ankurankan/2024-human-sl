source('test_dag.R')


library(future.apply)
library(tidyverse)
library(progress)
plan(multisession)

oracle <- function(u, v, true_dag, accuracy=0.9){
	if (runif(1) <= accuracy){
		if (v %in% dagitty::children(true_dag, u)){
			return(c(u, v))
		}
		else if (u %in% dagitty::children(true_dag, v)){
			return(c(v, u))
		}
		else{
			return(NULL)
		}
	}
	else{
		return(list(c(u, v), c(v, u), NULL)[[sample(1:3, 1)]])
	}
}

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

# Generates a random dag with n_nodes number of nodes with edge probability edge_prob.
# Returns the SHD between the learned (using a greedy approach) and the true graph.
run.sim <- function(n_nodes, edge_prob, oracle_acc){
	varnames <- sapply(1:n_nodes, function(v) {
	    paste0("x", v)
	})
	
	dag <- pcalg::randomDAG(n=n_nodes, prob=edge_prob, lB=1, uB=1, V=varnames)
	dag <- pcalg::pcalg2dagitty(as(dag, "matrix"), labels = varnames, type = "dag")
	repeat{
		sim_data <- try(simulateSEM(dag, empirical=T))
		if (!(inherits(sim_data, "try-error"))){
			break
		}
	}

	marg_cor <- compute_effects_marg(dag, sim_data)[, c('u', 'v', 'cor')]
	marg_cor_thresh <- marg_cor[as.double(marg_cor$cor) > 0.05, ]
	marg_cor_thresh_sorted <- marg_cor_thresh %>% arrange(desc(marg_cor_thresh$cor))

	# Handle the case of no significant correlation i.e. no learned edge.
	if (nrow(marg_cor_thresh_sorted) == 0){
		true_adj <- dag_to_adjmatrix(dag)
		return (sum(true_adj))
	}
	else{	
		learned_edges <- list()
		for (i_row in 1:nrow(marg_cor_thresh_sorted)){
			oracle_edge <- oracle(marg_cor_thresh_sorted[i_row, 'u'], marg_cor_thresh_sorted[i_row, 'v'], true_dag=dag, accuracy=oracle_acc)	
			learned_edges[[i_row]] <- oracle_edge
		}
		learned_edges <- learned_edges %>% discard(is.null)
		learned_dag <- dagitty(paste0("dag{", paste0(names(dag), collapse=" "), " ", paste0(lapply(learned_edges, function(x) paste0(x, collapse=" -> ")), collapse=" ") ,"}"))
		
		true_adj <- dag_to_adjmatrix(dag)
		learned_adj <- dag_to_adjmatrix(learned_dag)
		dist <- sum(abs(true_adj - learned_adj))
		return(dist)
	}
}

edge_probs <- seq(0.1, 0.9, 0.2)
oracle_accs <- seq(0.1, 0.9, 0.2)
n_nodes <- 10

results <- data.frame()

pb <- progress_bar$new(total=length(oracle_accs) * length(edge_probs))
for (oracle_acc in oracle_accs){
	for (edge_prob in edge_probs){
		shd <- future_replicate(1000, run.sim(n_nodes=n_nodes, edge_prob=edge_prob, oracle_acc=oracle_acc), future.chunk.size=10)
		mean_shd <- mean(shd)
		results <- rbind(results, c(oracle_acc, edge_prob, mean(shd)))
		pb$tick()
	}
}
colnames(results) <- c('oracle_acc', 'edge_prob', 'mean_shd')
write.csv(results, 'sl_results.csv')
