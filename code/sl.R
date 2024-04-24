source('ci_test.R')
source('test_dag.R')


library(future.apply)
library(tidyverse)
library(progress)
plan(multisession, workers=32)

THRESHOLD <- 0.05

# Oracle function. Returns either true or (random or don't know) direction of the edge.
oracle <- function(u, v, true_dag, accuracy=1){
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
		return(list(c(u, v), c(v, u), NULL)[[sample(1:3, 1)]]) } }

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

# Simulate a human structure learning based on CI tests.
simulate_human_sl <- function(sim_data, true_dag, oracle_acc, max_iter=1e4){
	varnames <- colnames(sim_data)
	new_dag <- dagitty(paste0("dag{ ", paste(varnames, collapse=" "), " }"))
	counter <- 0
	blacklist_edges <- matrix(nrow=0, ncol=2)

	while (counter < max_iter){
		counter <- counter + 1

		effects <- compute_effects_v2(new_dag, sim_data)[, c('X', 'A', 'Y', 'cor')]

		# If edges are present do pruning.
		edges_to_prune <- effects %>% filter(A == '->') %>% filter(abs(cor) < THRESHOLD)
		if (edges_to_prune %>% nrow() > 0){
			new_dag <- remove_edges(new_dag, edges_to_prune)
		}
		
		# Recompute effects after pruning: TODO: Do we need to do this?
		effects <- compute_effects_v2(new_dag, sim_data)[, c('X', 'A', 'Y', 'cor')]
		unexplain_cor_sorted <- effects %>% filter(abs(as.double(effects$cor)) > THRESHOLD) %>% filter(A == '--') %>% arrange(desc(abs(cor)))

		# Remove blaclisted edges
		if (blacklist_edges %>% nrow() > 0){
			unexplain_cor_sorted <- unexplain_cor_sorted %>%
				filter(!((X %in% blacklist_edges[, 1]) &
					 (Y %in% blacklist_edges[, 2])))
		}

		# If no unexplained correlation is remaining, exit loop.
		if(unexplain_cor_sorted %>% nrow() == 0){
			break
		}
		# Else add the highest correlated edge.
		else{
			oracle_edge <- oracle(
				u=unexplain_cor_sorted[1, 'X'],
				v=unexplain_cor_sorted[1, 'Y'],
				true_dag=true_dag,
				accuracy=oracle_acc
			)
			# If oracle doesn't know the direction of this edge, add it to
			# blacklist to not ask oracle again about it.
			if (is.null(oracle_edge)){
				blacklist_edges <- rbind(blacklist_edges,
							 c(unexplain_cor_sorted[1, 'X'],
							   unexplain_cor_sorted[1, 'Y']))
			}
			else{
				new_dag <- add_edge(new_dag, oracle_edge)
			}
		}
	}
	return(dag_to_adjmatrix(new_dag))
}


# Generates a random dag with n_nodes number of nodes with edge probability edge_prob.
# Returns the SHD between the learned (using a greedy approach) and the true graph.
run_single_exp <- function(n_nodes, edge_prob, oracle_acc){
	# Step 1: Create a random DAG and simulate data from it.
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
	true_adj <- dag_to_adjmatrix(dag)

	# Step 2: Learn the model using Hill-Climb Search
	hc_dag <- bnlearn::hc(sim_data)
	hc_adj <- bnlearn::amat(hc_dag)

	# Step 3: Learn the model using PC algorithm
    	# suffStat <- list(dm = sim_data, adaptDF = FALSE, test = 'mxm')
    	# alpha <- 0.05

	# pc.skel <- pcalg::skeleton(
	# 	suffStat,
	# 	indepTest = sl.test,
	# 	alpha = alpha,
	# 	labels = colnames(sim_data),
	#     	method = "stable",
	#     	verbose = FALSE
	# )
	pc_dag <- bnlearn::pc.stable(sim_data, test='mc-cor', undirected=F)
	pc_adj <- bnlearn::amat(pc_dag)

	# Step 4: Simulate human SL based on independence tests
	human_adj <- simulate_human_sl(sim_data=sim_data, true_dag=dag, oracle_acc=oracle_acc)

	# Step 4: Compare the learned graph with the true graph.
	human_dist <- sum(abs(true_adj - human_adj))
	hc_dist <- sum(abs(true_adj - hc_adj))
	return (c(human_dist, hc_dist))
}


run_sim <- function(R, n_nodes, edge_probs, oracle_accs){
	results <- data.frame()
	results_pruned <- data.frame()
	
	pb <- progress_bar$new(total=length(oracle_accs) * length(edge_probs))
	for (oracle_acc in oracle_accs){
		for (edge_prob in edge_probs){
			shd <- t(future_replicate(R, run.sim(n_nodes=n_nodes, edge_prob=edge_prob, oracle_acc=oracle_acc), future.chunk.size=3))
			# shd <- t(replicate(R, run.sim(n_nodes=n_nodes, edge_prob=edge_prob, oracle_acc=oracle_acc, prune_dags=F)))
			mean_shd <- apply(shd, mean, MARGIN=2)
			sd_shd <- apply(shd, sd, MARGIN=2)/sqrt(R)
			results <- rbind(results, c(oracle_acc, edge_prob, mean_shd, sd_shd))
	
			shd_pruned <- t(future_replicate(R, run.sim(n_nodes=n_nodes, edge_prob=edge_prob, oracle_acc=oracle_acc), future.chunk.size=3))
			mean_shd <- apply(shd_pruned, mean, MARGIN=2)
			sd_shd <- apply(shd_pruned, sd, MARGIN=2)/sqrt(R)
			results_pruned <- rbind(results_pruned, c(oracle_acc, edge_prob, mean_shd, sd_shd))
			pb$tick()
		}
	}
	colnames(results) <- c('oracle_acc', 'edge_prob', 'mean_shd_human', 'mean_shd_hc', 'std_error_human', 'std_error_hc')
	colnames(results_pruned) <- c('oracle_acc', 'edge_prob', 'mean_shd_human', 'mean_shd_hc', 'std_error_human', 'std_error_hc')
	write.csv(results, 'results/sl_results.csv')
	write.csv(results_pruned, 'results/sl_results_pruned.csv')
}


edge_probs <- seq(0.1, 0.9, 0.2)
oracle_accs <- seq(0.1, 0.9, 0.1)
n_nodes <- 10
R <- 3

run_sim(R, n_nodes, edge_probs, oracle_accs)
