source('ci_test.R')
source('test_dag.R')


library(future.apply)
library(tidyverse)
library(progress)
plan(multisession, workers=32)

# Oracle function. Returns either true or (random or don't know) direction of the edge.
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

# Simulate a human structure learning based on CI tests.
# Use a greedy approach where the highest correlation is added first.
simulate.human.sl <- function(sim_data, max_iter=1e4){
	new_dag <- dagitty(paste0("dag{ ", paste(varnames, collapse=" "), " }"))
	counter <- 1
	while (counter < max_iter){
		effects <- compute_effects_v2(new_dag, sim_data)[, c('X', 'A', 'Y', 'cor')]
		effects_thres <- effects[abs(as.double(effects$cor)) > 0.05, ]
		effects_thres_sorted <- effects_thres %>% arrange(desc(abs(effects_thres$cor)))
		browser()
		if (nrow(marg_cor_thresh_sorted) == 0){
			return (new_dag)
		}
		else{
			learned_edges <- list()
			for (i_row in 1:nrow(effects_thres_sorted)){
				oracle_edge <- oracle(
					u=marg_cor_thresh_sorted[i_row, 'u'],
					v=marg_cor_thresh_sorted[i_row, 'v'],
					true_dag=dag,
					accuracy=oracle_acc
				)
				learned_edges[[i_row]] <- oracle_edge
				
				# Do the pruning using both the p-value and effect size.
				if(prune_dags){
					# Prune edges if coefficient goes to 0.
					inter_dag <- edgelist_to_dagitty(learned_edges, names(dag))
					edge_coefs <- compute_effects_marg(inter_dag, sim_data) %>% 
								filter(edge==T) %>%
								filter(abs(as.double(cor)) <= 0.05) %>%
								select(c('u', 'v'))
					if (nrow(edge_coefs) > 0){
						remove_indexes <- c()
						for (coefs_row in 1:nrow(edge_coefs)){
							for (i_edge in 1:length(learned_edges)){
								x1 <- edge_coefs[coefs_row, 'u']
								x2 <- edge_coefs[coefs_row, 'v']
								if (all(c(x1, x2) == learned_edges[[i_edge]]) | all(c(x2, x1)==learned_edges[[i_edge]])){
									# print(paste0("Removing edge: ", paste0(learned_edges[[i_edge]], collapse=" ")))
									remove_indexes <- c(remove_indexes, i_edge)
								}
							}
						}
						learned_edges[remove_indexes] <- NULL
					}
				}
			}
		}
		learned_dag <- edgelist_to_dagitty(edgelist=learned_edges, all_vars=names(dag))
		learned_adj <- dag_to_adjmatrix(learned_dag)
	}


}


# Generates a random dag with n_nodes number of nodes with edge probability edge_prob.
# Returns the SHD between the learned (using a greedy approach) and the true graph.
run.sim <- function(n_nodes, edge_prob, oracle_acc, prune_dags=T){
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
	simulate.human.sl(sim_data)

	# Step 4: Compare the learned graph with the true graph.
	human_dist <- sum(abs(true_adj - learned_adj))
	hc_dist <- sum(abs(true_adj - hc_adj))
	return (c(human_dist, hc_dist))
}

edge_probs <- seq(0.1, 0.9, 0.2)
oracle_accs <- seq(0.1, 0.9, 0.1)
n_nodes <- 10
R <- 100

results <- data.frame()
results_pruned <- data.frame()

pb <- progress_bar$new(total=length(oracle_accs) * length(edge_probs))
for (oracle_acc in oracle_accs){
	for (edge_prob in edge_probs){
		# shd <- t(future_replicate(R, run.sim(n_nodes=n_nodes, edge_prob=edge_prob, oracle_acc=oracle_acc, prune_dags=F), future.chunk.size=3))
		shd <- t(replicate(R, run.sim(n_nodes=n_nodes, edge_prob=edge_prob, oracle_acc=oracle_acc, prune_dags=F)))
		mean_shd <- apply(shd, mean, MARGIN=2)
		sd_shd <- apply(shd, sd, MARGIN=2)/sqrt(R)
		results <- rbind(results, c(oracle_acc, edge_prob, mean_shd, sd_shd))

		shd_pruned <- t(future_replicate(R, run.sim(n_nodes=n_nodes, edge_prob=edge_prob, oracle_acc=oracle_acc, prune_dags=T), future.chunk.size=3))
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
