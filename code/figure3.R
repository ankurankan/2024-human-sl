source('data.R')
source('ci_test.R')
source('test_scripts/test_dag.R')


library(future.apply)
library(tidyverse)
library(progress)
plan(multisession)

EFFECT_THRESHOLD <- 0.05
PVALUE_THRESHOLD <- 0.05

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
		
		# Take only potential edges with significant correlation.
		effects <- compute_effects_v2(new_dag, sim_data) %>% filter(p < PVALUE_THRESHOLD)
		effects <- effects[, c('X', 'A', 'Y', 'cor')]

		# If edges are present do pruning.
		edges_to_prune <- effects %>% filter(A == '->') %>% filter(abs(cor) < EFFECT_THRESHOLD)
		if (edges_to_prune %>% nrow() > 0){
			new_dag <- remove_edges(new_dag, edges_to_prune)
		}
		
		# Recompute effects after pruning. We don't really need to do this.
		effects <- compute_effects_v2(new_dag, sim_data) %>% filter(p < PVALUE_THRESHOLD)
		effects <- effects[, c('X', 'A', 'Y', 'cor')]
		unexplain_cor_sorted <- effects %>% filter(abs(as.double(effects$cor)) > EFFECT_THRESHOLD) %>% filter(A == '--') %>% arrange(desc(abs(cor)))

		# Remove blacklisted edges
		if (blacklist_edges %>% nrow() > 0){
			unexplain_cor_sorted <- unexplain_cor_sorted %>%
				filter(!((X %in% blacklist_edges[, 1]) &
					 (Y %in% blacklist_edges[, 2])))
		}

		# If no unexplained correlation is remaining, exit loop.
		if(unexplain_cor_sorted %>% nrow() == 0){
			print(paste0(counter, ": All corr explained"))
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
				temp_dag <- add_edge(new_dag, oracle_edge)
				if (dagitty::isAcyclic(temp_dag)){
					new_dag <- temp_dag
				}
				else{
					print("Not adding edge to avoid cycle.")
					blacklist_edges <- rbind(blacklist_edges, oracle_edge)
				}
			}
		}
	}
	return(dag_to_adjmatrix(new_dag))
}

gen_lin_data <- function(n_nodes, edge_prob){
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

run_single_exp_hc <- function(n_nodes, edge_prob){
	d <- gen_lin_data(n_nodes=n_nodes, edge_prob=edge_prob)
	true_adj <- d$true_adj
	sim_data <- d$sim_data
	var_types <- d$var_types
	# All continuous
	if (all(unlist(var_types) == 'cont')){
		scoring_method <- 'bic-g'
	}
	# All discrete 
	else if (all(unlist(var_types) != 'cont')){
		scoring_method <- 'bic'
	}
	# Mixture
	else{
		scoring_method <- 'bic-cg'
	}

	hc_dag <- bnlearn::hc(sim_data, score=scoring_method)
	hc_adj <- bnlearn::amat(hc_dag)
	hc_pdag <- pcalg::dag2cpdag(hc_adj)
	hc_alldags <- pcalg::pdag2allDags(hc_pdag)$dags

	# TODO: Understand this better.
	if (is.null(nrow(hc_alldags))){
		print("No orientation found from PDAG. Using original DAG.")
		shd <- causalDisco::shd(hc_adj, true_adj)
	}
	else{
		shd <- apply(hc_alldags, function(x) causalDisco::shd(matrix(x, n_nodes, n_nodes), true_adj), MARGIN=1)
	}
	sid <- SID::structIntervDist(trueGraph=true_adj, estGraph=hc_pdag)
	return(c(min(shd), max(shd), sid$sidLowerBound, sid$sidUpperBound))
}

run_single_exp_pc <- function(n_nodes, edge_prob){
	d <- gen_lin_data(n_nodes=n_nodes, edge_prob=edge_prob)
	true_adj <- d$true_adj
	sim_data <- d$sim_data

    	suffStat <- list(dm = sim_data, adaptDF = FALSE, test = 'glm_q3')
    	alpha <- 0.05

	pc.cpdag <- pcalg::pc(
		suffStat,
		indepTest = sl.test,
		alpha = alpha,
		u2pd='relaxed',
		labels = colnames(sim_data),
	    	skel.method = "stable",
	    	verbose = FALSE
	)
	pc_adj <- as(pc.cpdag, 'amat')
	pc_alldags <- pcalg::pdag2allDags(pc_adj)$dags

	sid <- SID::structIntervDist(trueGraph=true_adj, estGraph=pc_adj)	
	if (is.null(nrow(pc_alldags))){
		print("No orientation found from PDAG. Skipping computing SHD.")
		return(c(NA, NA, sid$sidLowerBound, sid$sidUpperBound))
	}
	else{
		shd <- apply(pc_alldags, function(x) causalDisco::shd(matrix(x, n_nodes, n_nodes), true_adj), MARGIN=1)
		return(c(min(shd), max(shd), sid$sidLowerBound, sid$sidUpperBound))
	}
}

run_single_exp_human <- function(n_nodes, edge_prob, oracle_acc){
	d <- gen_lin_data(n_nodes=n_nodes, edge_prob=edge_prob)
	true_dag <- d$true_dag
	true_adj <- d$true_adj
	sim_data <- d$sim_data

	human_adj <- simulate_human_sl(sim_data=sim_data, true_dag=true_dag, oracle_acc=oracle_acc)
	shd <- causalDisco::shd(human_adj, true_adj)
	sid <- SID::structIntervDist(trueGraph=true_adj, estGraph=human_adj)$sid

	return(c(shd, sid))
}


run_sim <- function(R, n_nodes, edge_probs, oracle_accs){
	results <- data.frame()
	
	pb <- progress_bar$new(total=length(oracle_accs) * length(edge_probs))
	for (edge_prob in edge_probs){
		hc_dist <- t(replicate(R, run_single_exp_hc(n_nodes=n_nodes, edge_prob=edge_prob)))
		hc_mean <- apply(hc_dist, mean, MARGIN=2)
		hc_sd <- apply(hc_dist, sd, MARGIN=2)/sqrt(R)

		pc_dist <- t(future_replicate(R, run_single_exp_pc(n_nodes=n_nodes, edge_prob=edge_prob)))
		pc_mean <- apply(pc_dist, function(x) mean(x, na.rm=T), MARGIN=2)
		pc_sd <- apply(pc_dist, function(x) sd(x, na.rm=T), MARGIN=2)/sqrt(sum(!is.na(pc_dist[, 1])))

		for (oracle_acc in oracle_accs){
			human_dist <- t(future_replicate(R, run_single_exp_human(n_nodes=n_nodes, edge_prob=edge_prob, oracle_acc=oracle_acc)))

			human_mean <- apply(human_dist, mean, MARGIN=2)
			human_sd <- apply(human_dist, sd, MARGIN=2)/sqrt(R)
			
			results <- rbind(results, c(oracle_acc, edge_prob, hc_mean, hc_sd, pc_mean, pc_sd, human_mean, human_sd))
			pb$tick()
		}
	}
	colnames(results) <- c('oracle_acc', 'edge_prob', 
			       'hc_lower_shd_mean', 'hc_upper_shd_mean', 'hc_lower_sid_mean', 'hc_upper_sid_mean', 
			       'hc_lower_shd_sd', 'hc_upper_shd_sd', 'hc_lower_sid_sd', 'hc_upper_sid_sd',
			       'pc_lower_shd_mean', 'pc_upper_shd_mean', 'pc_lower_sid_mean', 'pc_upper_sid_mean', 
			       'pc_lower_shd_sd', 'pc_upper_shd_sd', 'pc_lower_sid_sd', 'pc_upper_sid_sd',
			       'human_shd_mean', 'human_sid_mean', 'human_shd_sd', 'human_sid_sd')
	write.csv(results, 'results/sl_results_mixed.csv')
}


edge_probs <- seq(0.1, 0.9, 0.1)
oracle_accs <- seq(0.1, 0.9, 0.1)
n_nodes <- 10
R <- 30

run_sim(R, n_nodes, edge_probs, oracle_accs)
