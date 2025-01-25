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
            browser()
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


