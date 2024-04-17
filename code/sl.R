source('test_dag.R')

library(tidyverse)

N_NODES <- 5

oracle <- function(u, v, true_dag, accuracy=0.9){
	if (runif(1) < accuracy){
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

varnames <- sapply(1:N_NODES, function(v) {
    paste0("x", v)
})

dag <- pcalg::randomDAG(n=N_NODES, prob=0.6, lB=1, uB=1, V=varnames)
dag <- pcalg::pcalg2dagitty(as(dag, "matrix"), labels = varnames, type = "dag")
sim_data <- simulateSEM(dag, empirical=T)

marg_cor <- compute_effects_marg(dag, sim_data)[, c('u', 'v', 'cor')]
marg_cor_thresh <- marg_cor[as.double(marg_cor$cor) > 0.05, ]
marg_cor_thresh_sorted <- marg_cor_thresh %>% arrange(desc(marg_cor_thresh$cor))

learned_edges <- list()
for (i_row in 1:nrow(marg_cor_thresh_sorted)){
	oracle_edge <- oracle(marg_cor_thresh_sorted[i_row, 'u'], marg_cor_thresh_sorted[i_row, 'v'], true_dag=dag)
	learned_edges[[i_row]] <- oracle_edge
}
print(learned_edges)
