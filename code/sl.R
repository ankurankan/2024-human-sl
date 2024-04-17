source('test_dag.R')

library(tidyverse)

N_NODES <- 10

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

  ans_mat[as.matrix(edg[c("v", "w")])] <- 1
  return(ans_mat)
}

varnames <- sapply(1:N_NODES, function(v) {
    paste0("x", v)
})

dag <- pcalg::randomDAG(n=N_NODES, prob=0.4, lB=1, uB=1, V=varnames)
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
learned_edges <- learned_edges %>% discard(is.null)
learned_dag <- dagitty(paste0("dag{", paste0(names(dag), collapse=" "), " ", paste0(lapply(learned_edges, function(x) paste0(x, collapse=" -> ")), collapse=" ") ,"}"))

true_adj <- dag_to_adjmatrix(dag)
learned_adj <- dag_to_adjmatrix(learned_dag)
dist <- sum(abs(true_adj - learned_adj))
print(dist)
