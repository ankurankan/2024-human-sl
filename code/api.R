source('test_dag.R')

#* Return the sum of two numbers
#* @param dag The dag to compute the marginals on.
#* @param d:file
#* @post /marginal
#* @parser multi
#* @parser csv
function(dag, d){
	dag <- dagitty::dagitty(dag)
	marginal_cor <- compute_effects_marg(dag=dag, data=as.data.frame(d[[1]]))
	marginal_cor$cor <- round(as.double(marginal_cor$cor), 3)
	return(marginal_cor)
}


#* Return the sum of two numbers
#* @param dag The dag to compute the conditional correlations on.
#* @param d:file
#* @post /conditional
#* @parser multi
#* @parser csv
function(dag, d){
	dag <- dagitty::dagitty(dag)
	cond_cor <- compute_effects_cond(dag=dag, data=as.data.frame(d[[1]]))
	cond_cor$cor <- round(as.double(cond_cor$cor), 3)
	return(cond_cor)
}
