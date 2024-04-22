source('test_dag.R')

#* @filter cors
cors <- function(res) {
    res$setHeader("Access-Control-Allow-Origin", "*") # Or whatever
    plumber::forward()
}


#* Return the sum of two numbers
#* @param dag The dag to compute the marginals on.
#* @param d:file
#* @post /marginal
#* @preempt cors
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
#* @preempt cors
function(dag, d){
	dag <- dagitty::dagitty(dag)
	cond_cor <- compute_effects_cond(dag=dag, data=as.data.frame(d[[1]]))
	cond_cor$cor <- round(as.double(cond_cor$cor), 3)
	return(cond_cor)
}


#* @get /simpletest
function(){
	dag <- dagitty::dagitty("X->M->Y")
	marginal_cor <- compute_effects_marg(dag=dag, 
	data=simulateSEM("X->M->Y",b.default=.4,empirical=TRUE))
	marginal_cor$cor <- round(as.double(marginal_cor$cor), 3)
	return(marginal_cor)
}

