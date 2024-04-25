library(ggplot2)
library(dplyr)
library(tibble)


make_shd_plot <- function(filename, plot_filename){
	d <- read.csv(filename, row.names=1)
	d <- d[d$edge_prob %in% c(0.1, 0.5, 0.9), ]

	d$edge_prob <- sapply(d$edge_prob, as.character)

	col_names <- c('oracle_acc', 'edge_prob', 'shd_mean', 'sid_mean', 'shd_sd', 'sid_sd', 'alg')
	d_hc <- d %>% select(oracle_acc, edge_prob, hc_shd_mean:hc_sid_sd) %>% add_column(alg='hc')
	colnames(d_hc) <- col_names
	d_pc <- d %>% select(oracle_acc, edge_prob, pc_sid_best_mean:pc_sid_sd) %>% add_column(alg='pc')
	colnames(d_pc) <- col_names
	d_human <- d %>% select(oracle_acc, edge_prob, human_shd_mean:human_sid_sd) %>% add_column(alg='human')
	colnames(d_human) <- col_names

	d_long <- rbind(d_hc, d_pc, d_human)

	p_shd <- ggplot(d_long, aes(x=oracle_acc, y=shd_mean, color=edge_prob, shape=alg, group=interaction(edge_prob, alg))) + 
		geom_line() +
		geom_point() +
		geom_ribbon(aes(ymin=shd_mean - shd_sd, ymax=shd_mean + shd_sd, color=edge_prob), alpha=.2, linetype=0, show.legend=F) + 
      		theme_minimal(base_size = 8) + 
		theme(legend.position='top') +
		labs(x = "Oracle Accuracy") +
		labs(y = "Mean SHD") + 
		labs(color = "Edge Probability") +
		ylim(0, 50)

	ggsave("plots/shd.pdf", p_shd, units='in')

	p_sid <- ggplot(d_long, aes(x=oracle_acc, y=sid_mean, color=edge_prob, shape=alg, group=interaction(edge_prob, alg))) + 
		geom_line() +
		geom_point() +
		geom_ribbon(aes(ymin=sid_mean - sid_sd, ymax=sid_mean + sid_sd, color=edge_prob), alpha=.2, linetype=0, show.legend=F) + 
      		theme_minimal(base_size = 8) + 
		theme(legend.position='top') +
		labs(x = "Oracle Accuracy") +
		labs(y = "Mean SHD") + 
		labs(color = "Edge Probability") +
		ylim(0, 85)

	ggsave("plots/sid.pdf", p_sid, units='in')
}

make_shd_plot(filename='results/sl_results.csv')
