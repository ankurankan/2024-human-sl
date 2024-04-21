library(ggplot2)
library(dplyr)


make_shd_plot <- function(filename, plot_filename){
	d <- read.csv(filename, row.names=1)
	d$edge_prob <- sapply(d$edge_prob, as.character)

	d_long <- rbind(d, d)
	d_long[(nrow(d)+1):nrow(d_long), 'mean_shd_human'] <- d[, 'mean_shd_hc']
	d_long[(nrow(d)+1):nrow(d_long), 'std_error_human'] <- d[, 'std_error_hc']
	d_long <- d_long %>% select('oracle_acc', 'edge_prob', 'mean_shd_human', 'std_error_human')
	colnames(d_long) <- c('oracle_acc', 'edge_prob', 'mean_shd', 'std_error')
	d_long$alg <- c(rep('human', nrow(d)), rep('hc', nrow(d)))

	p <- ggplot(d_long, aes(x=oracle_acc, y=mean_shd, color=edge_prob, shape=alg, group=interaction(edge_prob, alg))) + 
		geom_line() +
		geom_point() +
		geom_ribbon(aes(ymin=mean_shd - std_error, ymax=mean_shd + std_error, color=edge), alpha=.2, linetype=0, show.legend=F) + 
      		theme_minimal(base_size = 8) + 
		theme(legend.position='top') +
		labs(x = "Oracle Accuracy") +
		labs(y = "Mean SHD") + 
		labs(color = "Edge Probability")

	ggsave(plot_filename, p)
}

make_shd_plot(filename='results/sl_results.csv', plot_filename='plots/shd.pdf')
make_shd_plot(filename='results/sl_results_pruned.csv', plot_filename='plots/shd_pruned.pdf')
