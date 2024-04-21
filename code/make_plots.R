library(ggplot2)
library(dplyr)


make_shd_plot <- function(filename, plot_filename){
	d <- read.csv(filename, row.names=1)
	d$edge_prob <- sapply(d$edge_prob, as.character)

	p <- ggplot(d, aes(x=oracle_acc, y=mean_shd_human, group=edge_prob, color=edge_prob)) + 
		geom_line() +
		geom_point() +
		geom_ribbon(aes(ymin=mean_shd_human - std_error_human, ymax=mean_shd_human + std_error_human), alpha=.2) + 
      		theme_minimal(base_size = 8) + 
		theme(legend.position='top') +
		labs(x = "Oracle Accuracy") +
		labs(y = "Mean SHD") + 
		labs(color = "Edge Probability")

	ggsave(plot_filename, p)
}

make_shd_plot(filename='results/sl_results.csv', plot_filename='plots/shd.pdf')
make_shd_plot(filename='results/sl_results_pruned.csv', plot_filename='plots/shd_pruned.pdf')
