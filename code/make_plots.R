library(ggplot2)
library(dplyr)


make_shd_plot <- function(filename='results/sl_results.csv'){
	d <- read.csv(filename, row.names=1)
	d$edge_prob <- sapply(d$edge_prob, as.character)

	p <- ggplot(d, aes(x=oracle_acc, y=mean_shd, group=edge_prob, color=edge_prob)) + 
		geom_line() +
		geom_point() +
		geom_ribbon(aes(ymin=mean_shd - std_error, ymax=mean_shd + std_error), alpha=.2) + 
      		theme_minimal(base_size = 8) + 
		theme(legend.position='top') +
		labs(x = "Oracle Accuracy") +
		labs(y = "Mean SHD") + 
		labs(color = "Edge Probability")

	ggsave('plots/shd.pdf', p)
}

make_shd_plot()
