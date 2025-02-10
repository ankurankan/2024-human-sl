library(ggplot2)


d_expert <- read.csv('results/unexplained_effect.txt', header=F)
d_expert <- t(d_expert)
colnames(d_expert) <- c('pillai', 'll')
rownames(d_expert) <- seq(1, nrow(d_expert))
d_expert <- as.data.frame(d_expert)
d_expert <- cbind(d_expert, x=seq(1, nrow(d_expert)))
d_expert$algo <- 'Expert'

d_ges <- read.csv('results/ges_unexplained_effect.txt', header=F)
d_ges <- as.data.frame(t(d_ges))
colnames(d_ges) <- c('pillai', 'll')
rownames(d_ges) <- seq(1, nrow(d_ges))
d_ges <- cbind(d_ges, x=seq(1, nrow(d_ges)))
d_ges$algo <- 'GES'

d_results <- rbind(d_expert, d_ges)
d_results <- d_results[d_results$x < 31, ]

p_unexplained_effect <- ggplot(d_results, aes(x=x, y=pillai, color=algo)) +
	geom_line(alpha=0.6) +
	geom_point(alpha=0.6, show.legend=F) +
	theme_minimal(base_size=8) +
	labs(x="Iteration No.") +
	labs(y="Total Residual Association") + 
	theme(legend.position='top',
	      legend.title=element_blank())

p_ll <- ggplot(d_results, aes(x=x, y=ll, color=algo)) +
	geom_line(alpha=0.6) +
	geom_point(alpha=0.6, show.legend=F) +
	theme_minimal(base_size=8) +
	labs(x="Iteration No.") +
	labs(y="Log-Likelihood") +
	theme(legend.position='top',
	      legend.title=element_blank())

ggsave('unexplained_effect.pdf', p_unexplained_effect, height=2.0, width=1.6, units='in')
ggsave('ll.pdf', p_ll, height=2.0, width=1.6, units='in')
