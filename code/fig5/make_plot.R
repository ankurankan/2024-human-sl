library(ggplot2)


d_expert <- read.csv('results/unexplained_effect.txt', header=F)
d_expert <- t(d_expert)
colnames(d_expert) <- c('expert_pillai', 'expert_ll')
rownames(d_expert) <- seq(1, nrow(d_expert))
d_expert <- as.data.frame(d_expert)
d_expert <- cbind(d_expert, x=seq(1, nrow(d_expert)))

d_ges <- read.csv('results/ges_unexplained_effect.txt', header=F)
d_ges <- as.data.frame(t(d_ges))
colnames(d_ges) <- c('ges_pillai', 'ges_ll')
rownames(d_ges) <- seq(1, nrow(d_ges))
d_ges <- cbind(d_ges, x=seq(1, nrow(d_ges)))

p <- ggplot(d_expert, aes(x=x, y=pillai)) +
	geom_line(alpha=0.6, show.legend=F) +
	geom_point() +
	theme_minimal(base_size=8) +
	labs(x="Iteration No.") +
	labs(y="Unexplained Effect")

ggsave('unexplained.pdf', p, height=2.5, width=3.2, units='in')


