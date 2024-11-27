library(ggplot2)


d <- read.csv('results/unexplained_effect.txt', header=F)
d <- t(d)
colnames(d) <- c('pillai', 'log-like')
rownames(d) <- seq(1, nrow(d))
d <- as.data.frame(d)
d <- cbind(d, x=seq(1, nrow(d)))

p <- ggplot(d, aes(x=x, y=pillai)) +
	geom_line(alpha=0.6, show.legend=F) +
	geom_point() +
	theme_minimal(base_size=8) +
	labs(x="Iteration No.") +
	labs(y="Unexplained Effect")

ggsave('unexplained.pdf', p, height=2.5, width=3.2, units='in')


