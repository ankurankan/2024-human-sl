library(ggplot2)
library(dplyr)
library(tibble)
library(directlabels)


make_plots <- function(filename, plot_type){
	d <- read.csv(filename, row.names=1)
	# d <- d[d$edge_prob %in% c(0.1, 0.3, 0.5, 0.7, 0.9), ]
	d <- d[d$oracle_acc %in% c(0.1, 0.3, 0.5, 0.7, 0.9), ]

	d$edge_prob <- sapply(d$edge_prob, as.character)
	col_names <- c('oracle_acc', 'edge_prob', 'shd_mean', 'sid_mean', 'shd_sd', 'sid_sd', 'alg')

	d_hc_lower <- d %>% select(oracle_acc, edge_prob, hc_lower_shd_mean, hc_lower_sid_mean, hc_lower_shd_sd, hc_lower_sid_sd) %>% add_column(alg='hc_lower')
	colnames(d_hc_lower) <- col_names
	d_hc_upper <- d %>% select(oracle_acc, edge_prob, hc_upper_shd_mean, hc_upper_sid_mean, hc_upper_shd_sd, hc_upper_sid_sd) %>% add_column(alg='hc_upper')
	colnames(d_hc_upper) <- col_names
	
	d_pc_lower <- d %>% select(oracle_acc, edge_prob, pc_lower_shd_mean, pc_lower_sid_mean, pc_lower_shd_sd, pc_lower_sid_sd) %>% add_column(alg='pc_lower')
	colnames(d_pc_lower) <- col_names
	d_pc_upper <- d %>% select(oracle_acc, edge_prob, pc_upper_shd_mean, pc_upper_sid_mean, pc_upper_shd_sd, pc_upper_sid_sd) %>% add_column(alg='pc_upper')
	colnames(d_pc_upper) <- col_names

	d_human <- d %>% select(oracle_acc, edge_prob, human_shd_mean:human_sid_sd) %>% add_column(alg='human')
	colnames(d_human) <- col_names

	d_hc_lower <- d_hc_lower %>% select(!oracle_acc) %>% distinct()
	d_hc_upper <- d_hc_upper %>% select(!oracle_acc) %>% distinct()
	d_hc_lower$color <- 'Hill-Climb'
	d_hc_lower$linetype <- 'solid'
	d_hc_upper$color <- 'Hill-Climb'
	d_hc_upper$linetype <- 'solid'

	d_pc_lower <- d_pc_lower %>% select(!oracle_acc) %>% distinct()
	d_pc_upper <- d_pc_upper %>% select(!oracle_acc) %>% distinct()
	d_pc_lower$color <- 'PC'
	d_pc_lower$linetype <- 'solid'
	d_pc_upper$color <- 'PC'
	d_pc_upper$linetype <- 'solid'

	o_acc <- d_human$oracle_acc
	d_human$alg <- paste(d_human$alg, d_human$oracle_acc)
	d_human$color <- 'Expert'
	d_human$linetype <- 'dashed'
	d_human <- d_human %>% select(!oracle_acc)

	if (plot_type == 'lines'){
		d_long_shd <- rbind(d_hc_lower, d_hc_upper, d_pc_lower, d_pc_upper, d_human)
		p_shd <- ggplot(d_long_shd, aes(x=edge_prob, y=shd_mean, group=alg, color=color)) + 
			geom_line(aes(linetype=linetype)) +
			scale_linetype_manual(values=c('dashed', 'solid')) + 
			geom_point() +
			geom_ribbon(aes(ymin=shd_mean - shd_sd, ymax=shd_mean + shd_sd, fill=color), alpha=.2, linetype=0, show.legend=F) + 
      			theme_minimal(base_size = 8) + 
			theme(legend.position='top') +
			labs(x = "Edge Probability") +
			labs(y = "Mean SHD") + 
			labs(color = "Algorithm") +
			ylim(0, 40)

		ggsave("plots/shd.pdf", p_shd, height=2.8, width=5, units='in')

		d_long_sid <- rbind(d_hc_lower, d_hc_upper, d_pc_lower, d_pc_upper, d_human)
		p_sid <- ggplot(d_long_sid, aes(x=edge_prob, y=sid_mean, group=alg, color=color))+ 
			geom_line(aes(linetype=linetype)) +
			scale_linetype_manual(values=c('dashed', 'solid')) + 
			geom_point() +
			geom_ribbon(aes(ymin=sid_mean - sid_sd, ymax=sid_mean + sid_sd, fill=color), alpha=.2, linetype=0, show.legend=F) + 
      			theme_minimal(base_size = 8) + 
			theme(legend.position='top') +
			labs(x = "Edge Probability") +
			labs(y = "Mean SID") + 
			labs(color = "Algorithm") +
			ylim(0, 80)

		ggsave("plots/sid.pdf", p_sid, height=2.8, width=5, units='in')
	}
	else if(plot_type == 'ribbon'){
		d_hc_wide <- cbind(d_hc_lower, shd_upper_mean=d_hc_upper$shd_mean, sid_upper_mean=d_hc_upper$sid_mean)
		d_pc_wide <- cbind(d_pc_lower, shd_upper_mean=d_pc_upper$shd_mean, sid_upper_mean=d_pc_upper$sid_mean)
		d_wide <- rbind(d_hc_wide, d_pc_wide)

		d_human <- cbind(d_human, oracle_acc=o_acc)

		p_shd <- ggplot(d_human, aes(x=edge_prob, y=shd_mean, group=alg, color=color)) +
			guides(color='none') +
			geom_line(show.legend=F) +
			geom_point(show.legend=F) + 
			geom_ribbon(aes(ymin=shd_mean - shd_sd, ymax=shd_mean + shd_sd, fill=color), alpha=.2, linetype=0, show.legend=F) +
			geom_dl(aes(label=oracle_acc), color='black', method=list(dl.trans(x=x+.2), "last.points", cex=0.5)) + 
			geom_ribbon(data=d_wide, aes(x=edge_prob, ymin=shd_mean-shd_sd, ymax=shd_upper_mean+shd_sd, group=alg, fill=color), alpha=0.2, linetype=2, show.legend=T) +
			guides(color='none') +
      			theme_minimal(base_size = 8) + 
			theme(legend.position='top') +
			labs(x = "Edge Probability") +
			labs(y = "Mean SHD") + 
			labs(color = "Algorithm") +
			ylim(0, 40)
			
		ggsave("plots/shd_ribbon.pdf", p_shd, height=3.1, width=4.8, units='in')

		p_sid <- ggplot(d_human, aes(x=edge_prob, y=sid_mean, group=alg, color=color)) +
			guides(color='none') +
			geom_line(show.legend=F) +
			geom_point(show.legend=F) + 
			geom_ribbon(aes(ymin=sid_mean - sid_sd, ymax=sid_mean + sid_sd, fill=color), alpha=.2, linetype=0, show.legend=F) +
			geom_dl(aes(label=oracle_acc), color='black', method=list(dl.trans(x=x+.2), "last.points", cex=0.5)) + 
			geom_ribbon(data=d_wide, aes(x=edge_prob, ymin=sid_mean-sid_sd, ymax=sid_upper_mean+sid_sd, group=alg, fill=color), alpha=0.2, linetype=2, show.legend=T) +
			guides(color='none') +
      			theme_minimal(base_size = 8) + 
			theme(legend.position='top') +
			labs(x = "Edge Probability") +
			labs(y = "Mean SID") + 
			labs(color = "Algorithm") +
			ylim(0, 100)
			
		ggsave("plots/sid_ribbon.pdf", p_sid, height=3.1, width=4.8, units='in')
	}
}

make_plots(filename='results/sl_results_mixed.csv', plot_type='ribbon')
