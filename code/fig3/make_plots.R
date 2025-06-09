library(ggplot2)
library(dplyr)
library(tibble)
library(directlabels)


# make_plots <- function(filename, plot_type){
# 	d <- read.csv(filename, row.names=1)
# 	# d <- d[d$edge_prob %in% c(0.1, 0.3, 0.5, 0.7, 0.9), ]
# 	d <- d[d$oracle_acc %in% c(0.1, 0.3, 0.5, 0.7, 0.9), ]
# 
# 	d$edge_prob <- sapply(d$edge_prob, as.character)
# 	col_names <- c('oracle_acc', 'edge_prob', 'shd_mean', 'sid_mean', 'shd_sd', 'sid_sd', 'alg')
# 
# 	d_ges_lower <- d %>% select(oracle_acc, edge_prob, ges_lower_shd_mean, ges_lower_sid_mean, ges_lower_shd_sd, ges_lower_sid_sd) %>% add_column(alg='ges_lower')
# 	colnames(d_ges_lower) <- col_names
# 	d_ges_upper <- d %>% select(oracle_acc, edge_prob, ges_upper_shd_mean, ges_upper_sid_mean, ges_upper_shd_sd, ges_upper_sid_sd) %>% add_column(alg='ges_upper')
# 	colnames(d_ges_upper) <- col_names
# 
# 	d_hc_lower <- d %>% select(oracle_acc, edge_prob, hc_lower_shd_mean, hc_lower_sid_mean, hc_lower_shd_sd, hc_lower_sid_sd) %>% add_column(alg='hc_lower')
# 	colnames(d_hc_lower) <- col_names
# 	d_hc_upper <- d %>% select(oracle_acc, edge_prob, hc_upper_shd_mean, hc_upper_sid_mean, hc_upper_shd_sd, hc_upper_sid_sd) %>% add_column(alg='hc_upper')
# 	colnames(d_hc_upper) <- col_names
# 	
# 	d_pc_lower <- d %>% select(oracle_acc, edge_prob, pc_lower_shd_mean, pc_lower_sid_mean, pc_lower_shd_sd, pc_lower_sid_sd) %>% add_column(alg='pc_lower')
# 	colnames(d_pc_lower) <- col_names
# 	d_pc_upper <- d %>% select(oracle_acc, edge_prob, pc_upper_shd_mean, pc_upper_sid_mean, pc_upper_shd_sd, pc_upper_sid_sd) %>% add_column(alg='pc_upper')
# 	colnames(d_pc_upper) <- col_names
# 
# 	d_human <- d %>% select(oracle_acc, edge_prob, human_shd_mean:human_sid_sd) %>% add_column(alg='human')
# 	colnames(d_human) <- col_names
# 
# 	d_ges_lower <- d_ges_lower %>% select(!oracle_acc) %>% distinct()
# 	d_ges_upper <- d_ges_upper %>% select(!oracle_acc) %>% distinct()
# 	d_ges_lower$color <- 'GES'
# 	d_ges_lower$linetype <- 'solid'
# 	d_ges_upper$color <- 'GES'
# 	d_ges_upper$linetype <- 'solid'
# 
# 	d_hc_lower <- d_hc_lower %>% select(!oracle_acc) %>% distinct()
# 	d_hc_upper <- d_hc_upper %>% select(!oracle_acc) %>% distinct()
# 	d_hc_lower$color <- 'Hill-Climb'
# 	d_hc_lower$linetype <- 'solid'
# 	d_hc_upper$color <- 'Hill-Climb'
# 	d_hc_upper$linetype <- 'solid'
# 
# 	d_pc_lower <- d_pc_lower %>% select(!oracle_acc) %>% distinct()
# 	d_pc_upper <- d_pc_upper %>% select(!oracle_acc) %>% distinct()
# 	d_pc_lower$color <- 'PC'
# 	d_pc_lower$linetype <- 'solid'
# 	d_pc_upper$color <- 'PC'
# 	d_pc_upper$linetype <- 'solid'
# 
# 	o_acc <- d_human$oracle_acc
# 	d_human$alg <- paste(d_human$alg, d_human$oracle_acc)
# 	d_human$color <- 'Expert'
# 	d_human$linetype <- 'dashed'
# 	d_human <- d_human %>% select(!oracle_acc)
# 
# 	if (plot_type == 'lines'){
# 		d_long_shd <- rbind(d_ges_lower, d_ges_upper, d_hc_lower, d_hc_upper, d_pc_lower, d_pc_upper, d_human)
# 		p_shd <- ggplot(d_long_shd, aes(x=edge_prob, y=shd_mean, group=alg, color=color)) + 
# 			geom_line(aes(linetype=linetype)) +
# 			scale_linetype_manual(values=c('dashed', 'solid')) + 
# 			geom_point() +
# 			geom_ribbon(aes(ymin=shd_mean - shd_sd, ymax=shd_mean + shd_sd, fill=color), alpha=.2, linetype=0, show.legend=F) + 
#       			theme_minimal(base_size = 8) + 
# 			theme(legend.position='top') +
# 			labs(x = "Edge Probability") +
# 			labs(y = "Mean SHD") + 
# 			labs(color = "Algorithm") +
# 			ylim(0, 40)
# 
# 		ggsave("plots/shd.pdf", p_shd, height=2.8, width=5, units='in')
# 
# 		d_long_sid <- rbind(d_hc_lower, d_hc_upper, d_pc_lower, d_pc_upper, d_human)
# 		p_sid <- ggplot(d_long_sid, aes(x=edge_prob, y=sid_mean, group=alg, color=color))+ 
# 			geom_line(aes(linetype=linetype)) +
# 			scale_linetype_manual(values=c('dashed', 'solid')) + 
# 			geom_point() +
# 			geom_ribbon(aes(ymin=sid_mean - sid_sd, ymax=sid_mean + sid_sd, fill=color), alpha=.2, linetype=0, show.legend=F) + 
#       			theme_minimal(base_size = 8) + 
# 			theme(legend.position='top') +
# 			labs(x = "Edge Probability") +
# 			labs(y = "Mean SID") + 
# 			labs(color = "Algorithm") +
# 			ylim(0, 80)
# 
# 		ggsave("plots/sid.pdf", p_sid, height=2.8, width=5, units='in')
# 	}
# 	else if(plot_type == 'ribbon'){
# 		d_ges_wide <- cbind(d_ges_lower, shd_upper_mean=d_ges_upper$shd_mean, sid_upper_mean=d_ges_upper$sid_mean)
# 		d_hc_wide <- cbind(d_hc_lower, shd_upper_mean=d_hc_upper$shd_mean, sid_upper_mean=d_hc_upper$sid_mean)
# 		d_pc_wide <- cbind(d_pc_lower, shd_upper_mean=d_pc_upper$shd_mean, sid_upper_mean=d_pc_upper$sid_mean)
# 		d_wide <- rbind(d_ges_wide, d_hc_wide, d_pc_wide)
# 
# 		d_human <- cbind(d_human, oracle_acc=o_acc)
# 
# 		p_shd <- ggplot(d_human, aes(x=edge_prob, y=shd_mean, group=alg, color=color)) +
# 			guides(color='none') +
# 			geom_line(show.legend=F) +
# 			geom_point(show.legend=F) + 
# 			geom_ribbon(aes(ymin=shd_mean - shd_sd, ymax=shd_mean + shd_sd, fill=color), alpha=.2, linetype=0, show.legend=F) +
# 			geom_dl(aes(label=oracle_acc), color='black', method=list(dl.trans(x=x+.2), "last.points", cex=0.5)) + 
# 			geom_ribbon(data=d_wide, aes(x=edge_prob, ymin=shd_mean-shd_sd, ymax=shd_upper_mean+shd_sd, group=alg, fill=color), alpha=0.2, linetype=2, show.legend=T) +
# 			guides(color='none') +
#       			theme_minimal(base_size = 8) + 
# 			theme(legend.position='top') +
# 			labs(x = "Edge Probability") +
# 			labs(y = "Mean SHD") + 
# 			labs(color = "Algorithm") +
# 			ylim(0, 40)
# 			
# 		ggsave("plots/shd_ribbon.pdf", p_shd, height=2.5, width=2.6, units='in')
# 
# 		p_sid <- ggplot(d_human, aes(x=edge_prob, y=sid_mean, group=alg, color=color)) +
# 			guides(color='none') +
# 			geom_line(show.legend=F) +
# 			geom_point(show.legend=F) + 
# 			geom_ribbon(aes(ymin=sid_mean - sid_sd, ymax=sid_mean + sid_sd, fill=color), alpha=.2, linetype=0, show.legend=F) +
# 			geom_dl(aes(label=oracle_acc), color='black', method=list(dl.trans(x=x+.2), "last.points", cex=0.5)) + 
# 			geom_ribbon(data=d_wide, aes(x=edge_prob, ymin=sid_mean-sid_sd, ymax=sid_upper_mean+sid_sd, group=alg, fill=color), alpha=0.2, linetype=2, show.legend=T) +
# 			guides(color='none') +
#       			theme_minimal(base_size = 8) + 
# 			theme(legend.position='top') +
# 			labs(x = "Edge Probability") +
# 			labs(y = "Mean SID") + 
# 			labs(color = "Algorithm") +
# 			ylim(0, 100)
# 			
# 		ggsave("plots/sid_ribbon.pdf", p_sid, height=2.5, width=2.6, units='in')
# 	}
# }

make_plots_v2 <- function(filename, plot_type){
	d <- read.csv(filename, row.names=1)
	# d <- d[d$edge_prob %in% c(0.1, 0.3, 0.5, 0.7, 0.9), ]
	d <- d[d$oracle_acc %in% seq(0, 0.9, 0.2), ]

	d$edge_prob <- sapply(d$edge_prob, as.character)

	d_shd <- d %>% select(edge_prob, oracle_acc, ges_lower_shd_mean, ges_upper_shd_mean, ges_lower_shd_sd, ges_upper_shd_sd, hc_lower_shd_mean, hc_upper_shd_mean, hc_lower_shd_sd, hc_upper_shd_sd, pc_lower_shd_mean, pc_upper_shd_mean, pc_lower_shd_sd, pc_upper_shd_sd, human_shd_mean, human_shd_sd)

	d_shd$ges_lower <- d_shd$ges_lower_shd_mean - d_shd$ges_lower_shd_sd
	d_shd$ges_upper <- d_shd$ges_upper_shd_mean + d_shd$ges_upper_shd_sd
	d_shd$ges_mean <- (d_shd$ges_lower + d_shd$ges_upper)/2

	d_shd$hc_lower <- d_shd$hc_lower_shd_mean - d_shd$hc_lower_shd_sd
	d_shd$hc_upper <- d_shd$hc_upper_shd_mean + d_shd$hc_upper_shd_sd
	d_shd$hc_mean <- (d_shd$hc_lower + d_shd$hc_upper)/2

	d_shd$pc_lower <- d_shd$pc_lower_shd_mean - d_shd$pc_lower_shd_sd
	d_shd$pc_upper <- d_shd$pc_upper_shd_mean + d_shd$pc_upper_shd_sd
	d_shd$pc_mean <- (d_shd$pc_lower + d_shd$pc_upper)/2

	d_shd$human_lower <- d_shd$human_shd_mean - d_shd$human_shd_sd
	d_shd$human_upper <- d_shd$human_shd_mean + d_shd$human_shd_sd
	d_shd$human_mean <- d_shd$human_shd_mean

	d_shd <- d_shd %>% select(edge_prob, oracle_acc, ges_mean, ges_lower, ges_upper, hc_mean, hc_lower, hc_upper, pc_mean, pc_lower, pc_upper, human_mean, human_lower, human_upper)

	d_shd <- as.data.frame(cbind(c(d_shd$edge_prob, d_shd$edge_prob, d_shd$edge_prob),
	      	       c(d_shd$oracle_acc, d_shd$oracle_acc, d_shd$oracle_acc),
	      	       c(d_shd$ges_mean, d_shd$hc_mean, d_shd$pc_mean, d_shd$human_mean),
	      	       c(d_shd$ges_lower, d_shd$hc_lower, d_shd$pc_lower, d_shd$human_lower),
	      	       c(d_shd$ges_upper, d_shd$hc_upper, d_shd$pc_upper, d_shd$human_upper),
		       c(rep('GES', nrow(d_shd)), rep('Hill-Climb', nrow(d_shd)), rep('PC', nrow(d_shd)), rep('Expert', nrow(d_shd))),
		       c(rep('ges', nrow(d_shd)), rep('hc', nrow(d_shd)), rep('pc', nrow(d_shd)), paste('human', d_shd$oracle_acc)),
		       c(rep(NA, nrow(d_shd)), rep(NA, nrow(d_shd)), rep(NA, nrow(d_shd)), d_shd$oracle_acc)
		       ))
	colnames(d_shd) <- c('edge_prob', 'oracle_acc', 'mean_val', 'lower', 'upper', 'algo', 'grp', 'line_labels')

	d_shd$oracle_acc <- NULL
	d_shd <- d_shd %>% distinct()
	d_shd$mean_val <- as.double(d_shd$mean_val)
	d_shd$lower <- as.double(d_shd$lower)
	d_shd$upper <- as.double(d_shd$upper)

	d_shd$line_labels <- round(as.numeric(d_shd$line_labels) + ((1 - as.numeric(d_shd$line_labels))/3), 2)
	p_shd <- ggplot(d_shd, aes(x=edge_prob, y=mean_val, 
				   ymin=lower, ymax=upper, 
				   group=grp, color=algo, 
				   fill=algo,
				   linetype=ifelse(algo %in% c('GES', 'PC', 'Hill-Climb'), NA, "solid"))) + 
		geom_line(alpha=0.4, show.legend=T) +
		geom_point(data=subset(d_shd, !(algo %in% c('GES', 'PC', 'Hill-Climb'))), pch=19, size=1, alpha=0.4, show.legend=F) +
		geom_ribbon(linetype=0, alpha=0.4, show.legend=F) + 
		geom_dl(aes(label=line_labels), color='black', method=list(dl.trans(x=x+.1), "last.points", cex=0.5)) + 
      		theme_minimal(base_size = 8) + 
		theme(legend.position='top',
		      legend.title=element_blank(),
		      legend.text=element_text(size=8)) +
		labs(x = "Edge Probability") +
		labs(y = "SHD") + 
		labs(color = "Algorithm") +
		ylim(0, 40) +
		guides(linetype='none', fill='none')

	ggsave("shd_ribbon.pdf", p_shd, height=2, width=3.37, units='in')


	d_sid <- d %>% select(edge_prob, oracle_acc, ges_lower_sid_mean, ges_upper_sid_mean, ges_lower_sid_sd, ges_upper_sid_sd, hc_lower_sid_mean, hc_upper_sid_mean, hc_lower_sid_sd, hc_upper_sid_sd, pc_lower_sid_mean, pc_upper_sid_mean, pc_lower_sid_sd, pc_upper_sid_sd, human_sid_mean, human_sid_sd)

	d_sid$ges_lower <- d_sid$ges_lower_sid_mean - d_sid$ges_lower_sid_sd
	d_sid$ges_upper <- d_sid$ges_upper_sid_mean + d_sid$ges_upper_sid_sd
	d_sid$ges_mean <- (d_sid$ges_lower + d_sid$ges_upper)/2

	d_sid$hc_lower <- d_sid$hc_lower_sid_mean - d_sid$hc_lower_sid_sd
	d_sid$hc_upper <- d_sid$hc_upper_sid_mean + d_sid$hc_upper_sid_sd
	d_sid$hc_mean <- (d_sid$hc_lower + d_sid$hc_upper)/2

	d_sid$pc_lower <- d_sid$pc_lower_sid_mean - d_sid$pc_lower_sid_sd
	d_sid$pc_upper <- d_sid$pc_upper_sid_mean + d_sid$pc_upper_sid_sd
	d_sid$pc_mean <- (d_sid$pc_lower + d_sid$pc_upper)/2

	d_sid$human_lower <- d_sid$human_sid_mean - d_sid$human_sid_sd
	d_sid$human_upper <- d_sid$human_sid_mean + d_sid$human_sid_sd
	d_sid$human_mean <- d_sid$human_sid_mean

	d_sid <- d_sid %>% select(edge_prob, oracle_acc, ges_mean, ges_lower, ges_upper, hc_mean, hc_lower, hc_upper, pc_mean, pc_lower, pc_upper, human_mean, human_lower, human_upper)

	d_sid <- as.data.frame(cbind(c(d_sid$edge_prob, d_sid$edge_prob, d_sid$edge_prob),
	      	       c(d_sid$oracle_acc, d_sid$oracle_acc, d_sid$oracle_acc),
	      	       c(d_sid$ges_mean, d_sid$hc_mean, d_sid$pc_mean, d_sid$human_mean),
	      	       c(d_sid$ges_lower, d_sid$hc_lower, d_sid$pc_lower, d_sid$human_lower),
	      	       c(d_sid$ges_upper, d_sid$hc_upper, d_sid$pc_upper, d_sid$human_upper),
		       c(rep('GES', nrow(d_sid)), rep('Hill-Climb', nrow(d_sid)), rep('PC', nrow(d_sid)), rep('Expert', nrow(d_sid))),
		       c(rep('ges', nrow(d_sid)), rep('hc', nrow(d_sid)), rep('pc', nrow(d_sid)), paste('human', d_sid$oracle_acc)),
		       c(rep(NA, nrow(d_sid)), rep(NA, nrow(d_sid)), rep(NA, nrow(d_sid)), d_sid$oracle_acc)
		       ))
	colnames(d_sid) <- c('edge_prob', 'oracle_acc', 'mean_val', 'lower', 'upper', 'algo', 'grp', 'line_labels')

	d_sid$oracle_acc <- NULL
	d_sid <- d_sid %>% distinct()
	d_sid$mean_val <- as.double(d_sid$mean_val)
	d_sid$lower <- as.double(d_sid$lower)
	d_sid$upper <- as.double(d_sid$upper)

	d_sid$line_labels <- round(as.numeric(d_sid$line_labels) + ((1 - as.numeric(d_sid$line_labels))/3), 2)
	p_sid <- ggplot(d_sid, aes(x=edge_prob, y=mean_val, 
				   ymin=lower, ymax=upper, 
				   group=grp, color=algo, 
				   fill=algo, linetype=ifelse(algo %in% c('GES', 'PC', 'Hill-Climb'), NA, "solid"))) + 
		geom_line(alpha=0.4, show.legend=T) +
		geom_point(data=subset(d_sid, !(algo %in% c('GES', 'PC', 'Hill-Climb'))), pch=19, size=1, alpha=0.4, show.legend=F) +
		geom_ribbon(linetype=0, alpha=0.4, show.legend=F) + 
		geom_dl(aes(label=line_labels), color='black', method=list(dl.trans(x=x+.1), "last.points", cex=0.5)) + 
      		theme_minimal(base_size = 8) + 
		theme(legend.position='top',
		      legend.title=element_blank(),
		      legend.text=element_text(size=8)) +
		labs(x = "Edge Probability") +
		labs(y = "SID") + 
		labs(color = "Algorithm") +
		ylim(0, 90) +
		guides(linetype='none', fill='none')

	ggsave("sid_ribbon.pdf", p_sid, height=2, width=3.37, units='in')
}

make_plots_v2(filename='results/sl_results_mixed.csv', plot_type='ribbon')

