library(dplyr)

source('ci_tests.R')

d <- read.csv("../utils/adult_unproc.csv")

# Preprocess the data
d$Fnlwgt <- NULL
d$EducationNum <- NULL
d$CapitalGain <- NULL
d$CaptialLoss <- NULL

# Categorical
d$Workclass <- factor(d$Workclass, ordered = F)
d$MaritalStatus <- factor(d$MaritalStatus, ordered = F)
d$Occupation <- factor(d$Occupation, ordered = F)
d$Relationship <- factor(d$Relationship, ordered = F)
d$Race <- factor(d$Race, ordered = F)
d$Sex <- factor(d$Sex, ordered = F)
d$NativeCountry <- factor(d$NativeCountry, ordered = F)
d$Income <- factor(d$Income, ordered = F)

# Continuous
d$Age <- as.double(d$Age)
d$HoursPerWeek <- as.double(d$HoursPerWeek)

# Ordinal
education_levels <- c("Preschool", "1st-4th", "5th-6th", "7th-8th", "9th", "10th", "11th", "12th", "HS-grad", "Some-college", "Assoc-voc", "Assoc-acdm", "Bachelors", "Masters", "Prof-school", "Doctorate")
d$Education <- factor(d$Education, levels = education_levels, ordered = T)

set.seed(42)
d <- d[complete.cases(d), ]

for (n_samples in c(400, 800)){
	df <- d[sample(nrow(d), n_samples), ]

	# Using pcalg
	var_types <- list(
			Age = "cont",
			Workclass = "cat",
			Education = "ord",
			MaritalStatus = "cat",
			Occupation = "cat",
			Relationship = "cat",
			Race = "cat",
			Sex = "cat",
			HoursPerWeek = "cont",
			Income = "cat",
			NativeCountry = "cat"
			)

	# Using our mixed data test
	suffStat_q3 <- list(dm = df, adaptDF = F, test = "rf_q3", var_types = var_types)
	cpdag.q3 <- pcalg::pc(
			suffStat_q3,
			indepTest = sl.test,
			alpha = 0.05,
			labels = colnames(df),
			u2pd = "relaxed",
			skel.method = "stable.fast",
			numCores = 8,
			verbose = F
			)
	
	# Using likelihood ratio test
	suffStat_mxm <- list(dm = df, adaptDF = F, test = "mxm", var_types = var_types)
	cpdag.mxm <- pcalg::pc(
			suffStat_mxm,
			indepTest = sl.test,
			alpha = 0.05,
			labels = colnames(df),
			u2pd = "relaxed",
			skel.method = "stable.fast",
			numCores = 8,
			verbose = F
			)
	
	# Using MI
	suffStat_mi <- list(dm = df, adaptDF = F, test = "mi_cg", var_types = var_types)
	cpdag.mi <- pcalg::pc(
			suffStat_mi,
			indepTest = sl.test,
			alpha = 0.05,
			labels = colnames(df),
			u2pd = "relaxed",
			skel.method = "stable.fast",
			numCores = 8,
			verbose = F
			)
	
	# Using hill climb search BIC score
	cpdag.bic <- bnlearn::cpdag(bnlearn::hc(df, score='bic-cg'))

	dir.create('data')

	adj_mat_q3 <- as(cpdag.q3, "amat")
	write.table(adj_mat_q3, file=paste0('data/q3_', n_samples, '.txt'), row.names=T, col.names=T)

	adj_mat_mxm <- as(cpdag.mxm, "amat")
	write.table(adj_mat_mxm, file=paste0('data/mxm_', n_samples, '.txt'), row.names=T, col.names=T)

	adj_mat_mi <- as(cpdag.mi, "amat")
	write.table(adj_mat_mi, file=paste0('data/mi_', n_samples, '.txt'), row.names=T, col.names=T)

	adj_mat_bic <- bnlearn::amat(cpdag.bic)
	write.table(adj_mat_bic, file=paste0('data/bic_', n_samples, '.txt'), row.names=T, col.names=T)
}
