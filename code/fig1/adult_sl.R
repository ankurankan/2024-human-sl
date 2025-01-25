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

d <- d[complete.cases(d), ]
d <- d[1:500, ]

# Using bnlearn
dag <- bnlearn::pc.stable(d)

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

suffStat_q3 <- list(dm = d, adaptDF = F, test = "rf_q3", var_types = var_types)
pc.cpdag.q3 <- pcalg::pc(
		suffStat_q3,
		indepTest = sl.test,
		alpha = 0.05,
		labels = colnames(d),
		u2pd = "relaxed",
		skel.method = "stable.fast",
		numCores = 8,
		verbose = F
		)

suffStat_mxm <- list(dm = d, adaptDF = F, test = "mxm", var_types = var_types)
pc.cpdag.mxm <- pcalg::pc(
		suffStat_mxm,
		indepTest = sl.test,
		alpha = 0.05,
		labels = colnames(d),
		u2pd = "relaxed",
		skel.method = "stable.fast",
		numCores = 8,
		verbose = F
		)

suffStat_mi <- list(dm = d, adaptDF = F, test = "mi_cg", var_types = var_types)
pc.cpdag.mi <- pcalg::pc(
		suffStat_mi,
		indepTest = sl.test,
		alpha = 0.05,
		labels = colnames(d),
		u2pd = "relaxed",
		skel.method = "stable.fast",
		numCores = 8,
		verbose = F
		)

hc.cpdag <- bnlearn::cpdag(bnlearn::hc(d, score='bic-cg'))

adj_mat_q3 <- pcalg::dag2cpdag(as(pc.skel.q3, "amat")) # Use this to get the final model.
adj_mat_mxm <- pcalg::dag2cpdag(as(pc.skel.mxm, "amat")) # Use this to get the final model.
adj_mat_mi <- pcalg::dag2cpdag(as(pc.skel.mi, "amat")) # Use this to get the final model.

