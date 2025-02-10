library(dagitty)
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

d$AgeStr <- rep("<21", nrow(d))
d$AgeStr[d$Age >= 21 & d$Age <= 30] <- "21-30"
d$AgeStr[d$Age >= 31 & d$Age <= 40] <- "31-40"
d$AgeStr[d$Age >= 41 & d$Age <= 50] <- "41-50"
d$AgeStr[d$Age >= 51 & d$Age <= 60] <- "51-60"
d$AgeStr[d$Age >= 61 & d$Age <= 70] <- "61-70"
d$AgeStr[d$Age > 70] <- ">70"
d$Age <- as.factor(d$AgeStr)
d$AgeStr <- NULL

d$HoursPerWeekStr <- as.character(d$HoursPerWeek)
d$HoursPerWeekStr[d$HoursPerWeek <= 20] <- "<=20" 
d$HoursPerWeekStr[d$HoursPerWeek > 40] <- ">40" 
d$HoursPerWeekStr[d$HoursPerWeek >= 20 & d$HoursPerWeek < 30] <- "21-30"
d$HoursPerWeekStr[d$HoursPerWeek >= 30 & d$HoursPerWeek <= 40] <- "31-40" 
d$HoursPerWeek <- as.factor(d$HoursPerWeekStr)
d$HoursPerWeekStr <- NULL


set.seed(42)
d <- d[complete.cases(d), ]
d <- d[sample(nrow(d), 800), ]

dag1 <- dagitty("dag{MaritalStatus -> Relationship
	       	    Relationship -> Sex 
		    HoursPerWeek -> Sex
		    Relationship -> HoursPerWeek
		    Relationship -> Age
		    HoursPerWeek -> Education
		    Age -> MaritalStatus
		    Occupation -> Relationship
		    Occupation -> HoursPerWeek
		    Occupation -> Sex
		    Income -> Age
		    Income -> Occupation
		    Occupation -> Workclass
		    Income -> Education
		    Occupation -> Education}")

dag2 <- dagitty("dag{MaritalStatus -> Relationship
		     Sex -> Relationship
		     Sex -> Occupation
		     Occupation -> Education
		     Workclass -> Education
		     Workclass -> Income
		     Occupation -> Workclass
		     }")

run_ci <- function(dag, d){
	results <- c()
	cis = as.list(impliedConditionalIndependencies(dag))
	for (i in 1:length(cis)){
		if (length(cis[[i]]$Z) == 0){
			results <- c(results, NULL)
		}
		else{
			results <- c(results, ci.test(cis[[i]]$X, cis[[i]]$Y, cis[[i]]$Z, d, 'glm_q3'))
		}
	}
	return(list(cis=cis, results=results))
}

tests <- run_ci(dag=dag1, d=d)
