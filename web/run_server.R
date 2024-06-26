library(dplyr)
library(plumber)

# ee <- new.env()
# 
# 
# d <- read.csv("adult_proc.csv",header=T,
# 		na.strings="?",sep=",",strip.white=TRUE,stringsAsFactors=TRUE)
# d <- d[, 2:ncol(d)]
# 	colnames(d) <- c("Age","Workclass", "Education",
#         	"MaritalStatus","Occupation","Relationship","Race","Sex",
# 	        "HoursPerWeek","NativeCountry","Income")
# 
# 	# Tell R about which variables are ordinal
# 	# d$Education <- as.integer(d$EducationNum)
# 	d$Income <- ordered(d$Income)
# 
# 	# d <- d[,-which(colnames(d)=="EducationNum")] 
# 
# 	d$NativeCountry <- as.character( d$NativeCountry )
# 	d$NativeCountry[!d$NativeCountry %in% c("United-States","Mexico")] <- "Other"
# 	d$NativeCountry <- factor( d$NativeCountry )
# 
# 	# Drop rows containing missing data
# d <- d[complete.cases(d),]
# 
# ee$d <- d

pr("api.R") %>% pr_run(port=8000)
