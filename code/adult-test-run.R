ee <- new.env()


d <- read.csv("adult.data",header=FALSE,
		na.strings="?",sep=",",strip.white=TRUE,stringsAsFactors=TRUE)
	colnames(d) <- c("Age","Workclass","Fnlwgt","Education","EducationNum",
        	"MaritalStatus","Occupation","Relationship","Race","Sex","CapitalGain",
	        "CapitalLoss","HoursPerWeek","NativeCountry","Income")

	# Tell R about which variables are ordinal
	d$Education <- as.integer(d$EducationNum)
	d$Income <- ordered(d$Income)

	d <- d[,-which(colnames(d)=="EducationNum")] 

	d$NativeCountry <- as.character( d$NativeCountry )
	d$NativeCountry[!d$NativeCountry %in% c("United-States","Mexico")] <- "Other"
	d$NativeCountry <- factor( d$NativeCountry )

	# Drop rows containing missing data
d <- d[complete.cases(d),]

ee$d <- d

pr("adult-test.R", envir=ee) %>% pr_run(port=8000)
