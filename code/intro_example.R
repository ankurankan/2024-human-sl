library(tidyverse)
library(bnlearn)

d <- read.csv('adult_unproc.csv')

# Preprocess the data
d$Fnlwgt <- NULL
d$EducationNum <- NULL
d$CapitalGain <- NULL
d$CaptialLoss <- NULL

d$Workclass <- factor(d$Workclass, ordered=F)
d$MaritalStatus <- factor(d$MaritalStatus, ordered=F)
d$Occupation <- factor(d$Occupation, ordered=F)
d$Relationship <- factor(d$Relationship, ordered=F)
d$Race <- factor(d$Race, ordered=F)
d$Sex <- factor(d$Sex, ordered=F)
d$NativeCountry <- factor(d$NativeCountry, ordered=F)
d$Income <- factor(d$Income, ordered=F)

d$Age <- as.double(d$Age)
d$HoursPerWeek <- as.double(d$HoursPerWeek)

education_levels = c( "Preschool", "1st-4th", "5th-6th", "7th-8th", "9th", "10th", "11th","12th", "HS-grad", "Some-college", "Assoc-voc", "Assoc-acdm", "Bachelors", "Masters", "Prof-school", "Doctorate" )
d$Education <- factor(d$Education, levels=education_levels, ordered=T)

dag <- pc.stable(d)
