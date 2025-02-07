library(jsonlite)

preprocess_adult <- function(d){
	d$X <- NULL
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
	d <- d[complete.cases(d), ]
	d <- d[1:1000, ]
	
	return(d)
}

#* @filter cors
cors <- function(res) {
    res$setHeader("Access-Control-Allow-Origin", "*") # Or whatever
    plumber::forward()
}

datasets <- list()

# Define the file upload endpoint
#* @post /upload
#* @param file:file
#* @response 200
function(req, res, file) {
  if (is.null(file)) {
    return(list(status = "failure", message = "No file uploaded"))
  }
 
  filename = names(file)
  # Read the uploaded CSV file
  csv_data <- read.csv(text=file[[filename]])
  dataset_name <- paste0(sample(c(LETTERS, tolower(LETTERS)), 20, T), collapse='')
  datasets[[dataset_name]] <<- csv_data

  res$setCookie("dataset_name", dataset_name, path = "/", secure = TRUE, http = TRUE)

  res$body <- toJSON(list(var_names=colnames(csv_data)))
  
  # Print the first few rows of the CSV file (for demonstration)
  # print(head(csv_data))
  
  # Return a success message
  # return(list(status = "success", message = "File uploaded successfully"))
  # return (dataset)
  return(res$toResponse())
}

#* @param dag
#* @param threshold
#* @param pval
#* @get /getassoc
#* @response 200
run_citests <- function( req, res, dag, threshold, pval ){
		g <- dagitty::dagitty(dag)
		r <- c()
		nn <- names(g)
		dataset <- preprocess_adult(datasets[[1]])
		for( n1i in seq(1,length(nn)-1,by=1) ){
			n1 <- nn[n1i]
			p1 <- dagitty::parents(g, n1)
			for( n2i in seq(n1i+1, length(nn), by=1) ){
				n2 <- nn[n2i]
				p2 <- dagitty::parents( g, n2 )
				if( n2 %in% p1 ){		
					otherparents <- setdiff( p1, n2 )
					tst <- dagitty::ciTest( X=n1, Y=n2, Z=otherparents, dataset,
						type="cis.pillai" )
					u <- n2; v <- n1 ; a <- "->"
				} else if( n1 %in% p2 ) {
					otherparents <- setdiff( p1, n2 )
					tst <- dagitty::ciTest( X=n1, Y=n2, Z=otherparents, dataset,
						type="cis.pillai" )
					u <- n1 ; v <- n2 ; a <- "->"
				} else {
					tst <- dagitty::ciTest( X=n1, Y=n2, Z=union( p1, p2 ), dataset,
						type="cis.pillai" )
					u <- n1 ; v <- n2 ; a <- "--"
				}
				r <- rbind( r, data.frame(list(X=u,A=a,Y=v,
					cor=tst[,"estimate"],p=tst[,"p.value"])) )

			}
		}
		r <- r %>% filter(abs(cor) > as.double(threshold)) %>% filter(p <= as.double(pval))
		return (r)
}

#* @param dag
#* @get /rmsea
rmsea <- function( dag ){
	g <- dagitty::dagitty(dag)
	pvalues <- c()
	nodes <- names(g)
	dataset <- preprocess_adult(datasets[[1]])
	for (i in 1:(length(nodes)-1)){
		for (j in (i+1):length(nodes)){
			pa_i <- dagitty::parents(g, nodes[i])
			pa_j <- dagitty::parents(g, nodes[j])
	
			if (!(nodes[i] %in% pa_j) & !(nodes[j] %in% pa_i)){
				tst <- dagitty::ciTest(X=nodes[i], Y=nodes[j], Z=union(pa_i, pa_j), dataset, type="cis.pillai")
				pvalues <- c(pvalues, tst[,"p.value"])
			}
		}
	}
	fisherc <- -2 * sum(log(pvalues))
	# return (pchisq(fisherc, 2*length(pvalues), lower.tail=F))
	return(sqrt( (fisherc**2 - 2*length(pvalues)) / (2*length(pvalues)) / nrow(dataset) ))
}
