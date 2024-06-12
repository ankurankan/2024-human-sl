#* @filter cors
cors <- function(res) {
    res$setHeader("Access-Control-Allow-Origin", "*") # Or whatever
    plumber::forward()
}

# Define the file upload endpoint
#* @post /upload
#* @param file:file
#* @response 200
function(file) {
  if (is.null(file)) {
    return(list(status = "failure", message = "No file uploaded"))
  }
 
  filename = names(file)
  # Read the uploaded CSV file
  csv_data <- read.csv(text=file[[filename]])
  dataset <<- csv_data
  
  # Print the first few rows of the CSV file (for demonstration)
  print(head(csv_data))
  
  # Return a success message
  # return(list(status = "success", message = "File uploaded successfully"))
  # return (dataset)
  return(colnames(csv_data))
}

#* @param dag
#* @param threshold
#* @param pval
#* @get /simpletest
run_citests <- function( dag, threshold, pval){
		# print(head(dataset))
		# d <- get("d")
		g <- dagitty::dagitty(dag)
		r <- c()
		nn <- names(g)
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
		browser()
		return (r)
}

