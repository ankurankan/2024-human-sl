source("test_dag_marg.R")

#* Computes the marginal correlations between each combination of variables.
#* @post /marginals
function(dag, data){
	dag <- dagitty::dag(dag)
	compute_effects(dag, data)
}

#* @post /upload
upload <- function(req, res){
  cat("---- New Request ----\n")
  # the path where you want to write the uploaded files
  file_path <- "."
  # strip the filename out of the postBody
  file_name <- gsub('\"', "", substr(req$postBody[2], 55, 1000))
  # need the length of the postBody so we know how much to write out
  file_length <- length(req$postBody)-1
  # first five lines of the post body contain metadata so are ignored
  file_content <- req$postBody[5:file_length]
  # build the path of the file to write
  file_to_write <- paste0(file_path, file_name)
  # write file out with no other checks at this time
  browser()
  write(file_content, file = file_to_write)
  # print logging info to console
  cat("File", file_to_write, "uploaded\n")
  # return file path &name to user
  return(file_to_write)
}

#* Computes the conditional correlations between each combination of variables.
