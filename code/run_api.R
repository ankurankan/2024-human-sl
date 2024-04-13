# Runs the web server to accept requests. Available at: http://localhost:8000

library(plumber)

pr("api.R") %>% pr_run(port=8000)
