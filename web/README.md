### 1. Required R packages to run the app:
1. plumber
2. dplyr
3. dagitty

### 2. Steps to run the app:

1. Run the backend API server using: `Rscript run_server.R`
2. Open `index.html` in the browser.

The app should be ready to use.

### 3. Example Datasets
There are two example datasets in this repo:
1. `mediator.csv` : Should work with the app without any changes.
2. `adult_mixed_proc.csv` : We will first need to replace the current `preprocess_dataset` function in `api.R` with the commented out version. And rerun the backend server.

### 4. Notes:

1. By default, the current implementation only works for continuous variables.
2. For using it on datasets with a mix of continuous and categorical variables, a `preprocess_dataset` function is required in `api.R`.
