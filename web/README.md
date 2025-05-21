Steps to run the app:

1. Run the backend API server using:
```bash
Rscript run_server.R
```

2. Open `index.html` in the browser.

The app should be ready to use.

Notes:

1. The current implementation by default only works for continuous variables.
2. To use it on mixture of datasets, a preprocessing function called `preprocess_dataset` needs to be added to `api.R`. An example function exists for the adult income dataset.
