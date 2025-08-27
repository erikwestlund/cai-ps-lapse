# Quick script to extract weights from saved ps_results_twang.rds
# This avoids having to re-run all of Step 3

source("dependencies.R")
library(twang)

# Load the saved results
ps_output <- readRDS(file.path(reanalysis_data_dir, "ps_results_twang.rds"))

# Extract weights from all twang models
twang_weights <- list()
for (i in 1:ps_output$n_imputations) {
  twang_weights[[i]] <- get.weights(ps_output$twang_results[[i]], stop.method = "es.mean")
}

# Save the weights
saveRDS(twang_weights, file.path(reanalysis_data_dir, "twang_weights.rds"))

cat("Successfully extracted and saved weights to:", 
    file.path(reanalysis_data_dir, "twang_weights.rds"), "\n")
cat("Number of weight sets:", length(twang_weights), "\n")
cat("Length of first weight set:", length(twang_weights[[1]]), "\n")