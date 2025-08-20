install_dependencies <- function() {
  # Set CRAN mirror to avoid the error
  if (is.null(getOption("repos")) || getOption("repos") == "@CRAN@") {
    options(repos = c(CRAN = "https://cloud.r-project.org/"))
  }
  
  # Core data manipulation and visualization
  required_packages <- c(
    # Data manipulation and analysis
    "tidyverse",
    "readr", 
    "dplyr",
    "ggplot2",
    "quarto",
    
    # Statistical analysis and survey methods
    "survey",
    "margins",
    "emmeans",
    "car",
    
    # Propensity score and causal inference methods
    "MatchIt",
    "WeightIt", 
    "twang",
    "CBPS",
    "ebal",
    "BART",
    
    # Machine learning for PS
    "gbm",
    "randomForest",
    "glmnet",
    "rpart",
    "gam",
    
    # Multiple imputation
    "mice",
    "VIM",
    "Hmisc",
    "naniar",
    
    # Table formatting and output
    "knitr",
    "kableExtra",
    "gt",
    "flextable",
    "gtsummary",
    
    # Balance checking and diagnostics
    "cobalt",
    "jtools",
    "gridExtra",
    "gee",
    
    # Sensitivity analysis
    "EValue",
    
    # Utility packages
    "scales",
    "purrr",
    "broom",
    "here",
    "fs"
  )
  
  # Check which packages are not installed
  missing_packages <- required_packages[!sapply(required_packages, requireNamespace, quietly = TRUE)]
  
  if (length(missing_packages) > 0) {
    cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
    install.packages(missing_packages, dependencies = TRUE)
  } else {
    cat("All required packages are already installed.\n")
  }
  
  # Load all packages to check for any issues
  cat("Loading packages to verify installation...\n")
  for (pkg in required_packages) {
    success <- require(pkg, character.only = TRUE, quietly = TRUE)
    if (!success) {
      warning(paste("Failed to load package:", pkg))
    }
  }
  
  cat("Dependency installation and verification complete.\n")
}

# Function to load required libraries for analysis
load_required_libraries <- function() {
  # Set CRAN mirror if not already set
  if (is.null(getOption("repos")) || getOption("repos") == "@CRAN@") {
    options(repos = c(CRAN = "https://cloud.r-project.org/"))
  }
  
  required_packages <- c(
    "tidyverse",
    "survey",
    "margins",
    "emmeans",
    "car",
    "MatchIt",
    "WeightIt",
    "twang",
    "CBPS",
    "ebal",
    "BART",
    "gbm",
    "randomForest",
    "glmnet",
    "rpart",
    "gam",
    "mice",
    "VIM",
    "Hmisc",
    "naniar",
    "knitr",
    "scales",
    "broom",
    "cobalt",
    "gtsummary",
    "jtools",
    "gridExtra",
    "gee",
    "flextable",
    "gt",
    "kableExtra",
    "EValue"
  )
  
  for (pkg in required_packages) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      install.packages(pkg, dependencies = TRUE, quiet = TRUE)
    }
    library(pkg, character.only = TRUE, quietly = TRUE)
  }
}

# Setup function for quarto documents
setup_analysis <- function(seed = 2025) {
  set.seed(seed)
  
  source("settings.R")
  source("functions.R")
  source("dependencies.R")
  
  load_required_libraries()
}