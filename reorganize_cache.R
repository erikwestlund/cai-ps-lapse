# Script to reorganize PS cache directories for consistency
# Run this ONCE to consolidate all PS results into a single cache structure

# Load settings to get paths
source("settings.R")

# Define directories
base_dir <- reanalysis_data_dir
old_ps_cache <- file.path(base_dir, "ps_cache")
ps_alt_methods <- file.path(base_dir, "ps_alternative_methods")
ps_cache_twang <- file.path(base_dir, "ps_cache_twang")
new_ps_cache <- file.path(base_dir, "ps_cache")
outcome_cache <- file.path(base_dir, "outcome_cache")

cat("================================================================================\n")
cat("CACHE REORGANIZATION SCRIPT\n")
cat("================================================================================\n")
cat("Base directory:", base_dir, "\n\n")

# Step 1: Check current state
cat("Checking current directories...\n")
if (dir.exists(old_ps_cache)) {
  cat("  ✓ Found ps_cache with", length(list.files(old_ps_cache)), "files\n")
} else {
  cat("  ✗ ps_cache not found\n")
}

if (dir.exists(ps_alt_methods)) {
  cat("  ✓ Found ps_alternative_methods with", length(list.files(ps_alt_methods)), "files\n")
} else {
  cat("  ✗ ps_alternative_methods not found\n")
}

# Check for command line arguments or environment variable
args <- commandArgs(trailingOnly = TRUE)
cat("Debug: args =", paste(args, collapse=", "), "\n")
cat("Debug: length(args) =", length(args), "\n")
cat("Debug: REORGANIZE_CONFIRM =", Sys.getenv("REORGANIZE_CONFIRM"), "\n")
cat("Debug: interactive() =", interactive(), "\n")

auto_confirm <- length(args) > 0 && args[1] == "--yes"
env_confirm <- Sys.getenv("REORGANIZE_CONFIRM") == "yes"

cat("Debug: auto_confirm =", auto_confirm, "\n")
cat("Debug: env_confirm =", env_confirm, "\n")

if (!auto_confirm && !env_confirm && interactive()) {
  # Only ask for confirmation in interactive mode
  cat("\nThis script will:\n")
  cat("1. Rename ps_cache → ps_cache_twang\n")
  cat("2. Rename ps_alternative_methods → ps_cache\n")
  cat("3. Move twang files from ps_cache_twang to ps_cache\n")
  cat("4. Rename twang files to consistent pattern\n")
  cat("5. Create outcome_cache directory\n")
  cat("\nProceed? (y/n): ")
  response <- readline()
  
  if (tolower(response) != "y") {
    cat("Cancelled.\n")
    stop("User cancelled operation")
  }
} else if (auto_confirm || env_confirm) {
  cat("\nAuto-confirmed. Proceeding with reorganization...\n")
} else {
  cat("\nRunning in non-interactive mode without confirmation.\n")
  cat("To run without prompts, use one of:\n")
  cat("  Rscript reorganize_cache.R --yes\n")
  cat("  REORGANIZE_CONFIRM=yes Rscript reorganize_cache.R\n")
  cat("  Or run in R console: source('reorganize_cache.R')\n")
  stop("Please confirm the operation using one of the methods above")
}

# Step 2: Rename ps_cache to ps_cache_twang
if (dir.exists(old_ps_cache) && !dir.exists(ps_cache_twang)) {
  cat("\nStep 1: Renaming ps_cache to ps_cache_twang...\n")
  file.rename(old_ps_cache, ps_cache_twang)
  cat("  ✓ Renamed successfully\n")
} else if (dir.exists(ps_cache_twang)) {
  cat("\nStep 1: ps_cache_twang already exists, skipping rename\n")
}

# Step 3: Rename ps_alternative_methods to ps_cache
if (dir.exists(ps_alt_methods) && !dir.exists(new_ps_cache)) {
  cat("\nStep 2: Renaming ps_alternative_methods to ps_cache...\n")
  file.rename(ps_alt_methods, new_ps_cache)
  cat("  ✓ Renamed successfully\n")
} else if (dir.exists(new_ps_cache)) {
  cat("\nStep 2: ps_cache already exists (from ps_alternative_methods)\n")
}

# Step 4: Move and rename twang files
if (dir.exists(ps_cache_twang)) {
  cat("\nStep 3: Moving twang files from ps_cache_twang to ps_cache...\n")
  
  # List all twang files
  twang_files <- list.files(ps_cache_twang, pattern = "twang_imp.*\\.rds", full.names = TRUE)
  
  if (length(twang_files) > 0) {
    cat("  Found", length(twang_files), "twang files to move\n")
    
    moved_count <- 0
    for (file in twang_files) {
      # Extract imputation number
      filename <- basename(file)
      
      # Handle both twang_imp_1.rds and twang_imp1.rds patterns
      if (grepl("twang_imp_\\d+\\.rds", filename)) {
        # Has underscore - extract number and rename
        imp_num <- gsub("twang_imp_(\\d+)\\.rds", "\\1", filename)
        new_name <- paste0("twang_gbm_imp", imp_num, ".rds")
      } else if (grepl("twang_imp\\d+\\.rds", filename)) {
        # No underscore - just rename to twang_gbm
        imp_num <- gsub("twang_imp(\\d+)\\.rds", "\\1", filename)
        new_name <- paste0("twang_gbm_imp", imp_num, ".rds")
      } else {
        cat("  Warning: Unexpected filename pattern:", filename, "\n")
        next
      }
      
      new_path <- file.path(new_ps_cache, new_name)
      
      # Copy file (safer than move in case something goes wrong)
      if (file.copy(file, new_path, overwrite = FALSE)) {
        moved_count <- moved_count + 1
        if (moved_count %% 10 == 0) {
          cat("    Moved", moved_count, "files...\n")
        }
      }
    }
    
    cat("  ✓ Moved and renamed", moved_count, "twang files\n")
    
    # Optionally delete originals after successful copy
    if (interactive()) {
      cat("\nDelete original twang files from ps_cache_twang? (y/n): ")
      del_response <- readline()
      if (tolower(del_response) == "y") {
        unlink(twang_files)
        cat("  ✓ Original files deleted\n")
      } else {
        cat("  Original files kept in ps_cache_twang\n")
      }
    } else {
      cat("  Original files kept in ps_cache_twang (run interactively to delete)\n")
    }
  } else {
    cat("  No twang files found to move\n")
  }
}

# Step 5: Create outcome_cache directory
if (!dir.exists(outcome_cache)) {
  cat("\nStep 4: Creating outcome_cache directory...\n")
  dir.create(outcome_cache, recursive = TRUE)
  cat("  ✓ Created outcome_cache\n")
} else {
  cat("\nStep 4: outcome_cache already exists\n")
}

# Step 6: Verify final structure
cat("\n================================================================================\n")
cat("VERIFICATION\n")
cat("================================================================================\n")

if (dir.exists(new_ps_cache)) {
  all_files <- list.files(new_ps_cache, pattern = "\\.rds$")
  methods <- unique(gsub("_imp\\d+\\.rds", "", all_files))
  
  cat("\nps_cache contents:\n")
  for (method in sort(methods)) {
    method_files <- list.files(new_ps_cache, pattern = paste0("^", method, "_imp\\d+\\.rds$"))
    cat("  ", method, ":", length(method_files), "files\n")
  }
}

cat("\n✓ Cache reorganization complete!\n")
cat("\nNext steps:\n")
cat("1. Run this script on your remote system where the data is located\n")
cat("2. Update code files to use the new cache structure\n")
cat("3. Test that all methods are detected correctly\n")