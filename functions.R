# ============================================================================
# Logging System
# ============================================================================

# Initialize logging for a specific analysis step
init_log <- function(analysis_name, log_dir = "logs") {
  # Close any existing log connections first
  try({
    # Close all sink connections if any exist
    while (sink.number() > 0) {
      sink()
    }
    # Close all open connections except stdin/stdout/stderr
    cons <- showConnections()
    if (nrow(cons) > 0) {
      for (i in 1:nrow(cons)) {
        if (as.numeric(rownames(cons)[i]) > 2) {  # Don't close stdin(0), stdout(1), stderr(2)
          try(close(getConnection(as.numeric(rownames(cons)[i]))), silent = TRUE)
        }
      }
    }
  }, silent = TRUE)
  
  # Create log directory if it doesn't exist
  if (!dir.exists(log_dir)) {
    dir.create(log_dir, recursive = TRUE)
  }
  
  # Create timestamped log filename
  timestamp <- format(Sys.time(), "%Y%m%d_%H%M%S")
  log_file <- file.path(log_dir, paste0(analysis_name, "_", timestamp, ".log"))
  
  # Delete any existing file with same name (shouldn't happen with timestamp, but just in case)
  if (file.exists(log_file)) {
    Sys.sleep(0.1)  # Small delay to ensure file handle is released
    try(unlink(log_file, force = TRUE), silent = TRUE)
    Sys.sleep(0.1)  # Another small delay
  }
  
  # Initialize log with header
  header_msg <- paste0("================================================================================\n",
                      "Analysis: ", analysis_name, "\n",
                      "Started: ", Sys.time(), "\n",
                      "================================================================================\n\n")
  
  # Use writeLines instead of cat for more reliable file writing
  tryCatch({
    writeLines(header_msg, log_file)
  }, error = function(e) {
    # If writeLines fails, try cat
    cat(header_msg, file = log_file, append = FALSE)
  })
  
  # Also output to console
  cat(header_msg)
  
  # Store in options for easy access
  options(current_log_file = log_file)
  options(log_start_time = Sys.time())
  
  # Return log file path
  return(log_file)
}

# Log a message with timestamp
log_message <- function(msg, level = "INFO") {
  log_file <- getOption("current_log_file")
  
  if (!is.null(log_file)) {
    timestamp <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    formatted_msg <- paste0("[", timestamp, "] [", level, "] ", msg)
    
    # Try multiple approaches to write to log file
    write_success <- FALSE
    
    # Approach 1: Try write() function
    tryCatch({
      write(formatted_msg, file = log_file, append = TRUE)
      write_success <- TRUE
    }, error = function(e) {
      write_success <- FALSE
    })
    
    # Approach 2: If write() failed, try cat() with error handling
    if (!write_success) {
      tryCatch({
        cat(formatted_msg, "\n", file = log_file, append = TRUE, sep = "")
        write_success <- TRUE
      }, error = function(e) {
        write_success <- FALSE
      })
    }
    
    # Approach 3: If still failing, try opening a connection
    if (!write_success) {
      tryCatch({
        con <- file(log_file, open = "a")
        writeLines(formatted_msg, con)
        close(con)
        write_success <- TRUE
      }, error = function(e) {
        write_success <- FALSE
      })
    }
    
    # Always output to console
    cat(formatted_msg, "\n")
    
    # If writing failed completely, warn user
    if (!write_success) {
      warning(paste("Could not write to log file:", log_file))
    }
    
    # Return the message invisibly
    invisible(formatted_msg)
  } else {
    # If no log file, just output to console
    cat(paste0("[", level, "] ", msg, "\n"))
    warning("No log file initialized. Call init_log() first.")
  }
}

# Log progress with percentage and time estimates
log_progress <- function(current, total, item_name = "item") {
  log_file <- getOption("current_log_file")
  start_time <- getOption("log_start_time")
  
  if (!is.null(log_file) && !is.null(start_time)) {
    pct <- round(100 * current / total, 1)
    elapsed <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    
    if (current > 0) {
      rate <- elapsed / current
      remaining <- rate * (total - current)
      
      msg <- sprintf("Progress: %s %d/%d (%.1f%%) - Elapsed: %.1f min - Remaining: ~%.1f min",
                     item_name, current, total, pct, elapsed, remaining)
    } else {
      msg <- sprintf("Starting: %d %ss to process", total, item_name)
    }
    
    log_message(msg, level = "PROGRESS")
    
    # Return for potential display
    invisible(msg)
  }
}

# Log an error with traceback
log_error <- function(error_msg, include_traceback = TRUE) {
  log_message(error_msg, level = "ERROR")
  
  if (include_traceback) {
    tb <- traceback(max.lines = 5)
    if (!is.null(tb)) {
      log_message(paste("Traceback:", paste(tb, collapse = "\n  ")), level = "ERROR")
    }
  }
}

# Finalize log with summary
finalize_log <- function(success = TRUE) {
  log_file <- getOption("current_log_file")
  start_time <- getOption("log_start_time")
  
  if (!is.null(log_file) && !is.null(start_time)) {
    total_time <- as.numeric(difftime(Sys.time(), start_time, units = "mins"))
    
    status <- ifelse(success, "COMPLETED SUCCESSFULLY", "FAILED")
    
    summary_msg <- paste0("\n================================================================================\n",
                         "Analysis ", status, "\n",
                         "Ended: ", Sys.time(), "\n",
                         "Total time: ", round(total_time, 2), " minutes\n",
                         "Log file: ", log_file, "\n",
                         "================================================================================\n")
    
    # Try to write final message
    tryCatch({
      cat(summary_msg, file = log_file, append = TRUE)
    }, error = function(e) {
      # If can't write, at least show in console
      warning("Could not write final message to log file")
    })
    
    # Also output to console
    cat(summary_msg)
    
    # Clear options BEFORE closing connections
    options(current_log_file = NULL)
    options(log_start_time = NULL)
    
    # Clean up any open connections
    try({
      # Close all sink connections if any exist
      while (sink.number() > 0) {
        sink()
      }
      # Garbage collection to ensure file handles are released
      gc(verbose = FALSE)
    }, silent = TRUE)
    
    # Small delay to ensure file is released
    Sys.sleep(0.1)
    
    # Return log file path for reference
    return(log_file)
  }
}

# Create a latest symlink for easy access
create_latest_log_link <- function(log_file, analysis_name, log_dir = "logs") {
  latest_link <- file.path(log_dir, paste0(analysis_name, "_latest.log"))
  
  # Remove existing symlink if it exists (force removal to avoid permission errors)
  if (file.exists(latest_link) || file.symlink(latest_link)) {
    try(unlink(latest_link, force = TRUE), silent = TRUE)
  }
  
  # Create new symlink (on Unix-like systems)
  if (.Platform$OS.type == "unix") {
    try({
      system(paste0("ln -sf ", basename(log_file), " ", latest_link))
    }, silent = TRUE)
  }
}

# ============================================================================
# Original Functions
# ============================================================================

# Create person_dr from No_DR/NPDR/PDR columns
create_person_dr <- function(data) {
  data |>
    mutate(
      person_dr = case_when(
        No_DR == TRUE ~ 0,
        NPDR == "Mild" | NPDR == "Moderate" | NPDR == "Severe" ~ 1,
        PDR == "Present" ~ 2,
        TRUE ~ NA_real_  # Handle missing/undefined cases
      )
    )
}

# Create outcome variables from logMAR
create_outcome_variables <- function(data) {
  data |>
    mutate(
      outcome_va_vi = outcome_VA_logMAR >= 0.3,
      outcome_va_vi_binary = if_else(outcome_va_vi, 1, 0),
      outcome_va_blind = outcome_VA_logMAR >= 1,
      outcome_va_blind_binary = if_else(outcome_va_blind, 1, 0)
    )
}

# Create age categories from continuous age
create_age_cat <- function(data) {
  data |>
    mutate(
      age_cat = cut(age, 
                   breaks = c(0, 20, 45, 65, Inf), 
                   labels = c("0-20", "21-45", "46-65", "65+"))
    )
}

# Create treatment type from individual treatment flags
# NOTE: This uses hierarchical assignment - patients with multiple treatments
# will be classified by the first matching condition (anti_VEGF > PRP > other)
create_treatment_type <- function(data) {
  data |>
    mutate(
      treatment_type = case_when(
        anti_VEGF == 1 ~ "anti_VEGF",
        PRP_flag == 1 ~ "PRP",
        other_inject == 1 | focal_laser_flag == 1 ~ "other_treatment",
        TRUE ~ "no_treatment"
      ),
      treatment_type = factor(treatment_type, 
                             levels = c("no_treatment", "anti_VEGF", "PRP", "other_treatment")),
      # Binary indicator for any treatment vs none - as factor for interaction models
      any_treatment = factor(
        ifelse(anti_VEGF == 1 | PRP_flag == 1 | other_inject == 1 | focal_laser_flag == 1,
               "Any_Treatment", "No_Treatment"),
        levels = c("No_Treatment", "Any_Treatment")
      )
    )
}

# Create DR severity variables (all versions)
create_dr_severity_variables <- function(data) {
  # Check if we have the source columns or just person_dr
  if (all(c("No_DR", "NPDR", "PDR") %in% names(data))) {
    # We have source columns, create person_dr_severity from them
    data |>
      mutate(
        # Detailed severity (0-4 scale)
        person_dr_severity = case_when(
          No_DR == TRUE ~ 0,
          NPDR == "Mild" ~ 1,
          NPDR == "Moderate" ~ 2,
          NPDR == "Severe" ~ 3,
          PDR == "Present" ~ 4,
          TRUE ~ NA_real_
        ),
        # Factor version for interaction models
        dr_severity = factor(person_dr,
                            levels = c(0, 1, 2),
                            labels = c("No_DR", "NPDR", "PDR"))
      )
  } else {
    # We only have person_dr (from imputation), create simplified versions
    # Convert back from factor if needed
    if (is.factor(data$person_dr)) {
      # If person_dr is an ordered factor with labels from imputation
      # Convert labels back to numeric (0, 1, 2)
      person_dr_num <- as.numeric(data$person_dr) - 1  # Factor levels are 1-indexed
    } else {
      person_dr_num <- data$person_dr
    }
    
    data |>
      mutate(
        person_dr = person_dr_num,
        # For imputed data, we can't distinguish NPDR subtypes
        # So person_dr_severity will be same as person_dr
        person_dr_severity = person_dr_num,
        # Factor version for interaction models
        dr_severity = factor(person_dr_num,
                            levels = c(0, 1, 2),
                            labels = c("No_DR", "NPDR", "PDR"))
      )
  }
}

# Load and preprocess data
load_and_prepare_data <- function(data_path) {
  data <- readr::read_csv(data_path) |> 
    mutate(
      outcome_va_vi = outcome_VA_logMAR >= 0.3,
      outcome_va_blind = outcome_VA_logMAR >= 1,
      outcome_va_vi_binary = if_else(outcome_va_vi, 1, 0),
      outcome_va_blind_binary = if_else(outcome_va_blind, 1, 0),
      ever_lapse_cat = factor(person_ever_lapse),
      ever_lapse_binary = if_else(ever_lapse_cat == TRUE, 1, 0),
      MRN = dplyr::min_rank(e_mrn_deidentified),
      gender_cat = factor(gender),
      race_ethnic_cat = factor(race_ethnicity_gp),
      insurance_cat = factor(insurance_gp),
      age_cat = cut(age, breaks = c(0, 20, 45, 65, Inf), 
                    labels = c("0-20", "21-45", "46-65", "65+")),
      person_dr = case_when(
        No_DR == TRUE ~ 0,
        NPDR == "Mild" ~ 1,
        NPDR == "Moderate" ~ 1,
        NPDR == "Severe" ~ 1,
        PDR == "Present" ~ 2,
      ),
      person_dr_severity = case_when(
        No_DR == TRUE ~ 0,
        NPDR == "Mild" ~ 1,
        NPDR == "Moderate" ~ 2,
        NPDR == "Severe" ~ 3,
        PDR == "Present" ~ 4,
      ),
      person_ever_treat = case_when(
        other_inject == 0 & anti_VEGF == 0 & 
          focal_laser_flag == 0 & PRP_flag == 0 ~ FALSE,
        other_inject == 1 | anti_VEGF == 1 | 
          focal_laser_flag == 1 | PRP_flag == 1 ~ TRUE
      ),
      glaucoma_bef_hitplus_cat = case_when(
        Glaucoma_bef_hitplus != "Present" | 
          is.na(Glaucoma_bef_hitplus) ~ FALSE,
        Glaucoma_bef_hitplus == "Present" ~ TRUE
      ),
      glaucoma_after_hitplus_cat = case_when(
        Glaucoma_after_hitplus != "Present" | 
          is.na(Glaucoma_after_hitplus) ~ FALSE,
        Glaucoma_after_hitplus == "Present" ~ TRUE
      ),
      otherretina_bef_hitplus_cat = case_when(
        Otherretina_bef_hitplus != "Present" | 
          is.na(Otherretina_bef_hitplus) ~ FALSE,
        Otherretina_bef_hitplus == "Present" ~ TRUE
      ),
      otherretina_after_hitplus_cat = case_when(
        Otherretina_after_hitplus != "Present" | 
          is.na(Otherretina_after_hitplus) ~ FALSE,
        Otherretina_after_hitplus == "Present" ~ TRUE
      ),
      other_inject = factor(other_inject),
      anti_VEGF = factor(anti_VEGF),
      focal_laser_flag = factor(focal_laser_flag),
      PRP_flag = factor(PRP_flag),
      glaucoma_bef_hitplus_cat = factor(glaucoma_bef_hitplus_cat),
      otherretina_bef_hitplus_cat = factor(otherretina_bef_hitplus_cat),
      catsurg_before_hitplus_cat = factor(cataract_surgery_bef_hitplus)
    )
  
  return(data)
}

# Apply standard exclusions
apply_exclusions <- function(data) {
  data |>
    filter(!is.na(CCI) & !is.na(DCSI))
}

# Load cached PS results
load_cached_results <- function(ps_dir = "ps_results") {
  if (!dir.exists(ps_dir)) {
    message("PS results directory not found: ", ps_dir)
    return(list())
  }
  
  results_files <- list.files(ps_dir, pattern = "\\.rds$", 
                              full.names = TRUE)
  
  if (length(results_files) == 0) {
    message("No cached results found in ", ps_dir)
    return(list())
  }
  
  cached_results <- lapply(results_files, readRDS)
  names(cached_results) <- gsub("\\.rds$", "", basename(results_files))
  
  message("Loaded ", length(cached_results), " cached results")
  return(cached_results)
}

# Save original image helper
save_og_image <- function(plot, filename, width = 800, height = 1000) {
  png(filename = filename, width = width, height = height)
  print(plot)
  dev.off()
}

# Note: load_required_libraries is now defined in dependencies.R
# This avoids duplicate function definitions

# Create survey design for clustered data
create_survey_design <- function(data, weights = NULL) {
  if (is.null(weights)) {
    svydesign(
      ids = ~MRN,
      data = data
    )
  } else {
    svydesign(
      ids = ~MRN,
      weights = weights,
      data = data
    )
  }
}

# Extract and format model results
extract_model_results <- function(model, outcome_name) {
  tidy(model, conf.int = TRUE, exponentiate = TRUE) |>
    filter(term == "ever_lapse_binary") |>
    mutate(
      outcome = outcome_name,
      estimate_ci = sprintf("%.3f (%.3f, %.3f)", 
                            estimate, conf.low, conf.high),
      p_value = sprintf("%.4f", p.value)
    ) |>
    select(outcome, estimate, conf.low, conf.high, 
           estimate_ci, p_value)
}

# Create balance table
create_balance_table <- function(matched_data, formula, method_name) {
  bal_tab <- bal.tab(
    formula,
    data = matched_data,
    stats = c("mean.diffs", "variance.ratios"),
    thresholds = c(m = 0.1, v = 2)
  )
  
  balance_summary <- data.frame(
    Method = method_name,
    Mean_Diff_Max = max(abs(bal_tab$Balance$Diff.Adj), na.rm = TRUE),
    Mean_Diff_Mean = mean(abs(bal_tab$Balance$Diff.Adj), na.rm = TRUE),
    Var_Ratio_Max = max(bal_tab$Balance$V.Ratio.Adj, na.rm = TRUE),
    Var_Ratio_Min = min(bal_tab$Balance$V.Ratio.Adj, na.rm = TRUE)
  )
  
  return(balance_summary)
}

# Define formulas used across analyses

# Original matching formula (used by analysis.Rmd)
get_matching_formula <- function() {
  as.formula("ever_lapse_binary ~ 
    baseline_VA_logMAR +
    gender_cat +
    race_ethnic_cat +
    insurance_cat +
    age_cat +
    CCI +
    DCSI +
    other_inject +
    anti_VEGF +
    focal_laser_flag +
    PRP_flag +
    glaucoma_bef_hitplus_cat +
    otherretina_bef_hitplus_cat +
    catsurg_before_hitplus_cat")
}

# Enhanced matching formula for reanalysis (includes dr_severity)
get_matching_formula_reanalysis <- function() {
  as.formula("ever_lapse_binary ~ 
    baseline_VA_logMAR +
    gender_cat +
    race_ethnic_cat +
    insurance_cat +
    age_cat +
    CCI +
    DCSI +
    other_inject +
    anti_VEGF +
    focal_laser_flag +
    PRP_flag +
    glaucoma_bef_hitplus_cat +
    otherretina_bef_hitplus_cat +
    catsurg_before_hitplus_cat +
    dr_severity")
}

get_analysis_formulas <- function() {
  list(
    simple = as.formula("outcome_va_vi_binary ~ ever_lapse_binary"),
    middle = as.formula("outcome_va_vi_binary ~ 
      ever_lapse_binary + 
      gender_cat +
      race_ethnic_cat +
      insurance_cat +
      age_cat"),
    full = as.formula("outcome_va_vi_binary ~
      ever_lapse_binary + 
      gender_cat +
      race_ethnic_cat +
      insurance_cat +
      age_cat +
      CCI +
      DCSI +
      other_inject +
      anti_VEGF +
      focal_laser_flag +
      PRP_flag +
      glaucoma_bef_hitplus_cat +
      otherretina_bef_hitplus_cat +
      catsurg_before_hitplus_cat")
  )
}

# Enhanced outcome formulas for reanalysis MAIN EFFECTS (includes dr_severity only)
get_analysis_formulas_reanalysis <- function() {
  list(
    simple = as.formula("outcome_va_vi_binary ~ ever_lapse_binary"),
    middle = as.formula("outcome_va_vi_binary ~ 
      ever_lapse_binary + 
      gender_cat +
      race_ethnic_cat +
      insurance_cat +
      age_cat"),
    full = as.formula("outcome_va_vi_binary ~
      ever_lapse_binary + 
      dr_severity +
      baseline_VA_logMAR +
      gender_cat +
      race_ethnic_cat +
      insurance_cat +
      age_cat +
      CCI +
      DCSI +
      other_inject +
      anti_VEGF +
      focal_laser_flag +
      PRP_flag +
      glaucoma_bef_hitplus_cat +
      otherretina_bef_hitplus_cat +
      catsurg_before_hitplus_cat")
  )
}

# Enhanced outcome formulas for reanalysis INTERACTIONS (includes dr_severity AND any_treatment)
get_analysis_formulas_reanalysis_interaction <- function() {
  list(
    simple = as.formula("outcome_va_vi_binary ~ ever_lapse_binary"),
    middle = as.formula("outcome_va_vi_binary ~ 
      ever_lapse_binary + 
      gender_cat +
      race_ethnic_cat +
      insurance_cat +
      age_cat"),
    full = as.formula("outcome_va_vi_binary ~
      ever_lapse_binary + 
      dr_severity +
      any_treatment +
      ever_lapse_binary:dr_severity +
      ever_lapse_binary:any_treatment +
      dr_severity:any_treatment +
      ever_lapse_binary:dr_severity:any_treatment +
      baseline_VA_logMAR +
      gender_cat +
      race_ethnic_cat +
      insurance_cat +
      age_cat +
      CCI +
      DCSI +
      glaucoma_bef_hitplus_cat +
      otherretina_bef_hitplus_cat +
      catsurg_before_hitplus_cat")
  )
}

# Progress reporting for Quarto notebooks
# Shows progress in rendered output and during interactive execution
report_progress <- function(current, total, message = "Processing", newline = TRUE) {
  # Create progress message
  pct <- round(100 * current / total)
  progress_msg <- sprintf("%s: %d of %d (%d%%)", message, current, total, pct)
  
  # For interactive sessions, use cat with carriage return
  if (interactive()) {
    if (newline || current == total) {
      cat("\r", progress_msg, "\n", sep = "")
    } else {
      cat("\r", progress_msg, sep = "")
    }
    flush.console()
  }
  
  # Return the message for inline display in rendered documents
  # This will be visible in the rendered HTML
  invisible(progress_msg)
}

# Initialize progress tracking
init_progress <- function(total, message = "Starting processing") {
  progress_state <- list(
    total = total,
    current = 0,
    message = message,
    start_time = Sys.time()
  )
  
  if (interactive()) {
    cat(message, "\n")
  }
  
  return(progress_state)
}

# Update progress with timing information
update_progress <- function(progress_state, increment = 1) {
  progress_state$current <- progress_state$current + increment
  
  # Calculate elapsed and estimated time
  elapsed <- as.numeric(difftime(Sys.time(), progress_state$start_time, units = "secs"))
  rate <- progress_state$current / elapsed
  remaining <- (progress_state$total - progress_state$current) / rate
  
  # Create detailed message
  if (progress_state$current < progress_state$total) {
    time_msg <- sprintf(" (ETA: %.0f seconds)", remaining)
  } else {
    time_msg <- sprintf(" (completed in %.1f seconds)", elapsed)
  }
  
  progress_msg <- report_progress(
    progress_state$current, 
    progress_state$total,
    paste0(progress_state$message, time_msg),
    newline = (progress_state$current == progress_state$total)
  )
  
  return(progress_state)
}

# Variable lists for reanalysis
get_reanalysis_variables <- function() {
  list(
    # Variables used in propensity score models
    ps_model_vars = c(
      "ever_lapse_binary",
      "baseline_VA_logMAR",
      "gender_cat",
      "race_ethnic_cat", 
      "insurance_cat",
      "age_cat",
      "CCI",
      "DCSI",
      "other_inject",
      "anti_VEGF",
      "focal_laser_flag",
      "PRP_flag",
      "glaucoma_bef_hitplus_cat",
      "otherretina_bef_hitplus_cat",
      "catsurg_before_hitplus_cat"
    ),
    
    # Enhanced PS variables for reanalysis
    ps_model_vars_enhanced = c(
      "ever_lapse_binary",
      "baseline_VA_logMAR",
      "gender_cat",
      "race_ethnic_cat", 
      "insurance_cat",
      "age_cat",
      "CCI",
      "DCSI",
      "other_inject",
      "anti_VEGF",
      "focal_laser_flag",
      "PRP_flag",
      "glaucoma_bef_hitplus_cat",
      "otherretina_bef_hitplus_cat",
      "catsurg_before_hitplus_cat",
      "dr_severity"
    ),
    
    # Outcome variables
    outcome_vars = c(
      "outcome_VA_logMAR",
      "outcome_va_vi_binary",
      "person_ever_lapse",
      "cohort_id"
    ),
    
    # DR severity variables
    dr_vars = c(
      "No_DR",
      "NPDR",
      "PDR"
    ),
    
    # Temporal VA variables
    temporal_vars = c(
      "cohort_id",
      "baseline_VA_logMAR",
      "intermediate_VA_logMAR",
      "outcome_VA_logMAR"
    )
  )
}

# Model fitting and summary functions
fit_logistic_models <- function(design, formulas) {
  models <- list()
  for (i in seq_along(formulas)) {
    model <- svyglm(
      formula = formulas[[i]],
      design = design,
      family = quasibinomial(),
      contrasts = NULL
    )
    models[[names(formulas)[i]]] <- model
  }
  models
}

summarize_logistic_models <- function(models, method_name) {
  roundTo <- 2
  roundP <- 3

  results <- lapply(names(models), function(name) {
    model <- models[[name]]

    model_summary <- summary(model)
    coef_table <- model_summary$coefficients
    estimate <- coef_table[2, "Estimate"]

    or <- formatC(exp(estimate), format = "f", digits = roundTo)
    estimate <- formatC(estimate, format = "f", digits = roundTo)
    se <- formatC(coef_table[2, "Std. Error"], format = "f", digits = roundTo)
    t_value <- formatC(coef_table[2, "t value"], format = "f", digits = roundTo)
    p_value <- formatC(coef_table[2, "Pr(>|t|)"], format = "f", digits = roundP)

    confint_values <- confint(model, level = 0.95)[2, ]
    ci_lb_or <- formatC(exp(confint_values[1]), format = "f", digits = roundTo)
    ci_ub_or <- formatC(exp(confint_values[2]), format = "f", digits = roundTo)

    package <- if (method_name %in% c("cbps", "entropy", "bart")) "WeightIt"
    else if (method_name %in% c("twang_gbm")) "twang"
    else "MatchIt"

    approach <- if (method_name %in% c("cbps", "entropy", "bart", "twang_gbm")) "ipw"
    else if (method_name == "subclass_glm") "subclassification"
    else "match w/ replacement"

    tibble::tibble(
      package = package,
      method = method_name,
      approach = approach,
      estimand = "ATT",
      formula = name,
      estimate = estimate,
      estimate_or = or,
      se = se,
      t_value = t_value,
      p_value = p_value,
      lb_95_or = ci_lb_or,
      ub_95_or = ci_ub_or
    )
  })

  bind_rows(results)
}

tidy_output_model_formatted <- function(model, digits = 2) {
  tidy(model, exponentiate = TRUE, conf.int = TRUE) |>
    mutate(
      estimate = round(estimate, digits),
      std.error = round(std.error, digits),
      statistic = round(statistic, digits),
      conf.low = round(conf.low, digits),
      conf.high = round(conf.high, digits),
      p.value = round(p.value, digits + 1),
      # OR_CI = paste0(estimate, " (", conf.low, ", ", conf.high, ")"),
      p.value.sum = case_when(
        p.value < 0.001 ~ "<.001",
        TRUE ~ formatC(p.value, format="f", digits = 3)
      )
    )
}