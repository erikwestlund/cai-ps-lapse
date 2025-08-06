# reviewerResponse.R
# Analysis code to address reviewer comments
# Created: 2025-08-05

# Load required libraries and settings
source("settings.R")
source("functions.R")

# Load all required libraries
load_required_libraries()

# Load and prepare data
data_path <- file.path(s_root, "Gina's Project/df_Final_240909.csv")
data <- load_and_prepare_data(data_path)

# Apply the same exclusions as main analysis
m_data <- apply_exclusions(data)

# Load cached results if available
cached_results <- load_cached_results("ps_results")

# Load or generate twang weights for IPTW
# Check if twang model exists in cached results
if("twang_gbm" %in% names(cached_results)) {
  cat("Loading cached twang model for IPTW weights...\n")
  twang_model <- cached_results$twang_gbm$result
} else {
  cat("Running twang model to generate IPTW weights...\n")
  # Get the matching formula from functions.R
  matchingFormula <- get_matching_formula()
  
  # Run twang ps model
  library(twang)
  twang_model <- ps(
    formula = matchingFormula, 
    data = data.frame(m_data),
    n.trees = 10000,
    interaction.depth = 3,
    shrinkage = 0.01,
    estimand = "ATT",
    stop.method = c("es.mean", "ks.max")
  )
  
  # Save for future use
  if(!dir.exists("ps_results")) {
    dir.create("ps_results")
  }
  saveRDS(list(result = twang_model), file = "ps_results/twang_gbm.rds")
}

# Extract IPTW weights from twang model
m_data$twang_att_w <- get.weights(twang_model, stop.method = "es.mean")

# ============================================================================
# Reviewer Question 1: Subgroup Testing with Formal Interaction Terms
# Response to: "Inadequate Subgroup Testing - need formal interaction testing"
# ============================================================================

# First, create the treatment type variable
m_data <- m_data |>
  mutate(
    treatment_type = case_when(
      anti_VEGF == 1 ~ "anti_VEGF",
      PRP_flag == 1 ~ "PRP",
      other_inject == 1 | focal_laser_flag == 1 ~ "other",
      TRUE ~ "no_treatment"
    ),
    treatment_type = factor(treatment_type, 
                           levels = c("no_treatment", "anti_VEGF", "PRP", "other"))
  )

# Define the base formula (same as analysisFormulaFull but without the outcome)
base_covariates <- c(
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
)

# Model 1: Interaction with DR severity (person_dr)
# person_dr: 0 = No DR, 1 = NPDR, 2 = PDR
interaction_dr_formula <- as.formula(paste(
  "outcome_va_vi_binary ~ ever_lapse_binary * factor(person_dr) +",
  paste(base_covariates, collapse = " + ")
))

# Model 2: Interaction with treatment type
interaction_treatment_formula <- as.formula(paste(
  "outcome_va_vi_binary ~ ever_lapse_binary * treatment_type +",
  paste(base_covariates, collapse = " + ")
))

# Create survey designs for both unweighted and weighted analyses
survey_design_unweighted <- svydesign(
  ids = ~e_mrn_deidentified,
  data = m_data,
  weights = ~1
)

survey_design_weighted <- svydesign(
  ids = ~e_mrn_deidentified,
  data = m_data,
  weights = ~twang_att_w
)

# Store results for both weighted and unweighted analyses
interaction_results_list <- list()

# Loop through both weighted and unweighted designs
for(weight_type in c("unweighted", "weighted")) {
  
  cat("\n\n========================================\n")
  cat(sprintf("ANALYSIS TYPE: %s\n", toupper(weight_type)))
  cat("========================================\n")
  
  # Select appropriate survey design
  current_design <- if(weight_type == "unweighted") {
    survey_design_unweighted
  } else {
    survey_design_weighted
  }
  
  # Fit Model 1: DR severity interaction
  cat(sprintf("\n===== Model 1: Interaction with DR Severity (%s) =====\n", weight_type))
  model_dr_interaction <- svyglm(
    formula = interaction_dr_formula,
    design = current_design,
    family = quasibinomial()
  )
  
  # Summary of DR interaction model
  summary_dr <- summary(model_dr_interaction)
  print(summary_dr)
  
  # Extract interaction p-values for DR model
  dr_interaction_terms <- grep("ever_lapse_binary:factor\\(person_dr\\)", 
                              names(coef(model_dr_interaction)), value = TRUE)
  dr_interaction_pvals <- if(length(dr_interaction_terms) > 0) {
    summary_dr$coefficients[dr_interaction_terms, "Pr(>|t|)"]
  } else {
    numeric(0)
  }
  
  cat(sprintf("\n--- DR Severity Interaction P-values (%s) ---\n", weight_type))
  if(length(dr_interaction_terms) > 0) {
    for(i in seq_along(dr_interaction_terms)) {
      cat(sprintf("%s: p = %.4f\n", dr_interaction_terms[i], dr_interaction_pvals[i]))
    }
  } else {
    cat("No interaction terms found\n")
  }
  
  # Fit Model 2: Treatment type interaction  
  cat(sprintf("\n===== Model 2: Interaction with Treatment Type (%s) =====\n", weight_type))
  model_treatment_interaction <- svyglm(
    formula = interaction_treatment_formula,
    design = current_design,
    family = quasibinomial()
  )
  
  # Summary of treatment interaction model
  summary_treatment <- summary(model_treatment_interaction)
  print(summary_treatment)
  
  # Extract interaction p-values for treatment model
  treatment_interaction_terms <- grep("ever_lapse_binary:treatment_type", 
                                     names(coef(model_treatment_interaction)), value = TRUE)
  treatment_interaction_pvals <- if(length(treatment_interaction_terms) > 0) {
    summary_treatment$coefficients[treatment_interaction_terms, "Pr(>|t|)"]
  } else {
    numeric(0)
  }
  
  cat(sprintf("\n--- Treatment Type Interaction P-values (%s) ---\n", weight_type))
  if(length(treatment_interaction_terms) > 0) {
    for(i in seq_along(treatment_interaction_terms)) {
      cat(sprintf("%s: p = %.4f\n", treatment_interaction_terms[i], treatment_interaction_pvals[i]))
    }
  } else {
    cat("No interaction terms found\n")
  }
  
  # Store results for this weight type
  interaction_results_list[[weight_type]] <- list(
    dr_model = model_dr_interaction,
    treatment_model = model_treatment_interaction,
    dr_pvalues = dr_interaction_pvals,
    treatment_pvalues = treatment_interaction_pvals
  )
}

# Create a comparison summary table of interaction effects
comparison_results <- data.frame(
  Model = rep(c("DR Severity", "Treatment Type"), 2),
  Weight_Type = rep(c("Unweighted", "Weighted"), each = 2),
  N_Interactions = c(
    length(interaction_results_list$unweighted$dr_pvalues),
    length(interaction_results_list$unweighted$treatment_pvalues),
    length(interaction_results_list$weighted$dr_pvalues),
    length(interaction_results_list$weighted$treatment_pvalues)
  ),
  Min_P_Value = c(
    ifelse(length(interaction_results_list$unweighted$dr_pvalues) > 0, 
           min(interaction_results_list$unweighted$dr_pvalues), NA),
    ifelse(length(interaction_results_list$unweighted$treatment_pvalues) > 0, 
           min(interaction_results_list$unweighted$treatment_pvalues), NA),
    ifelse(length(interaction_results_list$weighted$dr_pvalues) > 0, 
           min(interaction_results_list$weighted$dr_pvalues), NA),
    ifelse(length(interaction_results_list$weighted$treatment_pvalues) > 0, 
           min(interaction_results_list$weighted$treatment_pvalues), NA)
  ),
  Significant_at_05 = c(
    sum(interaction_results_list$unweighted$dr_pvalues < 0.05),
    sum(interaction_results_list$unweighted$treatment_pvalues < 0.05),
    sum(interaction_results_list$weighted$dr_pvalues < 0.05),
    sum(interaction_results_list$weighted$treatment_pvalues < 0.05)
  )
)

cat("\n\n========================================\n")
cat("COMPARISON SUMMARY OF INTERACTION TESTS\n")
cat("========================================\n")
print(comparison_results)

# Check for significant interactions and report
cat("\n\n========================================\n")
cat("INTERPRETATION OF RESULTS\n")
cat("========================================\n")

# Check unweighted results
if(any(interaction_results_list$unweighted$dr_pvalues < 0.05)) {
  cat("\n** UNWEIGHTED: Significant DR severity interactions detected **\n")
  sig_terms <- names(interaction_results_list$unweighted$dr_pvalues)[
    interaction_results_list$unweighted$dr_pvalues < 0.05
  ]
  cat("Significant terms:", paste(sig_terms, collapse = ", "), "\n")
}

if(any(interaction_results_list$unweighted$treatment_pvalues < 0.05)) {
  cat("\n** UNWEIGHTED: Significant treatment type interactions detected **\n")
  sig_terms <- names(interaction_results_list$unweighted$treatment_pvalues)[
    interaction_results_list$unweighted$treatment_pvalues < 0.05
  ]
  cat("Significant terms:", paste(sig_terms, collapse = ", "), "\n")
}

# Check weighted results  
if(any(interaction_results_list$weighted$dr_pvalues < 0.05)) {
  cat("\n** WEIGHTED (IPTW): Significant DR severity interactions detected **\n")
  sig_terms <- names(interaction_results_list$weighted$dr_pvalues)[
    interaction_results_list$weighted$dr_pvalues < 0.05
  ]
  cat("Significant terms:", paste(sig_terms, collapse = ", "), "\n")
}

if(any(interaction_results_list$weighted$treatment_pvalues < 0.05)) {
  cat("\n** WEIGHTED (IPTW): Significant treatment type interactions detected **\n")
  sig_terms <- names(interaction_results_list$weighted$treatment_pvalues)[
    interaction_results_list$weighted$treatment_pvalues < 0.05
  ]
  cat("Significant terms:", paste(sig_terms, collapse = ", "), "\n")
}

# Note about interpretation
cat("\n\nNOTE: Interaction p-values test whether the effect of lapsing differs\n")
cat("      significantly across subgroups. Non-significant p-values suggest\n")
cat("      consistent effects across groups, supporting the main analysis.\n")
cat("      The IPTW-weighted analysis adjusts for confounding using propensity scores.\n")

# Save all results for reporting
reviewer_q1_results <- list(
  unweighted = interaction_results_list$unweighted,
  weighted = interaction_results_list$weighted,
  comparison_table = comparison_results,
  twang_weights = m_data$twang_att_w
)

saveRDS(reviewer_q1_results, file = "reviewer_q1_interaction_results.rds")

cat("\n\nResults saved to: reviewer_q1_interaction_results.rds\n")


# ============================================================================
# Reviewer Question 2: 
# ============================================================================

# Analysis code for question 2


# ============================================================================
# Reviewer Question 3: 
# ============================================================================

# Analysis code for question 3


# ============================================================================
# Additional Analyses
# ============================================================================

# Any supplementary analyses requested


# ============================================================================
# Summary Tables/Figures
# ============================================================================

# Generate any summary outputs for reviewer response