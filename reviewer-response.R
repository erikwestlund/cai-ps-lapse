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

# Run the interaction models using survey design for clustered data
survey_design <- svydesign(
  ids = ~e_mrn_deidentified,
  data = m_data,
  weights = ~1
)

# Fit Model 1: DR severity interaction
cat("\n===== Model 1: Interaction with DR Severity =====\n")
model_dr_interaction <- svyglm(
  formula = interaction_dr_formula,
  design = survey_design,
  family = quasibinomial()
)

# Summary of DR interaction model
summary_dr <- summary(model_dr_interaction)
print(summary_dr)

# Extract interaction p-values for DR model
dr_interaction_terms <- grep("ever_lapse_binary:factor\\(person_dr\\)", 
                            names(coef(model_dr_interaction)), value = TRUE)
dr_interaction_pvals <- summary_dr$coefficients[dr_interaction_terms, "Pr(>|t|)"]

cat("\n--- DR Severity Interaction P-values ---\n")
for(i in seq_along(dr_interaction_terms)) {
  cat(sprintf("%s: p = %.4f\n", dr_interaction_terms[i], dr_interaction_pvals[i]))
}

# Fit Model 2: Treatment type interaction
cat("\n===== Model 2: Interaction with Treatment Type =====\n")
model_treatment_interaction <- svyglm(
  formula = interaction_treatment_formula,
  design = survey_design,
  family = quasibinomial()
)

# Summary of treatment interaction model
summary_treatment <- summary(model_treatment_interaction)
print(summary_treatment)

# Extract interaction p-values for treatment model
treatment_interaction_terms <- grep("ever_lapse_binary:treatment_type", 
                                   names(coef(model_treatment_interaction)), value = TRUE)
treatment_interaction_pvals <- summary_treatment$coefficients[treatment_interaction_terms, "Pr(>|t|)"]

cat("\n--- Treatment Type Interaction P-values ---\n")
for(i in seq_along(treatment_interaction_terms)) {
  cat(sprintf("%s: p = %.4f\n", treatment_interaction_terms[i], treatment_interaction_pvals[i]))
}

# Create a summary table of interaction effects
interaction_results <- data.frame(
  Model = c("DR Severity", "Treatment Type"),
  N_Interactions = c(length(dr_interaction_terms), length(treatment_interaction_terms)),
  Min_P_Value = c(
    ifelse(length(dr_interaction_pvals) > 0, min(dr_interaction_pvals), NA),
    ifelse(length(treatment_interaction_pvals) > 0, min(treatment_interaction_pvals), NA)
  ),
  Significant_at_05 = c(
    sum(dr_interaction_pvals < 0.05),
    sum(treatment_interaction_pvals < 0.05)
  )
)

cat("\n===== Summary of Interaction Tests =====\n")
print(interaction_results)

# Calculate marginal effects for significant interactions if any
if(any(dr_interaction_pvals < 0.05)) {
  cat("\n===== Marginal Effects for DR Severity Groups =====\n")
  # Calculate predicted probabilities for each DR group
  for(dr_level in unique(m_data$person_dr)) {
    subset_data <- m_data[m_data$person_dr == dr_level, ]
    if(nrow(subset_data) > 0) {
      cat(sprintf("\nDR Level %d:\n", dr_level))
      # Calculate average marginal effect for this subgroup
    }
  }
}

if(any(treatment_interaction_pvals < 0.05)) {
  cat("\n===== Marginal Effects for Treatment Type Groups =====\n")
  # Calculate predicted probabilities for each treatment group
  for(tx_type in levels(m_data$treatment_type)) {
    subset_data <- m_data[m_data$treatment_type == tx_type, ]
    if(nrow(subset_data) > 0) {
      cat(sprintf("\nTreatment Type: %s\n", tx_type))
      # Calculate average marginal effect for this subgroup
    }
  }
}

# Save results for reporting
reviewer_q1_results <- list(
  dr_model = model_dr_interaction,
  treatment_model = model_treatment_interaction,
  dr_pvalues = dr_interaction_pvals,
  treatment_pvalues = treatment_interaction_pvals,
  summary_table = interaction_results
)

saveRDS(reviewer_q1_results, file = "reviewer_q1_interaction_results.rds")


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