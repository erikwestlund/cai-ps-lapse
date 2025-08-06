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

# First, create the treatment type variable with expanded categories
m_data <- m_data |>
  mutate(
    treatment_type = case_when(
      anti_VEGF == 1 ~ "anti_VEGF",
      PRP_flag == 1 ~ "PRP",
      other_inject == 1 ~ "other_inject",
      focal_laser_flag == 1 ~ "focal_laser",
      TRUE ~ "no_treatment"
    ),
    treatment_type = factor(treatment_type, 
                           levels = c("no_treatment", "anti_VEGF", "PRP", 
                                    "other_inject", "focal_laser")),
    # Also create a severe DR indicator
    severe_dr = (NPDR == "Severe" | PDR == "Present")
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

# Create separate data subsets for analysis
data_subsets <- list(
  all_cases = m_data,
  severe_dr_only = m_data |> filter(severe_dr == TRUE)
)

# Loop through both data subsets and both weighted and unweighted designs
for(subset_name in names(data_subsets)) {
  
  current_data <- data_subsets[[subset_name]]
  
  cat("\n\n========================================\n")
  cat(sprintf("DATA SUBSET: %s (N = %d)\n", toupper(subset_name), nrow(current_data)))
  cat("========================================\n")
  
  # Update weights for current subset
  if("twang_att_w" %in% names(current_data)) {
    # Weights already exist from full dataset
  } else {
    current_data$twang_att_w <- m_data$twang_att_w[m_data$e_mrn_deidentified %in% current_data$e_mrn_deidentified]
  }
  
  for(weight_type in c("unweighted", "weighted")) {
  
    cat(sprintf("\n----- ANALYSIS TYPE: %s -----\n", toupper(weight_type)))
    
    # Create survey design for current subset
    current_design <- if(weight_type == "unweighted") {
      svydesign(
        ids = ~e_mrn_deidentified,
        data = current_data,
        weights = ~1
      )
    } else {
      svydesign(
        ids = ~e_mrn_deidentified,
        data = current_data,
        weights = ~twang_att_w
      )
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
  
    # Store results for this weight type and subset
    if(!subset_name %in% names(interaction_results_list)) {
      interaction_results_list[[subset_name]] <- list()
    }
    interaction_results_list[[subset_name]][[weight_type]] <- list(
      dr_model = model_dr_interaction,
      treatment_model = model_treatment_interaction,
      dr_pvalues = dr_interaction_pvals,
      treatment_pvalues = treatment_interaction_pvals
    )
  } # end weight_type loop
} # end subset_name loop

# Create a comparison summary table of interaction effects
comparison_rows <- list()
for(subset_name in names(interaction_results_list)) {
  for(weight_type in c("unweighted", "weighted")) {
    subset_results <- interaction_results_list[[subset_name]][[weight_type]]
    
    comparison_rows[[paste(subset_name, weight_type, sep="_")]] <- data.frame(
      Subset = subset_name,
      Model = c("DR Severity", "Treatment Type"),
      Weight_Type = weight_type,
      N_Interactions = c(
        length(subset_results$dr_pvalues),
        length(subset_results$treatment_pvalues)
      ),
      Min_P_Value = c(
        ifelse(length(subset_results$dr_pvalues) > 0, 
               min(subset_results$dr_pvalues), NA),
        ifelse(length(subset_results$treatment_pvalues) > 0, 
               min(subset_results$treatment_pvalues), NA)
      ),
      Significant_at_05 = c(
        sum(subset_results$dr_pvalues < 0.05),
        sum(subset_results$treatment_pvalues < 0.05)
      )
    )
  }
}
comparison_results <- do.call(rbind, comparison_rows)

cat("\n\n========================================\n")
cat("COMPARISON SUMMARY OF INTERACTION TESTS\n")
cat("========================================\n")
print(comparison_results)

# Check for significant interactions and report
cat("\n\n========================================\n")
cat("INTERPRETATION OF RESULTS\n")
cat("========================================\n")

for(subset_name in names(interaction_results_list)) {
  cat(sprintf("\n--- %s ---\n", toupper(subset_name)))
  
  # Check unweighted results
  unweighted_results <- interaction_results_list[[subset_name]][["unweighted"]]
  if(any(unweighted_results$dr_pvalues < 0.05)) {
    cat(sprintf("\n** %s UNWEIGHTED: Significant DR severity interactions detected **\n", subset_name))
    sig_terms <- names(unweighted_results$dr_pvalues)[
      unweighted_results$dr_pvalues < 0.05
    ]
    cat("Significant terms:", paste(sig_terms, collapse = ", "), "\n")
  }
  
  if(any(unweighted_results$treatment_pvalues < 0.05)) {
    cat(sprintf("\n** %s UNWEIGHTED: Significant treatment type interactions detected **\n", subset_name))
    sig_terms <- names(unweighted_results$treatment_pvalues)[
      unweighted_results$treatment_pvalues < 0.05
    ]
    cat("Significant terms:", paste(sig_terms, collapse = ", "), "\n")
  }
  
  # Check weighted results
  weighted_results <- interaction_results_list[[subset_name]][["weighted"]]
  if(any(weighted_results$dr_pvalues < 0.05)) {
    cat(sprintf("\n** %s WEIGHTED (IPTW): Significant DR severity interactions detected **\n", subset_name))
    sig_terms <- names(weighted_results$dr_pvalues)[
      weighted_results$dr_pvalues < 0.05
    ]
    cat("Significant terms:", paste(sig_terms, collapse = ", "), "\n")
  }
  
  if(any(weighted_results$treatment_pvalues < 0.05)) {
    cat(sprintf("\n** %s WEIGHTED (IPTW): Significant treatment type interactions detected **\n", subset_name))
    sig_terms <- names(weighted_results$treatment_pvalues)[
      weighted_results$treatment_pvalues < 0.05
    ]
    cat("Significant terms:", paste(sig_terms, collapse = ", "), "\n")
  }
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
# Visualization of Interaction Effects
# ============================================================================

library(ggplot2)
library(dplyr)
library(tidyr)

cat("\n\n========================================\n")
cat("CREATING INTERACTION VISUALIZATIONS\n")
cat("========================================\n")

# Function to extract predicted probabilities for interaction plots
extract_predictions <- function(model, data, interaction_var, fixed_vars = NULL) {
  # Create prediction data
  pred_data <- expand.grid(
    ever_lapse_binary = c(0, 1),
    stringsAsFactors = FALSE
  )
  
  # Add the interaction variable levels
  if(interaction_var == "person_dr") {
    pred_data <- expand.grid(
      ever_lapse_binary = c(0, 1),
      person_dr = unique(data$person_dr),
      stringsAsFactors = FALSE
    )
    pred_data$person_dr <- factor(pred_data$person_dr)
  } else if(interaction_var == "treatment_type") {
    pred_data <- expand.grid(
      ever_lapse_binary = c(0, 1),
      treatment_type = levels(data$treatment_type),
      stringsAsFactors = FALSE
    )
  }
  
  # Set other variables to their reference levels or means
  for(var in all.vars(formula(model))[-1]) {
    if(!var %in% names(pred_data)) {
      if(is.factor(data[[var]]) || is.character(data[[var]])) {
        pred_data[[var]] <- factor(levels(factor(data[[var]]))[1], 
                                  levels = levels(factor(data[[var]])))
      } else {
        pred_data[[var]] <- mean(data[[var]], na.rm = TRUE)
      }
    }
  }
  
  # Get predictions
  pred_data$predicted <- predict(model, newdata = pred_data, type = "response")
  pred_data$se <- predict(model, newdata = pred_data, type = "response", se.fit = TRUE)$se.fit
  pred_data$lower <- pred_data$predicted - 1.96 * pred_data$se
  pred_data$upper <- pred_data$predicted + 1.96 * pred_data$se
  
  return(pred_data)
}

# Create plots for each subset and weight combination
plot_list <- list()

for(subset_name in names(interaction_results_list)) {
  for(weight_type in c("unweighted", "weighted")) {
    
    current_results <- interaction_results_list[[subset_name]][[weight_type]]
    current_data <- data_subsets[[subset_name]]
    
    # Plot 1: DR Severity Interaction
    dr_pred <- extract_predictions(current_results$dr_model, current_data, "person_dr")
    dr_pred$lapse_status <- factor(dr_pred$ever_lapse_binary, 
                                   levels = c(0, 1),
                                   labels = c("No Lapse", "Lapse"))
    dr_pred$dr_label <- factor(dr_pred$person_dr,
                               levels = c(0, 1, 2),
                               labels = c("No DR", "NPDR", "PDR"))
    
    p_dr <- ggplot(dr_pred, aes(x = dr_label, y = predicted, 
                                color = lapse_status, group = lapse_status)) +
      geom_point(position = position_dodge(0.2), size = 3) +
      geom_errorbar(aes(ymin = lower, ymax = upper), 
                   width = 0.2, position = position_dodge(0.2)) +
      geom_line(position = position_dodge(0.2), alpha = 0.5) +
      labs(title = paste("DR Severity Interaction:", subset_name, "-", weight_type),
           x = "DR Severity", 
           y = "Predicted Probability of Vision Impairment",
           color = "Lapse Status") +
      theme_minimal() +
      theme(legend.position = "bottom") +
      scale_y_continuous(limits = c(0, 1))
    
    # Plot 2: Treatment Type Interaction
    tx_pred <- extract_predictions(current_results$treatment_model, current_data, "treatment_type")
    tx_pred$lapse_status <- factor(tx_pred$ever_lapse_binary,
                                   levels = c(0, 1),
                                   labels = c("No Lapse", "Lapse"))
    
    p_tx <- ggplot(tx_pred, aes(x = treatment_type, y = predicted,
                                color = lapse_status, group = lapse_status)) +
      geom_point(position = position_dodge(0.2), size = 3) +
      geom_errorbar(aes(ymin = lower, ymax = upper),
                   width = 0.2, position = position_dodge(0.2)) +
      geom_line(position = position_dodge(0.2), alpha = 0.5) +
      labs(title = paste("Treatment Type Interaction:", subset_name, "-", weight_type),
           x = "Treatment Type",
           y = "Predicted Probability of Vision Impairment",
           color = "Lapse Status") +
      theme_minimal() +
      theme(legend.position = "bottom",
            axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_y_continuous(limits = c(0, 1))
    
    # Store plots
    plot_name_dr <- paste(subset_name, weight_type, "dr", sep = "_")
    plot_name_tx <- paste(subset_name, weight_type, "treatment", sep = "_")
    plot_list[[plot_name_dr]] <- p_dr
    plot_list[[plot_name_tx]] <- p_tx
    
    # Save plots
    ggsave(paste0("interaction_plot_dr_", subset_name, "_", weight_type, ".png"), 
           p_dr, width = 8, height = 6, dpi = 300)
    ggsave(paste0("interaction_plot_tx_", subset_name, "_", weight_type, ".png"), 
           p_tx, width = 10, height = 6, dpi = 300)
  }
}

# Create a combined plot showing treatment effects by subgroup
# Extract coefficients for lapse effect in each treatment subgroup
treatment_effects <- data.frame()

for(subset_name in names(interaction_results_list)) {
  for(weight_type in c("unweighted", "weighted")) {
    model <- interaction_results_list[[subset_name]][[weight_type]]$treatment_model
    coef_summary <- summary(model)$coefficients
    
    # Get main effect
    main_effect <- coef_summary["ever_lapse_binary", "Estimate"]
    main_se <- coef_summary["ever_lapse_binary", "Std. Error"]
    
    # Get interaction effects
    interaction_terms <- grep("ever_lapse_binary:treatment_type", 
                            rownames(coef_summary), value = TRUE)
    
    for(term in interaction_terms) {
      tx_type <- gsub("ever_lapse_binary:treatment_type", "", term)
      interaction_effect <- coef_summary[term, "Estimate"]
      interaction_se <- coef_summary[term, "Std. Error"]
      
      # Combined effect = main + interaction
      combined_effect <- main_effect + interaction_effect
      combined_se <- sqrt(main_se^2 + interaction_se^2) # Approximate
      
      treatment_effects <- rbind(treatment_effects, data.frame(
        subset = subset_name,
        weight_type = weight_type,
        treatment = tx_type,
        effect = combined_effect,
        se = combined_se,
        lower = combined_effect - 1.96 * combined_se,
        upper = combined_effect + 1.96 * combined_se,
        or = exp(combined_effect),
        or_lower = exp(combined_effect - 1.96 * combined_se),
        or_upper = exp(combined_effect + 1.96 * combined_se)
      ))
    }
    
    # Add reference category (no_treatment)
    treatment_effects <- rbind(treatment_effects, data.frame(
      subset = subset_name,
      weight_type = weight_type,
      treatment = "no_treatment",
      effect = main_effect,
      se = main_se,
      lower = main_effect - 1.96 * main_se,
      upper = main_effect + 1.96 * main_se,
      or = exp(main_effect),
      or_lower = exp(main_effect - 1.96 * main_se),
      or_upper = exp(main_effect + 1.96 * main_se)
    ))
  }
}

# Create forest plot of treatment-specific effects
p_forest <- ggplot(treatment_effects, 
                  aes(x = or, y = interaction(treatment, subset, weight_type))) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = or_lower, xmax = or_upper), height = 0.2) +
  geom_vline(xintercept = 1, linetype = "dashed", alpha = 0.5) +
  labs(title = "Odds Ratios for Lapse Effect by Treatment Type",
       x = "Odds Ratio (95% CI)",
       y = "Treatment Type | Subset | Weight Type") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))

ggsave("interaction_forest_plot.png", p_forest, width = 12, height = 10, dpi = 300)

cat("\n\nInteraction plots saved to working directory.\n")

# Save plot list for later use
saveRDS(plot_list, file = "interaction_plots.rds")


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