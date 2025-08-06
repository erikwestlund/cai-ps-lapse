# functions.R
# Shared functions for the PS Lapse analysis project
# Created: 2025-08-05

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

# Load all required libraries
load_required_libraries <- function() {
  library(broom)
  library(cobalt)
  library(dplyr)
  library(flextable)
  library(gee)
  library(ggplot2)
  library(gt)
  library(gtsummary)
  library(kableExtra)
  library(jtools)
  library(margins)
  library(MatchIt)
  library(purrr)
  library(readr)
  library(survey)
  library(twang)
  library(WeightIt)
}

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