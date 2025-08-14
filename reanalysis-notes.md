# Reanalysis Notes

## Key Decisions and Assumptions

### Missing Data Handling

#### Original Analysis Approach
The original analysis handles missing data with a mixed strategy:

1. **CCI and DCSI (Comorbidity Scores)**
   - **Original Approach**: Complete case exclusion
   - **Original Implementation**: Rows with missing CCI or DCSI are removed via `apply_exclusions()`
   - **Original Justification**: These are key confounders that cannot be reasonably imputed without clinical context

2. **Glaucoma and Other Retina Conditions** 
   - **Approach**: Missing coded as absence of condition
   - **Implementation**: 
     - `Glaucoma_bef_hitplus`: Missing/NA → 0 (no glaucoma)
     - `Otherretina_bef_hitplus`: Missing/NA → 0 (no other retina condition)
   - **Justification**: Clinical assumption that these conditions would be documented if present
   - **Effect**: Propensity score models receive complete data with no missing values for these variables

3. **Cataract Surgery**
   - **Approach**: Kept as is (including missing values)
   - **Implementation**: Factor variable that may contain NA values

#### Reanalysis Approach
For the reanalysis with multiple imputation, we will:

1. **Maintain the original approach for glaucoma/retina conditions**
   - Recode missing as 0 (absence of condition) before imputation
   - This preserves the clinical assumption and comparability with original analysis

2. **Impute CCI and DCSI scores**
   - **NEW**: Unlike the original analysis, we will impute missing CCI and DCSI values
   - **Rationale**: Preserve sample size and reduce selection bias from complete case analysis
   - **Method**: Multiple imputation using predictive mean matching or similar appropriate method
   - CCI and DCSI are comorbidity indices that can be reliably imputed from other patient characteristics, demographics, and clinical variables

3. **Apply multiple imputation for other missing covariates**
   - Impute missing values in baseline VA, demographics, etc.
   - Pool results across imputed datasets

4. **Sensitivity analysis**
   - Compare complete case analysis with imputed results
   - Document any meaningful differences in findings
   - Special attention to the impact of imputing CCI/DCSI vs excluding

### Variable Definitions

#### Treatment Type Categorization
Following the reviewer response analysis, treatment is categorized as:
- `no_treatment`: No interventions recorded
- `anti_VEGF`: Anti-VEGF therapy (prioritized if multiple treatments)
- `PRP`: Pan-retinal photocoagulation (if no anti-VEGF)  
- `other_treatment`: Focal laser or other injections (if neither anti-VEGF nor PRP)

#### DR Severity
- Numeric coding: 0 = No DR, 1 = NPDR, 2 = PDR
- Factor labels: "No_DR", "NPDR", "PDR"
- Used for interaction analyses as requested by reviewers

### Analysis Pipeline

1. **Data Preparation** (`reanalysis-1-data_preparation.qmd`)
   - Load combined dataset
   - Create analysis variables
   - Handle missing data per above strategy
   - Save to network drive

2. **Multiple Imputation** (`reanalysis-2-multiple_imputation.qmd`) 
   - Impute missing covariates (except those coded as 0)
   - Generate multiple imputed datasets
   - Assess imputation quality

3. **Propensity Score Analysis** (`reanalysis-3-propensity_scores.qmd`)
   - Calculate PS using various methods on each imputed dataset
   - Assess balance
   - Create weights/matched samples

4. **Outcome Analysis** (`reanalysis-4-outcome_analysis.qmd`)
   - Fit outcome models on weighted/matched data
   - Pool results across imputations
   - Calculate marginal effects

5. **Sensitivity Analysis** (`reanalysis-5-sensitivity.qmd`)
   - Compare with complete case analysis
   - Test robustness of findings