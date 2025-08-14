data_last_updated <- as.Date('2025-08-13')

# Working directly on SAFE Desktop:
s_root <- "S:/SDoHinDRWestlundE"

# Working on Crunchr:
#s_root <- "~/workspace/SDoHinDRWestlundE" 

# Original data file for reference
source_file_path <- paste0(s_root, "/df_update_lapses_to_EW.csv")

# Combined data file for reanalysis with multiple imputation
reanalysis_data_file_path <- paste0(s_root, "/Gina's Project/df_combined_130825.csv")

# Original analysis data file for comparison
original_analysis_file_path <- paste0(s_root, "/Gina's Project/df_Final_240909.csv")

ps_dir <- file.path(s_root, "ps_results")