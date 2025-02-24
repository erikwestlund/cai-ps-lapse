data_last_updated <- as.Date('2023-08-29')

# Working directly on SAFE Desktop:
s_root <- "S:/SDoHinDRWestlundE"

# Workign on Crunchr:
#s_root <- "~/workspace/SDoHinDRWestlundE" 

# You may need to alter the CSV file for your source data.
source_file_path <- paste0(s_root, "/df_update_lapses_to_EW.csv")


ps_dir <- file.path(s_root, "ps_results")