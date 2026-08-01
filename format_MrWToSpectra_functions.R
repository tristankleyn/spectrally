library(dplyr)
library(stringr)
library(purrr)
library(readr)
library(matrixStats)
library(lubridate)

formatIntoSpec <- function(dir_path, skip_head=0, skip_tail=0, respPrefix='R_', norm_method=NULL) {
  
  # 1. Find all CSV files matching trial[a-z]{5}.csv (or trial-[a-z]{5}.csv)
  csv_files <- list.files(
    path = dir_path,
    pattern = "^trial-?[a-z]{5}\\.csv$",
    full.names = TRUE
  )
  
  # 2. Read, trim top/bottom rows, assign trial_id, and combine
  data <- csv_files %>% 
    map_dfr(function(file) {
      trial_id <- str_extract(basename(file), "[a-z]{5}(?=\\.csv$)")
      
      df <- read.csv(file)
      n_rows <- nrow(df)
      
      start_idx <- 1 + skip_head
      end_idx   <- n_rows - skip_tail
      
      if (start_idx <= end_idx && start_idx <= n_rows) {
        df <- df[start_idx:end_idx, , drop = FALSE]
      } else {
        df <- df[0, , drop = FALSE]
      }
      
      df %>% mutate(trial = trial_id, .before = 1)
    })
  
  # 3. Identify matching R_ columns directly on current names(data)
  # Ensure regex prefix starts with '^'
  clean_prefix <- ifelse(startsWith(respPrefix, "^"), respPrefix, paste0("^", respPrefix))
  
  matching_cols <- setdiff(names(data)[str_detect(names(data), clean_prefix)], "trial")
  non_matching_cols <- setdiff(names(data), c("trial", matching_cols))
  
  # Create new sequential names for matching columns ("1", "2", "3"...)
  renamed_matching_names <- as.character(seq_along(matching_cols))
  
  # Rename matching columns safely
  names(data)[names(data) %in% matching_cols] <- renamed_matching_names
  
  # 4. Reorder: trial, non-matching, renamed matching, then add id
  data <- data %>% 
    select(trial, all_of(non_matching_cols), all_of(renamed_matching_names)) %>% 
    mutate(id = row_number()) %>% 
    relocate(id, .before = 1)
  
  # 5. Extract Month, Day, Hour from Timestamp
  if ("Timestamp" %in% names(data)) {
    data <- data %>% 
      mutate(
        dt    = ymd_hms(Timestamp),
        month = month(dt),
        day   = day(dt),
        hour  = hour(dt)
      ) %>% 
      relocate(month, day, hour, .after = Timestamp) %>% 
      select(-dt) # Safely drop the temporary datetime variable
  }
  
  # 6. Row-wise Normalization
  if (!is.null(norm_method)) {
    mat <- as.matrix(data[, renamed_matching_names])
    
    if (norm_method == "sum") {
      row_sums <- rowSums(mat, na.rm = TRUE)
      mat <- mat / row_sums
      
    } else if (norm_method == "minmax") {
      r_min <- rowMins(mat, na.rm = TRUE)
      r_max <- rowMaxs(mat, na.rm = TRUE)
      r_range <- r_max - r_min
      
      r_range[r_range == 0] <- 1  # Prevent division by zero
      mat <- (mat - r_min) / r_range
      
    } else {
      warning("Unknown norm_method provided. Columns were left un-normalized.")
    }
    
    data[, renamed_matching_names] <- mat
  }
  
  return(data)
}


exportToCSV_dt <- function(df, name='spectra') {
  if (!'formattedData' %in% dir()) {
    dir.create('formattedData')
  }
  
  dt <- gsub(' ', '-', substr(Sys.time(), 1, 16))
  dt <- gsub(':', '-', dt)
  dt <- gsub('-','', dt)
  write.csv(df, sprintf('formattedData/%s_%s.csv', name, dt), row.names=FALSE)
  cat(sprintf('Saved formatted data to formattedData/%s_%s.csv', name, dt))
} 





