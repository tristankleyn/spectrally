library(dplyr)
library(imager)

spectraFromImages <- function(folder, window, axis = 1, select_files = c(), N = 10,
                              medianNormImg = FALSE, sumNormSpec = TRUE, verbose = FALSE) {
  count <- 1
  img_data <- data.frame()
  if (length(select_files) == 0) {
    select_files <- dir(folder)
  }
  num_files <- length(select_files)

  # Initialize progress bar
  pb <- txtProgressBar(min = 0, max = num_files, style = 3)
  
  for (n in 1:num_files) {
    file <- select_files[n]
    if (verbose == TRUE) {
      print(sprintf('Loaded file: %s', file))
    }
    
    img <- load.image(sprintf('%s/%s', folder, file)) %>% grayscale()
    img <- as.array(img)
    if (medianNormImg == TRUE) {
      img <- img / median(img)
    }
    
    for (m in 1:N) {
      ind <- sample(window[1]:window[2], 1)
      row <- data.frame(id = count, filename = file, index = ind)
      if (axis == 1) {
        row1 <- as.numeric(img[ind, , , ])
      } else {
        row1 <- as.numeric(img[, ind, , ])
      }
      
      if (sumNormSpec == TRUE) {
        row1 <- row1 / sum(row1)
      }
      
      row1 <- t(as.data.frame(row1))
      colnames(row1) <- 1:length(row1)
      rownames(row1) <- c(1)
      row <- cbind(row, row1)
      img_data <- rbind(img_data, row)
      rownames(img_data) <- 1:nrow(img_data)
      count <- count + 1
    }
    # Update progress bar
    setTxtProgressBar(pb, n)
  }
  # Close progress bar
  close(pb)
  
  return(img_data)
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


