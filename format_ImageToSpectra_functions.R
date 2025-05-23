library(dplyr)
library(fs)
library(imager)

processImageFolder <- function(input_folder) {
  image_files <- dir(input_folder)
  dimensions <- list()
  
  # First, get dimensions of all images
  for (file in image_files) {
    img_path <- file.path(input_folder, file)
    img <- load.image(img_path)
    img_gray <- if (dim(img)[3] > 1) img[,,,1] else img
    dimensions[[file]] <- dim(img_gray)[1:2]
  }
  
  # Check if all dimensions are the same
  first_dim <- dimensions[[1]]
  all_same <- all(sapply(dimensions, function(d) all(d == first_dim)))
  
  if (all_same) {
    cat("All images in", input_folder, "have the same dimensions:", first_dim, "\nNo new folder created.\n")
    return(invisible(NULL)) # Exit the function
  } else {
    cat("Images in", input_folder, "have different dimensions. \nFinding and cropping...\n\n")
    
    # Create a new output folder with a unique name
    output_base_name <- paste0(input_folder, "_C")
    output_folder <- output_base_name
    counter <- 1
    while (dir_exists(output_folder)) {
      output_folder <- paste0(output_base_name, counter)
      counter <- counter + 1
    }
    dir_create(output_folder)

    # Find the minimum height and width
    min_height <- min(sapply(dimensions, function(d) d[1]))
    min_width <- min(sapply(dimensions, function(d) d[2]))

    # Load, convert, crop, and save images
    for (file in image_files) {
      img_path <- file.path(input_folder, file)
      img <- load.image(img_path)
      img_gray <- if (dim(img)[3] > 1) img[,,,1] else img
      
      current_height <- dim(img_gray)[1]
      current_width <- dim(img_gray)[2]
      
      if (current_height > min_height || current_width > min_width) {
        row_start <- floor((current_height - min_height) / 2) + 1
        row_end <- row_start + min_height - 1
        col_start <- floor((current_width - min_width) / 2) + 1
        col_end <- col_start + min_width - 1
        
        # Explicitly subset all dimensions
        cropped_array <- img_gray[row_start:row_end, col_start:col_end, 1, 1]
        cropped_img <- as.cimg(cropped_array, dim = c(min_height, min_width, 1, 1))
        
        save.image(cropped_img, file.path(output_folder, file))
      } else {
        save.image(img_gray, file.path(output_folder, file))
      }
    }
    cat(sprintf("Image processed and saved to %s.\n Image dimensions: %s x %s", output_folder, min_height, min_width))
    invisible(output_folder) # Return the name of the output folder
  }
}



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
    
    img <- load.image(sprintf('%s/%s', folder, file))
    img <- img[,,,1]
    img <- as.array(img)
    if (medianNormImg == TRUE) {
      img <- img / median(img)
    }
    
    for (m in 1:N) {
      ind <- sample(window[1]:window[2], 1)
      row <- data.frame(id = count, filename = file, index = ind)
      if (axis == 1) {
        row1 <- as.numeric(img[ind,])
      } else {
        row1 <- as.numeric(img[,ind])
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


