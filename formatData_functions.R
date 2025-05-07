library(dplyr)

formatIntoSpec <- function(data, varList, varX, varY, sampleIDloc, bufferRows, rtStart, rtEnd, rtStep, sumNormalize=TRUE) {
  dataSpec <- data.frame()
  
  sampleStart <- c()
  sampleID <- c()
  for (i in 1:nrow(data)) {
    vals <- unlist(data[i,])
    if (vals[1] == 1) {
      sampleID <- append(sampleID, data[i+sampleIDloc[1], 1 + sampleIDloc[2]])
      sampleStart <- append(sampleStart, i)
    }
  }
  
  
  pb <- txtProgressBar(min = 1, max = length(sampleStart), style = 3)
  for (i in 1:length(sampleStart)) {
    setTxtProgressBar(pb, i)
    start <- sampleStart[i]
    if (i != length(sampleStart)) {
      end <- sampleStart[i+1] - (bufferRows+1)
    } else {
      end <- nrow(data)
    }
    ID <- sampleID[i]
    row <- data.frame(id=ID)
    frame <- data[start:end,]
    frame <- frame[!apply(frame == "", 1, any),]
    
    names(frame) <- varList
    rownames(frame) <- 1:nrow(frame)
    frame[[varX]] <- as.numeric(frame[[varX]])
    frame[[varY]] <- as.numeric(frame[[varY]])
    if (sumNormalize == TRUE) {
      frame[[varY]] <- frame[[varY]]/sum(frame[[varY]])
    }
    
    x0 <- rtStart 
    Yvals <- c()
    Xvals <- c()
    
    while (x0 < rtEnd) {   
      x1 <- x0 + rtStep
      window <- subset(frame, frame[[varX]] > x0 & frame[[varX]] <= x1)            
      Yframe <- sum(window[[varY]])                                       
      Yvals <- append(Yvals, Yframe)                                
      Xvals <- append(Xvals, mean(c(x0, x1)))                         
      x0 <- x0 + rtStep
    }
    
    row1 <- t(data.frame(Yvals))        
    rownames(row1) <- c(1)
    colnames(row1) <- seq(1, length(Yvals)) 
    row <- cbind(row, row1) 
    dataSpec <- rbind(dataSpec, row) 
  }
  
  ind <- which(names(dataSpec)=='1')
  nSpec <- ncol(dataSpec)-ind
  nSamples <- length(unique(dataSpec$id))
  cat('\n\n')
  cat(sprintf('\033[1mData formatted into spectral table.\033[0m\n\033[1mSamples: \033[0m%s\n\033[1mLength of spectra: \033[0m%s', nSamples, nSpec))
  
  return(dataSpec)
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
