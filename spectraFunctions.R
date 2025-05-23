library(dplyr)
library(DT)
library(RColorBrewer)
library(tidyr)
library(scales)


printInfo <- function(data, infoType='dims') {
  if (infoType == 'dims') {
    print(sprintf('Data shape: (%s, %s)', as.integer(dim(data)[1]), as.integer(dim(data)[2])))
  }
  if (infoType == 'header') {
    print('Data preview:')
    print(head(data))
  }
}


showExampleTable <- function() {
  row <- data.frame(id=c(1,2,3), site=c('Jupiter', 'Mars', 'Mars'), species=c('A', 'A', 'C'))
  row1 <- data.frame()
  for (i in 1:3) {
    vals <- c()
    for (j in 1:20) {
      vals <- append(vals, runif(1,0,1))
    }
    vals <- vals/sum(vals)
    vals <- t(data.frame(vals))
    row1 <- rbind(row1, vals)
  }
  names(row1) <- as.character(1:20)
  rownames(row1) <- c(1,2,3)
  
  row <- cbind(row, row1)
  print(row)
  
}

checkData <- function(data) {
  ind1 <- which(names(data) %in% c('X1', 'x1', 'v1', 'V1', '1'))

  errorMessage <- c()
  #CHECKS
  if (all(sapply(data[,ind1:ncol(data)], is.numeric))) {
    consecSpec <- TRUE
  } else {
    consecSpec <- FALSE
    message <- 'Spectra columns not in consecutive order.'
    errorMessage <- append(errorMessage, message)
  }
  
  if (names(data)[1] == 'id') {
    idCol <- TRUE
  } else {
    idCol <- FALSE
    message <- 'id not found in first column'
    errorMessage <- append(errorMessage, message)
  }
  
  if (ind1 == 2) {
    metaInfo <- FALSE
    message <- 'No metadata variables found in table'
    errorMessage <- append(errorMessage, message)
    
  } else {
    metaInfo <- as.character(names(data)[2])
    if (ind1 > 3) {
      for (n in names(data)[3:(ind1-1)]) {
        metaInfo <- paste(metaInfo, n, sep=', ')
      }
    }
  }
  
  L <- ncol(data) - ind1
  if (length(errorMessage) > 0) {
    count <- 1
    print('Data not in correct format - please fix the following before proceeding:')
    for (item in errorMessage) {
      print(sprintf('%s: %s', count, item))
    }
  } else {
    cat(sprintf("\033[1mData in correct format:\033[0m\nMetavariables: %s\nNumber of spectra: %s\nLength of spectra: %s\n",
                metaInfo, as.integer(nrow(data)), L))
    

  }
  
  isNum <- c()
  for (i in ind1:ncol(data)) {
    if (all(is.numeric(data[[names(data)[i]]]))) {
      isNum <- append(isNum, TRUE)
    } else {
      isNum <- append(isNum, FALSE)
    }
  }
  if (all(isNum) == TRUE) {
    allColsNumeric <- TRUE
  } else {
    allColsNumeric <- FALSE
    cat('Not all spectral variables are numeric')
  }
  
  names(data)[(ind1):ncol(data)] <- 1:(ncol(data)-ind1+1)
  
  return(list(data=data, 'ind1'=ind1, 'vars'= names(data[2:ind1-1])))

}

plotSpec <- function(data, sample_name, source, label=NULL, color=rgb(0,0,0,0.5), rt0=0, rt1=26, xmax=NULL, step=0.05, showRT=TRUE, initial=TRUE, ymax=0.8) {
  sub <- subset(data, sample==sample_name & source==source)
  
  ind1 <- which(names(data)=='1')
  ind2 <- ncol(data)
  n <- nrow(sub)
  print(sub)
  if (n > 1) {
    print('WARNING: Multiple spectra found for given sample. Showing first spectrum.')
  }
  
  y <- as.numeric(sub[1, ind1:ind2])
  x <- 1:length(y)
  if (showRT == TRUE) {
    x <- seq(from=rt0, to=(rt1-step), length.out=length(y))
    xl <- 'Retention time (min)'
  } else {
    xl <- 'Index'
  }
  
  if (is.null(xmax)) {
    xmax <- rt1
  }
  
  if (initial==TRUE) {
    plot(x, y, type='l', col=color, lwd=1, xlim=c(rt0, xmax), ylim=c(0,ymax), xlab=xl, ylab='Relative abundance', main=label)
  } else {
    lines(x, y, col=color)
  }
  
}


spectralDist <- function(data, n1, n2, rt1=55, rtMax=30, labelVar=NULL, kvals=c(1,2,4,8), plotSpectra=FALSE, fixed=TRUE, filters=c(), iters=10, verbose=TRUE) {
  ind1 <- which(names(data) %in% c('1', 'x1', 'X1', 'V1', 'v1'))
  ind2 <- as.integer(as.integer((ncol(data)-ind1)/rt1)*rtMax)
  
  if (is.null(labelVar)) {
    label1 <- NULL
    label2 <- NULL
  } else {
    label1 <- data[[labelVar]][n1]
    label2 <- data[[labelVar]][n2]
  }

  x1 <- as.numeric(data[n1, ind1:ncol(data)])
  x2 <- as.numeric(data[n2, ind1:ncol(data)])

  t <- 1:length(x1)
  
  if (plotSpectra == TRUE) {
    t <- 1:length(x1)
    par(mfrow=c(2,1), mar=c(3,3,2,0.5))
    plot(t, x1, type='l', lwd=2, col=rgb(0,0.6,0,0.8), xlab='Index', ylab='Relative abudance', main=sprintf('Spectrum %s (%s)', n1, label1), ylim=c(0,max(max(x1, na.rm=TRUE), max(x2, na.rm=TRUE))*1.1))
    plot(t, x2, type='l', lwd=2, col=rgb(0.2,0.2,0.5,0.8), xlab='Index', ylab='Relative abudance', main=sprintf('Spectrum %s (%s)', n2, label2), ylim=c(0,max(max(x1, na.rm=TRUE), max(x2, na.rm=TRUE))*1.1))
  }
  
  scores <- data.frame()
  for (k in kvals) {
    if (k > 1) {
      s1 <- c()
      s2 <- c()
      L <- length(x1)
      for (i in seq(1, L, k)) {
        vals1 <- x1[i:(i+(k-1))]
        vals2 <- x2[i:(i+(k-1))]
        if (length(vals1)==k & length(vals2) == k) {
          s1 <- append(s1, mean(vals1, na.rm=TRUE))
          s2 <- append(s2, mean(vals2, na.rm=TRUE))
        }
      }
    } else {
      s1 <- x1
      s2 <- x2
    }
    
    if (fixed==TRUE) {
      diff <- sum(abs(s1-s2))
    } else {
      subDists <- c()
      for (fL in filters) {
        fL <- as.integer(fL*length(s1))
        for (i in 1:iters) {
          start1 <- sample(1:(length(s1)-fL), 1)
          sub1 <- s1[start1:(start1+fL)]
          start2 <- sample(1:(length(s2)-fL), 1)
          sub2 <- s2[start2:(start2+fL)]
          subDists <- append(subDists, sum(abs(sub1-sub2)))
        }
      }
      diff <- mean(subDists)
    }
    
    row <- data.frame(n1=n1, n2=n2, k=k, difference=diff)
    scores <- rbind(scores, row)
  }
  
  if (verbose == TRUE) {
    print(sprintf('Spectral distance: %s', round(mean(scores$difference), 3)))
  }
  
  return(scores)
}

getDistMat <- function(data, fixed=TRUE, filters=c(), iters=10) {
  num_rows <- nrow(data)
  
  # Initialize an empty distance matrix
  distance_matrix <- matrix(0, nrow = num_rows, ncol = num_rows)
  pb <- txtProgressBar(min = 1, max = num_rows - 1, style = 3)
  
  # Iterate through the upper triangle of the matrix (excluding the diagonal)
  for (i in 1:(num_rows - 1)) {
    setTxtProgressBar(pb, i)
    for (j in (i + 1):num_rows) {
      # Calculate the spectral distance between row i and row j
      scores <- spectralDist(data = data, n1 = i, n2 = j, kvals = c(1, 2, 4, 8, 16, 32), plotSpectra = FALSE, fixed=fixed, filters=filters, iters=iters, verbose=FALSE)
      distance <- mean(scores$difference)
      
      # Fill in the upper triangle
      distance_matrix[i, j] <- distance
      
      # Reflect the value to the lower triangle
      distance_matrix[j, i] <- distance
    }
  }
  
  close(pb)
  return(distance_matrix)
}


plotSpectralGroups <- function(mdsData, variables = c(), s = 2, a = 0.7, show_legend = TRUE, selectTheme='light',
                               plot_borders = TRUE, axisTitleFont = 10, axisTitleMar = 10, axisLabelFont = 8, 
                               savePlot=FALSE, backgroundSet=TRUE) {
  
  themes <- list('dark'=c('grey40', 'grey90'),
                 'light'=c('#d4e3e5', '#e6f0f1'))
  
  theme <- themes[[selectTheme]]
  
  if (length(variables) > 0) {
    var1 <- variables[1]
    n_levels <- nlevels(as.factor(mdsData[[var1]]))
    if (n_levels == 2) {
      colList <- c('#86a9c4', 'salmon2')
    } else if (n_levels <= 8) {
      dark2_palette <- brewer.pal(8, "Dark2")
      colList <- sample(dark2_palette, size = n_levels, replace = FALSE)
    } else {
      colList <- c()
      for (n in 1:n_levels) {
        colList <- append(colList, rgb(runif(1, 0.3, 1), runif(1, 0.3, 1), runif(1, 0.3, 1), 1))
      }
    }
  }
  
  base_plot <- ggplot() +
    theme_minimal()
  
  if (length(variables) == 0) {
    p <- base_plot +
      geom_point(data = mdsData, aes(x = MDS1, y = MDS2), size = s, alpha = a)
  } else if (length(variables) == 1) {
    var1 <- variables[1]
    n_levels <- nlevels(as.factor(mdsData[[var1]]))
    mdsData <- mdsData[!is.na(mdsData[[var1]]), ]
    p <- base_plot +
      geom_point(data = mdsData, aes(x = MDS1, y = MDS2, color = .data[[var1]]), size = s, alpha = a) +
      scale_color_manual(values = colList)
  } else if (length(variables) == 2) {
    var1 <- variables[1]
    var2 <- variables[2]
    mdsData <- mdsData[(!is.na(mdsData[[var1]]) & !is.na(mdsData[[var2]])), ]
    p <- base_plot +
      geom_point(data = mdsData, aes(x = MDS1, y = MDS2, color = .data[[var1]]), size = s, alpha = a) +
      facet_grid(cols = vars(.data[[var2]])) +
      theme(
        axis.title.x = element_text(size = axisTitleFont, margin = margin(t = axisTitleMar, unit = "pt")),
        axis.title.y = element_text(size = axisTitleFont, margin = margin(r = axisTitleMar, unit = "pt")),
        axis.text.x = element_text(angle = 45, hjust = 1, size = axisLabelFont),
        axis.text.y = element_text(size = axisLabelFont),
        panel.border = element_rect(color = theme[1], fill = NA),
        strip.background = element_rect(fill = theme[2], color = theme[1]),
        strip.text = element_text(face = "bold")
      ) +
      scale_color_manual(values = colList)
  } else if (length(variables) == 3) {
    var1 <- variables[1]
    var2 <- variables[2]
    var3 <- variables[3]
    mdsData <- mdsData[(!is.na(mdsData[[var1]]) & !is.na(mdsData[[var2]]) & !is.na(mdsData[[var3]])), ]
    p <- base_plot +
      geom_point(data = mdsData, aes(x = MDS1, y = MDS2, color = .data[[var1]]), size = s, alpha = a) +
      facet_grid(cols = vars(.data[[var2]]), rows = vars(.data[[var3]])) +
      theme(
        axis.title.x = element_text(size = axisTitleFont, margin = margin(t = axisTitleMar, unit = "pt")),
        axis.title.y = element_text(size = axisTitleFont, margin = margin(r = axisTitleMar, unit = "pt")),
        axis.text.x = element_text(angle = 45, hjust = 1, size = axisLabelFont),
        axis.text.y = element_text(size = axisLabelFont),
        panel.border = element_rect(color = theme[1], fill = NA),
        strip.background = element_rect(fill = theme[2], color = theme[1]),
        strip.text = element_text(face = "bold")
      ) +
      scale_color_manual(values = colList)
  }
  
  if (!show_legend) {
    p <- p + theme(legend.position = "none")
  }
  
  if (plot_borders) {
    p <- p + theme(
      panel.border = element_rect(color = theme[1], fill = NA, linewidth = 0.5) # Add light grey border
    )
  } else {
    p <- p + theme(
      panel.border = element_blank() # Remove panel border
    )
  }
  
  if (backgroundSet) {
    p <- p +
      theme(
        panel.background = element_rect(fill = "white", colour = NA), # White background for the plotting area
        plot.background = element_rect(fill = "white", colour = NA)   # White background for the entire plot region
      )
  }

  if (savePlot) {
    current_date <- Sys.Date()
    current_datetime <- Sys.time()
    
    date_suffix <- format(current_date, "%d%m")
    datetime_string <- format(current_datetime, "%d%m-%H%M")
    folder_name <- paste0("figures", date_suffix)
    
    if (!dir.exists(folder_name)) {
      dir.create(folder_name)
    } else {
    }
    
    path <- sprintf('%s/P1-%s.png', folder_name, datetime_string)
    ggsave(path, plot = p, width = 7, height = 5, dpi = 400)
  }
  
  suppressWarnings(print(p))
  
  return(p)
}

getGroups <- function(data, vars) {
  # Remove rows with NA in any of the selected variables
  data_filtered <- data %>%
    filter(if_all(all_of(vars), ~ !is.na(.)))
  
  # Count occurrences of each combination
  combinations_with_counts <- data_filtered %>%
    count(across(all_of(vars)), name = "count") %>%
    mutate(id = row_number()) %>%
    relocate(id)
  
  print(combinations_with_counts)
  
  return(combinations_with_counts)
}

permTestGroups <- function(data, n1, n2, selectVars=c(), a=0.05, nTests=100) {
  if (length(selectVars) > 0) {
    var1 <- selectVars[1]
    G1 <- subset(data, data[[var1]]==as.character(groups[[var1]][n1]))
    if (length(selectVars) > 1) {
      for (var in selectVars[2:length(selectVars)]) {
        G1 <- subset(G1, G1[[var]]==as.character(groups[[var]][n1]))
      }
    }
  }
  
  if (length(selectVars) > 0) {
    var1 <- selectVars[1]
    G2 <- subset(data, data[[var1]]==as.character(groups[[var1]][n2]))
    if (length(selectVars) > 1) {
      for (var in selectVars[2:length(selectVars)]) {
        G2 <- subset(G2, G2[[var]]==as.character(groups[[var]][n2]))
      }
    }
  }
  

  if (length(selectVars) > 0) {
    if (length(selectVars) > 0) {
      var1 <- selectVars[1]
      G1 <- subset(data, data[[var1]]==as.character(groups[[var1]][n1]))
      if (length(selectVars) > 1) {
        for (var in selectVars[2:length(selectVars)]) {
          G1 <- subset(G1, G1[[var]]==as.character(groups[[var]][n1]))
        }
      }
    }
    
    if (length(selectVars) > 0) {
      var1 <- selectVars[1]
      G2 <- subset(data, data[[var1]]==as.character(groups[[var1]][n2]))
      if (length(selectVars) > 1) {
        for (var in selectVars[2:length(selectVars)]) {
          G2 <- subset(G2, G2[[var]]==as.character(groups[[var]][n2]))
        }
      }
    }
    
    G3 <- rbind(G1, G2)
    
    x1 <- mean(G1$MDS1)
    y1 <- mean(G1$MDS2)
    x2 <- mean(G2$MDS1)
    y2 <- mean(G2$MDS2)
    dist12 <- sqrt((x1-x2)**2 + (y1-y2)**2)
    
    dists <- c()
    for (i in 1:nTests) {
      R1 <- sample_n(G3, nrow(G1))
      R2 <- sample_n(G3, nrow(G2))
      x1 <- mean(R1$MDS1)
      y1 <- mean(R1$MDS2)
      x2 <- mean(R2$MDS1)
      y2 <- mean(R2$MDS2)
      dists <- append(dists, sqrt((x1-x2)**2 + (y1-y2)**2))
    }
    
    p <- sum(dists > dist12)/length(dists)
    
    if (p < a) {
      cat(sprintf('Permutation test (a = %s)\n\n\033[1mNull hypothesis\033[0m\nDistances between G1 and G2 spectra are not larger than between random spectra\n\n\033[1m\033[1mTest result\033[0m\np = %s (Sufficient evidence to reject null hypothesis)', a, p))
    } else {
      cat(sprintf('Permutation test (a = %s)\n\n\033[1mNull hypothesis\033[0m\nDistances between G1 and G2 spectra are not larger than between spectra\n\n\033[1m\033[1mTest result\033[0m\np = %s (Insufficent evidence to reject null hypothesis)', a, p))
    }
    
  } else {
    print('No variables selected.')
  }
  
  return(list(nullDist=dists, xTest=dist12, pval=p, g1=G1, g2=G2))
}

overlaySpectra <- function(data, variables=c(), subsetVar=NULL, subsetVarLevel=NULL, selectTheme='dark',
                           xScale=NULL, xLims=NULL, XYlabs=c('Index', 'Spectrum value'),
                           gridLines='both', plot_borders=TRUE, y_breaks=4,
                           axisTitleFont=12, axisTitleMar=10, 
                           axisLabelFont=10, groupLabelFont=12,
                           backgroundSet=TRUE, savePlot=FALSE) {
  
  themes <- list('dark'=c('grey40', 'grey90', 'grey92'),
                 'light'=c('#d4e3e5', '#e6f0f1', "grey92"))
  
  theme <- themes[[selectTheme]]
  
  ind1 <- which(names(data)=='1')
  
  if (!is.null(subsetVar)) {
    if (is.null(subsetVarLevel)) {
      subsetVarLevel <- 1
    }
    data <- subset(data, data[[subsetVar]]==unique(data[[subsetVar]])[subsetVarLevel])
    subset_text <- sprintf('Subset - %s', unique(data[[subsetVar]][subsetVarLevel]))
  } else {
    subset_text <- 'Subset - NONE'
  }

  nGroupVars <- length(variables)
  
  data_long <- data %>%
    pivot_longer(
      cols = names(data)[ind1]:names(data)[ncol(data)],
      names_to = "x",
      values_to = "y"
    ) %>% mutate(x = as.numeric(x))
  
  if (!is.null(xScale)) {
    original_min_x <- min(data_long$x)
    original_max_x <- max(data_long$x)
    data_long <- data_long %>%
      mutate(x = ( (x - original_min_x) / (original_max_x - original_min_x) ) * (xScale[2] - xScale[1]) + xScale[1])
  }
  
  data <- data.frame(data_long)
  var1 <- variables[1]
  var2 <- variables[2]
  var3 <- variables[1]
  for (var in variables) {
    data <- data[!is.na(data[[var]]),]
    data[[var]] <- as.factor(data[[var]])
  }
  
  if (length(variables) > 0) {
    n_levels <- nlevels(as.factor(data[[var1]]))
    if (n_levels == 2) {
      colList <- c('#86a9c4', 'salmon2')
    } else if (n_levels <= 8) {
      dark2_palette <- brewer.pal(8, "Dark2")
      colList <- sample(dark2_palette, size = n_levels, replace = FALSE)
    } else {
      colList <- c()
      for (n in 1:n_levels) {
        colList <- append(colList, rgb(runif(1,0.3,1), runif(1,0.3,1), runif(1,0.3,1), 1))
      }
    }
  } else {
    dark2_palette <- brewer.pal(8, "Dark2")
    colList <- sample(dark2_palette, size = 8, replace = FALSE)
  }
  
  if (length(variables) == 0) {
    p <- ggplot(data, aes(x=x, y=y)) + 
      geom_line() + 
      labs(
        title = element_text(subset_text),
        x = XYlabs[1],
        y = XYlabs[2],
      ) + 
      theme_bw() + 
      theme(
        axis.title.x = element_text(size = axisTitleFont, margin = margin(t = axisTitleMar, unit = "pt")),
        axis.title.y = element_text(size = axisTitleFont, margin = margin(r = axisTitleMar, unit = "pt")),
        axis.text.x = element_text(angle = 45, hjust = 1, size = axisLabelFont),
        axis.text.y = element_text(size = axisLabelFont),
        strip.text = element_text(size = groupLabelFont, face='bold'),
        strip.text.y = element_text(angle = 0, hjust = 0),
        strip.background = element_rect(fill = theme[2], color = theme[1]),
        panel.spacing = unit(1, "lines"),
        legend.position = "none" # Added legend position
      ) + 
      theme(strip.text = element_text(face = "bold")) + 
      scale_y_continuous(
        breaks = scales::breaks_pretty(n = y_breaks))
    
  } else if (length(variables)==1) {
    p <- ggplot(data, aes(x=x, y=y, group=as.factor(id))) + 
      geom_line() + 
      facet_grid(rows = vars(.data[[var1]])) + 
      labs(
        title = element_text(subset_text),
        x = XYlabs[1],
        y = XYlabs[2],
      ) + 
      theme_bw() + 
      theme(
        axis.title.x = element_text(size = axisTitleFont, margin = margin(t = axisTitleMar, unit = "pt")),
        axis.title.y = element_text(size = axisTitleFont, margin = margin(r = axisTitleMar, unit = "pt")),
        axis.text.x = element_text(angle = 45, hjust = 1, size = axisLabelFont),
        axis.text.y = element_text(size = axisLabelFont),
        strip.text = element_text(size = groupLabelFont, face='bold'),
        strip.text.y = element_text(angle = 0, hjust = 0),
        strip.background = element_rect(fill = theme[2], color = theme[1]),
        panel.spacing = unit(1, "lines"),
        legend.position = "none" # Added legend position
      ) + 
      theme(strip.text = element_text(face = "bold")) + 
      scale_y_continuous(
        breaks = scales::breaks_pretty(n = y_breaks)
      )
  } else {
    p <- ggplot(data, aes(x = x, y = y, group = as.factor(id), color = .data[[var2]])) + # Added 'group = id' and color
      geom_line() +
      facet_grid(cols = vars(.data[[var1]]), rows = vars(.data[[var2]])) + 
      labs(
        title = element_text(subset_text),
        x = XYlabs[1],
        y = XYlabs[2],
      ) +
      theme_minimal() +
      theme(
        axis.title.x = element_text(size = axisTitleFont, margin = margin(t = axisTitleMar, unit = "pt")),
        axis.title.y = element_text(size = axisTitleFont, margin = margin(r = axisTitleMar, unit = "pt")),
        axis.text.x = element_text(angle = 45, hjust = 1, size = axisLabelFont),
        axis.text.y = element_text(size = axisLabelFont),
        strip.text = element_text(size = groupLabelFont, face='bold'),
        strip.text.y = element_text(angle = 0, hjust = 0),
        strip.background = element_rect(fill = theme[2], color = theme[1]),
        panel.spacing = unit(1, "lines"),
        legend.position = "none" # Added legend position
      ) +
      theme(strip.text = element_text()) + 
      scale_color_manual(values = colList) + 
      scale_y_continuous(
        breaks = scales::breaks_pretty(n = y_breaks)
      )
  }
  
  if (!is.null(xLims)) {
    p <- p + xlim(xLims)
  }
  
  if (!is.null(gridLines)) {
    if (gridLines == 'h') {
      p <- p + theme(
        panel.grid.major.x = element_blank(), # Remove vertical major gridlines
        panel.grid.major.y = element_line(color = theme[3]), # Keep horizontal major gridlines
        panel.grid.minor.x = element_blank(), # Remove vertical minor gridlines
        panel.grid.minor.y = element_line(color = theme[3], linewidth = 0.25) # Add horizontal minor gridlines
      )
    } else if (gridLines == 'v') {
      p <- p + theme(
        panel.grid.major.x = element_line(color = theme[3]), # Keep vertical major gridlines
        panel.grid.major.y = element_blank(), # Remove horizontal major gridlines
        panel.grid.minor.x = element_line(color = theme[3], linewidth = 0.25), # Add vertical minor gridlines
        panel.grid.minor.y = element_blank() # Remove horizontal minor gridlines
      )
    } else if (gridLines == 'both') {
      p <- p + theme(
        panel.grid.major.x = element_line(color = theme[3]), # Keep vertical major gridlines
        panel.grid.major.y = element_line(color = theme[3]), # Keep horizontal major gridlines
        panel.grid.minor.x = element_line(color = theme[3], linewidth = 0.25), # Add vertical minor gridlines
        panel.grid.minor.y = element_line(color = theme[3], linewidth = 0.25) # Add horizontal minor gridlines
      )
    }
  } else { # If gridLines is NULL, remove all gridlines
    p <- p + theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()
    )
  }
  
  if (plot_borders) {
    p <- p + theme(
      panel.border = element_rect(color = theme[1], fill = NA, linewidth = 0.5) # Add light grey border
    )
  } else {
    p <- p + theme(
      panel.border = element_blank() # Remove panel border
    )
  }
  
  if (backgroundSet) {
    p <- p +
      theme(
        panel.background = element_rect(fill = "white", colour = NA), # White background for the plotting area
        plot.background = element_rect(fill = "white", colour = NA)   # White background for the entire plot region
      )
  }
  
  if (savePlot) {
    current_date <- Sys.Date()
    current_datetime <- Sys.time()
    
    date_suffix <- format(current_date, "%d%m")
    datetime_string <- format(current_datetime, "%d%m-%H%M")
    folder_name <- paste0("figures", date_suffix)
    
    if (!dir.exists(folder_name)) {
      dir.create(folder_name)
    } else {
    }
    
    path <- sprintf('%s/P2-%s.png', folder_name, datetime_string)
    ggsave(path, plot = p, width = 7, height = 5, dpi = 400)
  }
  
  
  suppressWarnings(print(p))
  return(p)
}

compressData <- function(data, k=1, sumNorm=TRUE) {
  if (k > 1) {
    compress <- function(x, k=1, sumNorm=TRUE) {
      new <- c()
      if (k > 1) {
        for (i in 1:length(x)) {
          if ((i-1)%%k == 0) {
            new <- append(new, mean(x[i:(i+k-1)], na.rm=TRUE))
          }
        }
      } else {
        new <- x
      }
      
      if (sumNorm==TRUE) {
        new <- new/sum(new)
      }
      return(new)
    }
    
    ind1 <- which(names(data)=='1')
    data_comp <- data.frame()
    for (i in 1:nrow(data)) {
      vals <- as.numeric(data[i, ind1:ncol(data)])
      vals_comp <- compress(vals, k=k, sumNorm=sumNorm)
      vals_comp <- t(data.frame(vals_comp))
      rownames(vals_comp) <- c(1)
      names(vals_comp) <- 1:ncol(vals_comp)
      row <- cbind(data[i, 1:(ind1-1)], vals_comp)
      data_comp <- rbind(data_comp, row)
      rownames(data_comp) <- 1:nrow(data_comp)
    }
    cat(sprintf('\nCompressed length of spectra from \033[1m%s\033[0m to \033[1m%s\033[0m', length(vals), ncol(vals_comp)))
  } else {
    data_comp <- data
  }

  return(data_comp)
}

downsampleData <- function(data, frac=1.0, vars=c()) {
  if (frac < 1) {
    rownames(data) <- 1:nrow(data)
    data$id <- 1:nrow(data)
    Nlim <- as.integer(frac*nrow(data))
    original <- nrow(data)
    
    count <- 0
    while (nrow(data) > Nlim) {
      print(nrow(data))
      count <- count + 1
      if (length(vars) == 0) {
        data <- sample_n(data, Nlim)
      } else if (length(vars) == 1) {
        var1 <- vars[1]
        cat1 <- names(table(data[[var1]])[rev(order(table(data[[var1]])))])[1]
        sub <- subset(data, data[[var1]]==cat1)
        if (nrow(sub) > 0) {
          sub <- sample_n(sub, 1)
          data <- subset(data, !id %in% sub$id)
        }
        
      } else if (length(vars) == 2) {
        var1 <- vars[1]
        var2 <- vars[2]
        cat1 <- names(table(data[[var1]])[rev(order(table(data[[var1]])))])[1]
        sub <- subset(data, data[[var1]]==cat1)
        cat2 <- names(table(sub[[var2]])[rev(order(table(sub[[var2]])))])[1]
        sub <- subset(sub, data[[var2]]==cat2)
        if (nrow(sub)>0) {
          sub <- sample_n(sub, 1)
          data <- subset(data, !id %in% sub$id)
        }
        
      } else if (length(vars) == 3) {
        var1 <- vars[1]
        var2 <- vars[2]
        var3 <- vars[3]
        cat1 <- names(table(data[[var1]])[rev(order(table(data[[var1]])))])[1]
        sub <- subset(data, data[[var1]]==cat1)
        cat2 <- names(table(sub[[var2]])[rev(order(table(sub[[var2]])))])[1]
        sub <- subset(sub, data[[var2]]==cat2)
        cat3 <- names(table(sub[[var3]])[rev(order(table(sub[[var3]])))])[1]
        sub <- subset(sub, data[[var3]]==cat3)
        if (nrow(sub)>0) {
          sub <- sample_n(sub, 1)
          data <- subset(data, !id %in% sub$id)
        }
        
      }
    } 
    
    cat(sprintf('\nDownsampled data from \033[1m%s\033[0m to \033[1m%s\033[0m samples', original, nrow(data)))
  }

  return(data)
}


downsampleData <- function(data, fraction = 1.0, variables = c()) {
  original <- nrow(data)
  if (fraction < 1) {
    if (!all(variables %in% names(data))) {
      stop(paste("One or more of the stratification variables '", paste(variables, collapse = ", "), "' are not found in the dataframe."))
    }
    
    data <- data %>%
      dplyr::group_by(!!!syms(variables)) %>%
      dplyr::slice_sample(prop = fraction) %>%
      dplyr::ungroup()
    
    cat(sprintf('\nDownsampled data from \033[1m%s\033[0m to \033[1m%s\033[0m samples', original, nrow(data)))
  }
  return(data)
}

exportResults <- function(data, dm, p1, p2, dims=c(7,5), DPI=400) {
  current_date <- Sys.Date()
  current_datetime <- Sys.time()
  
  datetime_string <- format(current_datetime, "%d%m-%H%M")
  folder_name <- paste0("output", datetime_string)
  
  if (!dir.exists(folder_name)) {
    dir.create(folder_name)
  } else {
  }
  
  if (!dir.exists(sprintf('%s/figures', folder_name))) {
    dir.create(sprintf('%s/figures', folder_name))
  }
  
  path <- sprintf('%s/P2-%s.png', folder_name, datetime_string)
  
  if (exists(data)) {
    write.csv(data, sprintf('%s/spectra.csv', folder_name), row.names = FALSE)
    cat(sprintf('Exported spectral data to %s/spectra.csv', folder_name))
  }
  if (exists(dm)) {
    write.csv(dm, sprintf('%s/distances.csv', folder_name), row.names = FALSE)
    cat(sprintf('Exported distance matrix to %s/distances.csv', folder_name))
  }
  if (exists(p1)) {
    ggsave(sprintf('%s/figures/P1.png', folder_name), plot = p1, width = dims[1], height = dims[2], dpi = DPI)
    cat(sprintf('Exported plot P1 to %s/figures/P1.png', folder_name))
  }
  if (exists(p2)) {
    ggsave(sprintf('%s/figures/P2.png', folder_name), plot = p2, width = dims[1], height = dims[2], dpi = DPI)
    cat(sprintf('Exported plot P2 to %s/figures/P2.png', folder_name))
  }
  
}