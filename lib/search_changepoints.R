require(dplyr)

# make 7 day lags for the data, datecolumn and columnname
makeLags <- function(data, date, colname, cols=c("t", "tm1", "tm2", "tm3", "tm4", "tm5", "tm6", "tm7")) {
  dateCol <- data[,date]
  col <- data[,colname]
  
  col[is.na(col)] <- 0
  
  totalLags <- length(cols) - 1
  
  # generate a lagged data series over 7 days per column.
  lagData <- data.frame(
    date = dateCol,
    t = col)
  
  ignore <- sapply(1:totalLags, function(k) {
    dateName <- paste0("datem",k)
    timeName <- paste0("tm",k)
    lagData[,dateName] <<- lag(dateCol, k)
    lagData[,timeName] <<- lag(col, k)
  })
  
  lagData <- lagData[complete.cases(lagData),]
  lagData
}

computeCorSteps <- function(lagData, totalVar, cols=c("t","tm1","tm2", "tm3", "tm4", "tm5", "tm6", "tm7")) {
  
  stepSize = length(cols)
  
  backward <- seq(1, (nrow(lagData)-stepSize), by=stepSize)
  forward <- seq(1+stepSize, nrow(lagData), by=stepSize)
  
  df <- lapply(1:length(backward), function(i) {
    prev <- backward[i]
    more <- forward[i]
    backDate <- lagData[prev,]$date
    forwardDate <- lagData[more,]$date
    
    set1 <- lagData[prev, cols]
    set1 <- set1[abs(set1) > 0]
    
    set2 <- lagData[more, cols]
    set2 <- set2[abs(set2) > 0]
    
    if (length(set1) < length(set2)) {
      set2 <- set2[1:length(set1)]
    } else if (length(set2) < length(set1)) {
      set1 <- set1[1:length(set2)]
    }
    
    c <- c(0)
    v <- c(0)
    mu <- c(0)
    
    varRatio <- c(0)
    ftest_pval <- c(1)
    ftest_reject <- c(FALSE)
    
    var1TotalRatio <- c(0)
    var2TotalRatio <- c(0)
    
    bartlett_pval <- c(1)
    bartlett_stat <- c(0)
    bartlett_reject <- c(FALSE)
    
    ttest_pval <- c(1)
    ttest_stat <- c(0)
    ttest_reject <- c(FALSE)
    
    if (length(set1) > 0 & length(set2) > 0) {
      c <- cor((set1), (set2))
      v <- var(set1, set2)
      mu <- mean(c(set1, set2))
      
      varRatio <- var(set1)/var(set2)
      
      df1 <- length(set1) - 1
      df2 <- length(set2) - 1
      ftest_pval <- 1 - pf(varRatio, df1, df2)
      ftest_reject <- ftest_pval < 0.05
      
      bartlett <- bartlett.test(list(set1, set2))
      bartlett_pval <- bartlett$p.value
      bartlett_stat <- bartlett$statistic
      bartlett_reject <- bartlett_pval < 0.05
      
      ttest <- t.test(set1, y=set2, alternative="g")
      ttest_pval <- ttest$p.value
      ttest_stat <- ttest$statistic
      ttest_reject <- ttest_pval < 0.05
    } 
    data.frame(
      date1=backDate,
      date2=forwardDate,
      cor=c[1],
      var=v[1],
      mu=mu[1],
      varRatio=varRatio[1],
      ftest_pval=ftest_pval[1],
      ftest_reject=ftest_reject[1],
      bartlett_pval=bartlett_pval[1],
      bartlett_stat=bartlett_stat[1],
      bartlett_reject=bartlett_reject[1],
      ttest_pval=ttest_pval[1],
      ttest_stat=ttest_stat[1],
      ttest_reject=ttest_reject[1]
    )
  })
  plyr::ldply(df)
}


# search for changepoints for a given application and column
search_changepoints_numeric <- function(data, idx, minXVal, xColumn, columnname, cols=c("t","tm1","tm2", "tm3", "tm4", "tm5", "tm6", "tm7")) {
  
  df <- data[idx,]
  
  df[,xColumn] <- df[,xColumn]
  
  idx <- which(df[,xColumn] >= minXVal)
  df <- df[idx,]
  
  if (nrow(df) == 0) {
    return (list())
  }
  
  lagged_data <- makeLags(df, xColumn, columnname, cols)
  
  totalVar <- var(df[,columnname])
  
  originalCor <- computeCorSteps(lagged_data, totalVar, cols)
  
  M <- as.matrix(lagged_data[,cols])
  
  pr <- princomp(scale(M))
  
  scoreDf <- data.frame(t=pr$scores[,1], date=lagged_data$date)
  
  totalVar <- var(scoreDf[,1])
  
  scoreLags <- makeLags(scoreDf, "date", "t", cols)
  
  corData <- computeCorSteps(scoreLags, totalVar)
  
  list(
    stepSize=length(cols),
    M=M,
    pr=pr,
    subset=originalCor,
    scoreDf=scoreDf,
    scoreLags=scoreLags,
    corData=corData
  )
}

min_max_scale <- function(series) {
  
  maxV <- max(series)
  minV <- min(series)
  
  v <- series
  
  scaleV <- (v - minV) / (maxV - minV)
  scaleV
}


# given the output of the search_changepoints
# plot the interesting dates for the function
# method selection includes ftest, bartlet or ttest
plot_interesting_changes <- function(data, xcol, ycol, result, appname, method="ftest") {
  
  idx1 <- which(result$corData$ftest_reject %in% TRUE)
  idx2 <- which(result$corData$bartlett_reject %in% TRUE)
  idx3 <- which(result$corData$ttest_reject %in% TRUE)
  
  if (method == "bartlett") {
    idx1 <- idx2
  } else if (method == "ttest") {
    idx1 <- idx3
  }
  data <- data[!is.na(data[,ycol]),]
  plot(data[,xcol], as.numeric(data[,ycol]), type="b", col="blue", xlab=xcol, ylab=ycol, main=paste("Changes in ", appname))
  
  minY <- min(data[,ycol])
  maxY <- max(data[,ycol])
  
  
  
  if (length(idx1) > 0) {
    for(i in idx1) {
      
      #browser()
      xid1 <- which(data[,xcol] %in% result$corData[i,]$date2)
      
      print(data[xid1,xcol])
      
      xid2 <- xid1 + result$stepSize
      
      if (xid2 > nrow(data)) {
        xid2 = nrow(data)
      }
      
      rect(data[xid1,xcol], minY, data[xid2,xcol], maxY, border="red", col=rgb(1.0,0,0,alpha=0.2))
    }
  }
  
}