require(wavelets)
require(dplyr)


# Make lag col names for the supplied source column
make_wavelet_colnames <- function(sourceCol) {
  sapply(c("A1", "C1", "C2", "C3"), function(c) {
    lag_name <- paste0(sourceCol, "_", c)
    lag_name
  })
}

make_wavelet_source_colnames <- function(targets, windowSize) {
  colnames <- c()
  ignore <- sapply(targets, function(col) {
    cols <- make_wavelet_colnames(col)
    colnames <<- c(colnames, cols)
  })
  colnames
}

#create a wavelet decomposition for the supplied data.
wavelet_decomposition <- function(data, colName, wavelet="d4", K=3) {
  
  series <- data[,colName]
  
  w1 <- modwt(series, filter=wavelet, n.levels=K)
  
  A1 <- w1@V$V1
  
  c1 <- w1@W$W1
  c2 <- w1@W$W2
  c3 <- w1@W$W3
  
  col <- paste0(colName,"_A1")
  data[,col] <- A1
  col <- paste0(colName, "_C1")
  data[,col] <- c1
  col <- paste0(colName, "_C2")
  data[,col] <- c2
  col <- paste0(colName, "_C3")
  data[,col] <- c3
  data
}

# Perform a wavelet decomposition for each of the source columns
# for each group.
wavelet_decomposition_per_group <- function(data, groupCol, sourceCols) {
  data <- as.data.frame(data)
  unique_groups <- unique(data[,groupCol])
  subsets <- lapply(unique_groups, function(group) {
    idx <- which(data[,groupCol] == group)
    subset <- data[idx,]
    # now we need to process each column in the source columns.
    ignore <- sapply(sourceCols, function(col) {
      test <- wavelet_decomposition(subset, col)
      subset <<- test
    })
    return (subset)
  })
  new_data <- plyr::ldply(subsets)
  new_data
}