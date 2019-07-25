alphabet <- c("A", "B", "C", "D", "E", "F", "G", "H", "I", "J", "K", "L", "M", "N", "O", "P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z", "1", "2", "3", "4", "5", "6", "7", "8", "9", "_")

find_char <- function(row, column) {
  m = matrix(alphabet, nrow=6, ncol=6, byrow = TRUE)
  return(m[row, column-6])
}

translate_to_char <- function(c, y) { 
  indices <- c[which(y %in% 1)]
  return(find_char(if(!is.null(indices) & length(indices) > 0) if (indices[1] < 7) indices[1] else 1 else 1, if(!is.null(indices) & length(indices) > 0) if (indices[length(indices)] > 6) indices[length(indices)] else 7 else 7))
}

best_char <- function(c, y) { 
  results <- rep(0, times = length(alphabet))
  names(results) <- alphabet
  for (i in 0:9) {
    found_char <- translate_to_char(c[i*12+1:12], y[i*12+1:12])
    results[found_char] <- results[found_char]+1
  }
  return(alphabet[which.max(results)])
}

translate_to_word <- function(c, y) {
  result <- list()
  for (i in 0:(round(length(c)/120) - 1)) {
    found_char <- best_char(c[i*120+1:120], y[i*120+1:120])
    result <- append(result, found_char)
  }
  return(paste(result, collapse=''))
}

find_best_pair <- function(y) {
  result <- rep(c(-1), times = 12)
  result[order(y[1:6])[1]] <- 1
  result[6+order(y[7:12])[1]] <- 1
  return(result)
}

find_best_pairs <- function(y) {
  result <- c()
  for (i in 0:(floor(length(y)/12)-1)) {
    result <- append(result, find_best_pair(y[i*12+1:12]))
  }
  return(result)
}

accuracy_by_char <- function(y, y_pred) {
  y <- strsplit(y, "")[[1]]
  y_pred <- strsplit(y_pred, "")[[1]]
  counter <- 0
  for (i in 1:length(y)) {
    if (!identical(y[i], y_pred[i])) {
      counter <- counter+1
    }
  }
  return(1 - (counter/length(y)))
}

accuracy_by_row_column <- function(y, y_pred) {
  res <- y - y_pred
  return(1- length(res[res>0])/(length(y)/6))
}


