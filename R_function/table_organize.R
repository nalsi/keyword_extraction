# Input x is the name of a summary table to be organized
# Output x is the organized table
table.organize = function(x) {
  x <- x[order(x$Freq, decreasing = T),]
  colnames(x) <- c("Category", "Frequency")
  rownames(x) <- 1:nrow(x)
  x
}