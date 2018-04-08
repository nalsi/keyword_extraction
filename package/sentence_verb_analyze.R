sentence.verb.analyze <- function(x, y, z) {
  
  x$phrase <- as.character(x$phrase)
  x$POS <- as.character(x$POS)
  
  # warnings()
  parse.df.1 <- x[x$phrase == "VP",]
  min.number = which(parse.df.1$level == min(parse.df.1$level))[1]
  min.row = as.numeric(row.names(parse.df.1)[min.number])
  
  if (missing(y)) {
    if (length(which(x$phrase == "VP")) == 0) {
      verb.vector <- "Empty"
    } else if (x$term[min.row] == "" & x$term[min.row + 1] == "") {
      verb.vector <- "Empty"
    } else {
      term.1 <- ifelse(x$term[min.row] %in% c("is", "are"), 
                       "VB",
                       x$POS[min.row])
      term.2 <- ifelse(x$term[min.row] %in% c("is", "are"), 
                       "VB",
                       ifelse(x$phrase[min.row + 1] == "VP",
                              x$POS[min.row + 1],
                              NA))
      verb.vector <- c(term.1, term.2)
    }
  } else {
    if (y == "B" & length(which(x$phrase == "VP" & x$group == z)) == 0) {
      verb.vector <- "Empty"
    } else {
      parse.df.1 <- x[x$phrase == "VP" & x$group == z,]
      min.number = which(parse.df.1$level == min(parse.df.1$level))
      min.row = as.numeric(row.names(parse.df.1)[min.number])
      term.1 <- ifelse(x$term[min.row] %in% c("is", "are"), 
                       "VB",
                       x$POS[min.row])
      term.2 <- ifelse(x$term[min.row] %in% c("is", "are"), 
                       "VB",
                       ifelse(x$phrase[min.row + 1] == "VP",
                              x$POS[min.row + 1],
                              NA))
      verb.vector <- c(term.1, term.2)
    }
  }
  verb.vector <- paste(verb.vector, collapse = " ")
}

