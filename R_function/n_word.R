require(stringr)
nwords <- function(string, pseudo=F){
  ifelse( pseudo, 
          pattern <- "\\S+", 
          pattern <- "[[:alpha:]]+" 
  )
  string <- trimws(string)
  str_count(string, pattern)
}