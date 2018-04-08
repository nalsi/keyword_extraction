# x is the name vector of a paper section
parse.section <- function(x) {
  text <- regmatches(html,
                     regexpr(paste("(?<=",
                                   sec.names[which(grepl(x, tolower(sec.names)) == T)],
                                   "\\<\\/h2\\>)(.*?)(?=\\<h2)",
                                   sep = ""),
                             html,
                             perl = T))
}