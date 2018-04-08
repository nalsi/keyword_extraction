# Input is a text vector of selected HTML code
text.cleaning <- function(x) {
  article.sec.text.remove <- gsub("\\<.*?\\>",
                                  "",
                                  x,
                                  perl = T)
  article.sec.text.remove <- gsub("\\(.*?\\)",
                                  "",
                                  article.sec.text.remove,
                                  perl = T)
  article.sec.text.remove <- gsub("\\[.*?\\]",
                                  "",
                                  article.sec.text.remove,
                                  perl = T)
}