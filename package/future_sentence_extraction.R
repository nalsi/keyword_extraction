# x is the text vector of the future section/conclusion/discussion section
future.sentence.extraction <- function(x) {
  x = gsubfn("; [[:alpha:]]{1}", toupper, x)
  x = gsub("\\;", "\\.", x)
  txt.future <- tokenize_sentences(trimws(x), lowercase = F)[[1]]
  future.df <- data.frame(sentence = character(0))
  txt.future <- gsub("further-more|Further-more", "furthermore", txt.future)
  txt.future <- gsub("(?<=[[:alpha:]])-(?=[[:alpha:]])", " ", txt.future, perl = T)
  target.term.list <- c("future", "further", "will", "additional", "needed", "need", "needs", "remain", "remains", "warrant", "warrants", "would")
  target.term.list.1 <- c("future", "further", "will", "additional", "needed", "need", "needs", "remain", "remains", "warrant", "warrants", "would be interesting")
  target.query <- list.to.query(target.term.list.1)
  sentence.vector <- grepl(target.query, tolower(txt.future)) == T & str_count(txt.future,'\\w+') > 6
  # verb.vector.df <- data.frame(sentence.id = numeric(0),
  #                              verb = character(0))
  if (sum(sentence.vector) == 0) {
    future.df <- future.df
  } else {
    for (j in 1:length(which(sentence.vector == T))) {
      sentence <- gsub("et al\\.(?<!$)", "and others", 
                       txt.future[which(sentence.vector)[j]], 
                       perl = T)
      sentence <- gsub("e\\.g\\.", "for example", 
                       sentence, 
                       perl = T)
      parse.df <- tree.to.df(sentence)
      parse.df$term <- tolower(parse.df$term)
      target <- as.numeric(parse.df$term.order[parse.df$term %in% target.term.list])
      window.text.vector <- term.window.calculate(parse.df, target)
      
      if (length(window.text.vector) > 0) {
        
        if (nwords(window.text.vector) < 16 & sum(grepl("this|This|These|these", window.text.vector)) > 0) {
          window.text.vector <- c(window.text.vector,
                                  txt.future[which(sentence.vector)[j] - 1])
        }
        window.text.vector <- paste(window.text.vector, collapse = ". ")
        future.df <- rbind(future.df,
                           data.frame(sentence = as.character(trimws(window.text.vector)),
                                      stringsAsFactors = F))
      }
    }
    future.df <- future.df
  }
}
