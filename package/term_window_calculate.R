# x is target.df
# y is the numeric vector of target terms

term.window.calculate <- function(x, y) {

  term.list <- as.character(x$term[! x$term == ""])
  
  # removed evidence and data
  positive.term.list <- c("research", "researches", "analysis", "analyses", 
                          "study", "studies", "work", "works", "direction", "directions", 
                          "effort", "efforts", "investigation", "investigations", 
                          "experiment", "experiments", "test", "tests", "testing",
                          "step", "steps", "understanding", "assessment",
                          "development", "developments", "elucidation", "verification")
  positive.verb.past.list <- c("determined", "examined", "decided", "investigated", 
                               "answered", "probed", "explored", "revisited", 
                               "analyzed", "elucidated", "defined", "tested",
                               "applied", "extended", "resolved", "settled")
  positive.verb.list <- c("determine", "examine", "decide", "investigate", 
                               "answere", "probe", "explore", "revisite", 
                               "analyze", "elucidate", "define", "test", 
                               "apply", "extend", "resolve", "settle", "consider")
  negative.term.list <- c("present", "past", "current", "previous")
  group.total = vector()
  text.all.vector = vector()
  window.text.vector <- vector()
  value.sum <- 0
  for (k in 1:length(y)) {
    if (term.list[y[k]] %in% term.list[y[-k]]) {
      p <- ifelse(k > max(which(term.list[y] == term.list[y[k]])[-k]),
                  2, 1)
      term.order = as.numeric(x$group[x$term == term.list[y[k]]])[p]
    } else {
      term.order = as.numeric(x$group[x$term == term.list[y[k]]])
    }
    # if (length(parse.df$term.order[parse.df$group == term.order]) < 4) {
    #   term.seq <- parse.df$term.order
    #   term.seq <- as.numeric(term.seq[! term.seq == ""])
    # } else {
    #   term.seq <- parse.df$term.order[parse.df$group == term.order]
    #   term.seq <- as.numeric(term.seq[! term.seq == ""])
    # }
    term.seq <- x$term.order
    term.seq <- as.numeric(term.seq[is.na(term.seq) != T])
    
    # TEST MODAL TERMS AND "IS NEEDED"
    tense.value.1 <- sum(grepl("VBD|VBN|Empty", sentence.verb.analyze(x)) == 0)
    tense.value.2 <- ifelse(sentence.verb.analyze(x, "B", term.order) == "Empty",
                            tense.value.1,
                            ifelse(grepl("VBD|VBN", sentence.verb.analyze(x, "B", term.order)),
                                   0, 1))
    if (term.list[y[k]] %in% c("future", "further", "additional")) {
      q <- ifelse(y[k] < max(term.seq) + 4, min(term.seq), y[k] - 3)
      n <- ifelse(y[k] > max(term.seq) - 4, max(term.seq), y[k] + 4)
      m <- ifelse(y[k] < min(term.seq) + 3, min(term.seq), y[k] - 2)
      o <- ifelse(y[k] > max(term.seq) - 8, max(term.seq), y[k] + 8)
      p <- ifelse(y[k] < min(term.seq) + 9, min(term.seq), y[k] - 8)
      
      window.text.1 <- term.list[m:y[k]]
      window.text.2 <- term.list[(y[k] + 1):n]
      window.text.3 <- term.list[q:y[k]]
      value.1 <- sum(window.text.2 %in% positive.term.list - window.text.2 %in% negative.term.list) - sum(window.text.3 %in% c("not"))
      value.2 <- ifelse(term.list[y[k]] %in% c("further", "additional"), (window.text.1 != "to"), 1)
      value.3 <- sum(term.list[m:y[k]] %in% c("in", "the")) * grepl("MD", sentence.verb.analyze(x)) * sum(term.list[o:p] %in% c(positive.term.list))
      if ((value.1 + value.3) * value.2* tense.value.2 > 0) {
        #group.total = c(group.total, x$group[which(x$term.order == y[k])])
        value.sum = value.sum + 1
      }
    }
    if (term.list[y[k]] %in% c("will", "would")) {
      n <- ifelse(y[k] > max(term.seq) - 4, max(term.seq), y[k] + 4)
      m <- ifelse(y[k] > max(term.seq) - 6, max(term.seq), y[k] + 6)
      o <- ifelse(y[k] < min(term.seq) + 7, min(term.seq), y[k] - 6)
      window.text.1 <- term.list[y[k]:n]
      window.text.2 <- term.list[m:o]
      value.1 <- sum(window.text.1 %in% c("be", "need", "require", "interesting", "interest", 
                                          "necessary", "important", "needed", "require", "required", "help",
                                          "enable", "enabled", "continued", "worthwhile"))
      value.2 <- sum(window.text.2 %in% c(positive.term.list, "data", "evidence", "evidences", 
                                          "approach", "approaches",
                                          positive.verb.past.list,
                                          positive.verb.list))
      if ((value.1 + value.2)* tense.value.2 > 2) {
        #group.total = c(group.total, x$group[which(x$term.order == y[k])])
        value.sum = value.sum + 1
      }
    }
    if (term.list[y[k]] %in% c("needed")) {
      m <- ifelse(y[k] < min(term.seq) + 7, min(term.seq), y[k] - 6)
      window.text.1 <- term.list[m:y[k]]
      value.1 <- sum(window.text.1 %in% c(positive.term.list, "data", "evidence", "evidences", 
                                          "approach", "approaches",
                                          positive.verb.past.list,
                                          positive.verb.list))
      if (value.1* tense.value.2 > 0) {
        #group.total = c(group.total, x$group[which(x$term.order == y[k])])
        value.sum = value.sum + 1
      }
    }
    if (term.list[y[k]] %in% c("remain", "remains", "need", "needs")) {
      n <- ifelse(y[k] > max(term.seq) - 3, max(term.seq), y[k] + 3)
      m <- ifelse(y[k] > max(term.seq) - 6, max(term.seq), y[k] + 6)
      o <- ifelse(y[k] < min(term.seq) + 7, min(term.seq), y[k] - 6)
      window.text.1 <- term.list[y[k]:n]
      window.text.2 <- term.list[m:o]
      value.1 <- sum(window.text.1 %in% c("to", "be"))
      value.2 <- sum(window.text.2 %in% c(positive.term.list, "data", "evidence", "evidences", 
                                          "approach", "approaches",
                                          positive.verb.past.list,
                                          positive.verb.list))
      if ((value.1 + value.2)* tense.value.2 > 2) {
        #group.total = c(group.total, x$group[which(x$term.order == y[k])])
        value.sum = value.sum + 1
      }
      
    }
    if (term.list[y[k]] %in% c("warrant", "warrants")) {
      m <- ifelse(y[k] > max(term.seq) - 6, max(term.seq), y[k] + 6)
      window.text.1 <- term.list[y[k]:m]
      value.1 <- sum(window.text.1 %in% positive.term.list)
      if (value.1* tense.value.2 > 0) {
        #group.total = c(group.total, x$group[which(x$term.order == y[k])])
        value.sum = value.sum + 1
      }
    }
    
  }
  
  if (value.sum > 0) {
    #group.all = as.numeric(unique(group.total))
    #text = paste(x$term[which(x$group %in% group.all)], collapse = " ")
    text = paste(x$term, collapse = " ")
    text.all.vector = c(text.all.vector, text)
  }
  
  text.all.vector
}
