# x is a sentence from term_window_calculate.R
# output is a data frame of grammar
tree.to.df <- function(x) {
  x <- as.String(x)
  annotation = annotateString(x)
  parse_tree <- getParse(annotation)
  parse_tree <- gsub("[[:cntrl:]]", "", parse_tree)
  parse_tree <- gsub(" {1,}", " ", parse_tree)
  target.term.list <- c("future", "further", "will", "additional", "needed", "need", "needs", "remain", "remains")
  target.query <- list.to.query(target.term.list)
  if (length(parse_tree) > 1 & sum(grepl(target.query, tolower(parse_tree)) == T) == 1) {
    parse_tree <- parse_tree[grepl(target.query, tolower(parse_tree)) == T]
  } else {
    parse_tree <- parse_tree[which.max(nchar(parse_tree))]
  }
  char <- unlist(Tree_parse(parse_tree)$children)
  char.df <- data.frame(keyName=names(char),
                        value=char, 
                        children = "",
                        row.names=NULL, 
                        stringsAsFactors = F)
  for (i in 1:nrow(char.df)) {
    if (grepl("value", char.df$keyName[i]) == F) {
      char.df$children[i-1] <- char.df$value[i]
    }
    char.df$level[i] <- length(regmatches(char.df$keyName[i], 
                                          gregexpr("\\.", char.df$keyName[i]))[[1]])
  }
  char.df <- char.df[-which(grepl("value", char.df$keyName) == F),]
  char.df <- char.df[! char.df$value == ".",]
  char.df <- char.df[-1, -1]
  char.df$class <- ifelse(char.df$children == "", "phrase", "term")
  row.names(char.df) <- 1:nrow(char.df)
  char.df$row.no <- 1:nrow(char.df)
  
  char.df.new <- data.frame(phrase = character(0),
                            level = numeric(0),
                            term = character(0),
                            POS = character(0),
                            term.order = numeric(0))
  
  phrase.list <- which(char.df$class == "phrase")
  for (i in 1:length(phrase.list)) {
    char.df.1 <- char.df[char.df$row.no > phrase.list[i],]
    number = char.df.1$row.no[char.df.1$class == "phrase"][1]
    n = ifelse(is.na(number) == T,
               nrow(char.df.1),
               number - phrase.list[i] - 1)
    rep.number <- ifelse(n == 2, 2, 1)
    if (n == 0) {
      df <- data.frame(term = "", POS = "", term.order = "")
    } else {
      df <- data.frame(term = char.df$children[phrase.list[i] + seq(n)],
                       POS = char.df$value[phrase.list[i] + seq(n)],
                       term.order = rep("", n),
                       stringsAsFactors = F)
    }
    char.df.new <- rbind(char.df.new,
                         data.frame(phrase = rep(char.df$value[phrase.list[i]], rep.number),
                                    level = rep(char.df$level[phrase.list[i]], rep.number),
                                    df, stringsAsFactors = F))
  }
  m = 1
  loop.list = which(char.df.new$term != "")
  for (i in 1:length(loop.list)) {
    char.df.new$term.order <- as.character(char.df.new$term.order)
    char.df.new$term.order[loop.list[i]] = m
    m = m + 1
  }
  
  char.df.new$term.order <- as.numeric(char.df.new$term.order)
  char.df.new$term <- as.character(char.df.new$term)
  char.df.new$phrase <- as.character(char.df.new$phrase)
  char.df.new$level <- as.numeric(char.df.new$level)

  m = 2
  for (i in 1:nrow(char.df.new)) {
    if (char.df.new$phrase[1] == "NP") {
      if (i == 1) {
        char.df.new$group[i] = 1
      } else if (char.df.new$level[i] >= char.df.new$level[i-1]) {
        char.df.new$group[i] = char.df.new$group[i-1]
      } else if (char.df.new$level[i] == 1 & char.df.new$phrase[i] == "VP") {
        char.df.new$group[i] = 1
      } else if (char.df.new$term[i-1] != ",") {
        char.df.new$group[i] = m
        m = m + 1
      } else {
        char.df.new$group[i] = char.df.new$group[i-1]
      }
    } else if (char.df.new$phrase[1] != "NP") {
      if (i == 1) {
        char.df.new$group[i] = m
        m = m + 1
      } else if (char.df.new$level[i] == 1 & char.df.new$phrase[i] %in% c("NP", "VP")) {
        char.df.new$group[i] = 1
      } else if (char.df.new$level[i] >= char.df.new$level[i-1]) {
        char.df.new$group[i] = char.df.new$group[i-1]
      } else if (char.df.new$level[i] < char.df.new$level[i-1] & char.df.new$term[i-1] == ",") {
        char.df.new$group[i] = m
        m = m + 1
      } else {
        char.df.new$group[i] = char.df.new$group[i-1]
      }
    }
  }
  
  group.list <- unique(char.df.new$group)
  for (i in 1:length(group.list)) {
    group.no = as.numeric(group.list[i])
    if (group.no > 1 & min(which(char.df.new$group == group.no)) > 1) {
      term.order.min = min(which(char.df.new$group == group.no))
      group.pre = char.df.new$group[term.order.min-1]
      if (nrow(char.df.new[char.df.new$group == group.no,]) < 6) {
        char.df.new$group[char.df.new$group == group.no] = group.pre
      }
    }
  }
  
  char.df.new
}

