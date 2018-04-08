library(readr)
library(XML)
source("file_download_1.R")
source("../R_function/n_word.R")

#### Step 1: from XML to data.frame

file <- read_file("pmc_result (4).xml")
xml <- regmatches(file, gregexpr("<article .*?<\\/article>", file))[[1]]
text.final <- data.frame(article.title = character(),
                         article.abstract = character(),
                         article.subject = character(),
                         article.type = character(),
                         publication.year = numeric(),
                         future = character(),
                         conclusion = character(),
                         discussion = character(),
                         text = character(),
                         stringsAsFactors = F)

for (i in 1:length(xml)) {
  text.df <- file.download(xml[i])
  text.final <- rbind(text.final,
                      text.df)
}

write.csv(text.final, "data.csv", row.names = F)

#### Step 2: from data frame to text

options(java.parameters = "-Xmx4096m")
library(coreNLP)
library(tokenizers)
library(stringr)
library(NLP)
library(xlsx)
library(gsubfn)
library(slowraker)
initCoreNLP()

source("file_download_1.R")
source("future_sentence_extraction.R")
source("term_window_calculate.R")
source("list_to_query.R")
source("tree_to_df.R")
source("sentence_verb_analyze.R")
source("../R_function/n_word.R")
source("../R_function/table_organize.R")

data <- read.csv("data.csv", stringsAsFactors = F)
data.paper <- data[data$article.type == "Research Article",]
data.paper$id <- 1:nrow(data.paper)

future.df.total <- data.frame(sentence = character(0),
                              article.id = character(0))

# set.seed(2)
# sample.list <- sample(nrow(data.paper), 100)

# nrow(data.paper)
# length(sample.list)
for (i in 1:nrow(data.paper)) {
  future.df.1 <- future.sentence.extraction(data.paper$text[i]) #i
  if (nrow(future.df.1) > 0) {
    future.df.1 <- data.frame(future.df.1,
                            article.id = i, # i
                            stringsAsFactors = F)
    future.df.total <- rbind(future.df.total,
                             future.df.1)
  }
}

# data.paper.sample <- data.paper[data.paper$id %in% sample.list,]
# future.df.sample <- future.df.total[future.df.total$article.id %in% sample.list,]

data.paper.1 <- data.paper[, c(10, 1, 2, 3, 5)]
data.paper.1$tit.abs <- paste(data.paper.1$article.title,
                              data.paper.1$article.abstract,
                              sep = ". ")

for (i in 1:nrow(data.paper.1)) {
  future.sub <- future.df.total[future.df.total$article.id == i,]
  data.paper.1$future.statement[i] <- paste(future.sub$sentence, collapse = ". ")
}

data.paper.2 <- data.paper.1[data.paper.1$future.statement != "",]
dp.2.subject <- data.frame(table(data.paper.2$article.subject))

write.xlsx(data.paper.1, "paper_sample.xlsx", sheetName = "Sheet1")
write.xlsx(future.df.total, "sentence_sample.xlsx", sheetName = "Sheet1")

nrow(data.paper.1[data.paper.1$future.statement != "",])
sci.adv.data = data.paper.1[data.paper.1$future.statement != "",]

write.csv(sci.adv.data,
          "sci.adv.data.csv",
          row.names = F)

#### RAKE extraction

library(slowraker)
library(xlsx)
source("../R_function/n_word.R")

data <- read.csv("final_data.csv", stringsAsFactors = F)
data <- data[is.na(data$future.statement) == F,]
data$tit.abs <- gsub(" {2,}", " ", data$tit.abs)
data$future.statement <- gsub(" {2,}", " ", data$future.statement)
data$analysis.text = paste(data$tit.abs, data$future.statement, sep = " ")

stopword.list <- c("future", "further", "additional", 
                   "research", "researches", "researcher", 
                   "researchers", "analysis", "analyses", 
                   "study", "studies", "work", "works", 
                   "direction", "directions", "efforts")
rakelist <- slowrake(txt = data$analysis.text,
                     stop_words = c(c(stopword.list, "future", "further, additional"), smart_words),
                     stop_pos = pos_tags$tag[!grepl("^N", pos_tags$tag)],
                     phrase_delims = '\\[|\\-|\\,|\\.|\\?|\\(|\\)|\\:|\\;|\\"|\\!|\\]|\\--|\\“|\\”|\\‘|\\’|\\{|\\}')
result.future <- rbind_rakelist(rakelist, doc_id = data$id)

write.csv(result.future, "rake_result.csv", row.names = F)