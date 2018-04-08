library(stringr)
library(RCurl)
library(XML)
source("text_cleaning.R")
source("html_preprocessing.R")

# Input: a name of the file from the folders
# Output: a data frame of metadata and data about the file
file.download <- function(x) {
  
  xml_file <- xmlParse(x)
  rootnode <- xmlRoot(xml_file)
  article.title <- xpathSApply(rootnode, ".//title-group/article-title", xmlValue)
  article.type <- xpathSApply(rootnode, ".//subj-group[@subj-group-type = 'article-type']/subject", xmlValue)
  if (length(xpathSApply(rootnode, 
                         ".//subj-group[@subj-group-type = 'overline']/subject", 
                         xmlValue)) == 0) {
    article.subject = ""
  } else {
    article.subject = xpathSApply(rootnode, 
                                  ".//subj-group[@subj-group-type = 'overline']/subject", 
                                  xmlValue)
  }
  article.subject = paste(article.subject, collapse = "; ")
  article.abstract <- xpathSApply(rootnode, ".//abstract[not(@*)]", xmlValue)
  publication.year <- ifelse(length(xpathSApply(rootnode, 
                                         ".//date[@date-type= 'accepted']/year", 
                                         xmlValue)) == 0,
                             "",
                             xpathSApply(rootnode, 
                                         ".//date[@date-type= 'accepted']/year", 
                                         xmlValue))
  
  sec.names <- xpathSApply(rootnode, ".//body/sec/title", xmlValue)
  future.list <- "future|outlook"
  conclusion.list <- "conclusion|conclusive|concluding"
  discussion.list <- "discussion"
  all.list <- paste(future.list, conclusion.list, discussion.list, sep = "|")
  
  parse.section <- function(x) {
    text.all = vector()
    sec.query <- sec.names[which(grepl(x, tolower(sec.names)) == T)]
    for (x in 1:length(sec.query)) {
      text <- as.character(xpathSApply(rootnode, 
                                       paste(".//body/sec[title = '",
                                             sec.query[x], 
                                             "']/sec/p | .//body/sec[title = '",
                                             sec.query[x], 
                                             "']/p",
                                             sep = ""), 
                                       xmlValue))
      text.all <- c(text.all, paste(text, collapse = " "))
    }
    text.all <- paste(text.all, collapse = " ")
  }
  
  future.text <- ifelse(sum(grepl(future.list, tolower(sec.names))) > 0,
                        "Y", "N")
  conclusion.text <- ifelse(sum(grepl(conclusion.list, tolower(sec.names))) > 0,
                            "Y", "N")
  discussion.text <- ifelse(sum(grepl(discussion.list, tolower(sec.names))) > 0,
                            "Y", "N")
  extract.text <- ifelse(sum(grepl(all.list, tolower(sec.names))) > 0,
                            parse.section(all.list),
                            "")
  extract.text.clean <- ifelse(length(text.cleaning(extract.text)) == 0,
                               "",
                               text.cleaning(extract.text))
  
  article.abstract <- ifelse(length(article.abstract) == 0, " ", article.abstract)
 
  text.df <- data.frame(article.title = article.title,
                        article.abstract = article.abstract,
                        article.subject = article.subject,
                        article.type = article.type,
                        publication.year = as.numeric(publication.year),
                        future = future.text,
                        conclusion = conclusion.text,
                        discussion = discussion.text,
                        text = extract.text.clean,
                        stringsAsFactors = F)
  
}
