library(NLP)
library(openNLP)
library(openNLPmodels.en)

# Input x is a sentence string
verb.analyze <- function(x) {
  x <- as.String(x)
  sent_token_annotator <- Maxent_Sent_Token_Annotator()
  word_token_annotator <- Maxent_Word_Token_Annotator()
  pos_tag_annotator <- Maxent_POS_Tag_Annotator()
  a3 <- annotate(x, pos_tag_annotator, 
                 annotate(x, list(sent_token_annotator, word_token_annotator)))
  POS.vector <- unlist(a3$features[c(2:(length(a3$features)-1))], use.names=F)
}
