list.to.query <- function(x) {
  x <- paste("( |^|[[:punct:]])",
            paste(x,
                  collapse = "( |$|[[:punct:]])|( |^|[[:punct:]])"),
            "( |$|[[:punct:]])",
            sep = "")
}