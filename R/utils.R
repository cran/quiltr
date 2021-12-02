pbreak <- function(input_data, page_break_every) {

  nformrow <- nrow(input_data)

  #get row IDs
  input_data$rowID <- 1:nformrow

  #get break points as decimals
  pbreaknums <- seq(from = page_break_every, to = nformrow,
                    by = page_break_every) + .1

  #add rows for insertion
  input_data[(nformrow + 1):(nformrow + length(pbreaknums)), ] <- ""

  #add page break rows as dummy questions
  input_data$prompt[(nformrow + 1):(nformrow + length(pbreaknums))] <- "[[PageBreak]]\n"

  #assign page break rows decimal row ID
  input_data$rowID[(nformrow + 1):(nformrow + length(pbreaknums))] <- pbreaknums

  #convert character to numeric for ordering
  input_data$rowID <- as.numeric(input_data$rowID)

  #shuffle data into row ID order
  input_data <- input_data[order(input_data$rowID), ]

  return(input_data)

}

formpaste <- function(input_data, question_type) {

  input_data$response_type <- gsub(";", "\n", input_data$response_type)

  input_data$id[which(input_data$id != "")] <-
    paste0(question_type, "\n", input_data$id[which(input_data$id != "")], "\n", sep = "")

  input_data$response_type[which(input_data$response_type != "")] <-
    paste0("\n", "[[Choices]]", "\n", input_data$response_type[which(input_data$response_type != "")], "\n", sep = "")

  quilted_form <- paste0(input_data$id, input_data$prompt, input_data$response_type)

  quilted_form[1] <- paste("[[AdvancedFormat]]\n", quilted_form[1], sep = "\n")

  return(quilted_form)
}

addprompt <- function(prompt, text) {

  if(is.null(prompt)){
    prompt = ""
    df = data.frame(prompt = paste0(rep(prompt, length(text)), text))
  } else {
    df = data.frame(prompt = paste(rep(prompt, "\n", length(text)), text))
  }

  return(df)
}


addformIDs <- function(input_data) {
  if (!(is.null(input_data$id))) {
    input_data$id <- paste("[[ID:",
                           input_data$id,
                           "]]", sep = "")
  } else {
    input_data$id <-
      paste("[[ID:", sub("\\s", "_", rep(1:nrow(input_data))), "]]", sep = "")
  }

  return(input_data)
}
