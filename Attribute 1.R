analyze_document_metrics <- function(doc_name, file_mapping) {
  # Construct file path
  file_path <- paste0("C:/Users/paulh/Documents/TTRFleschSentiment/", file_mapping[[doc_name]])
  
  if (!file.exists(file_path)) {
    cat("Error: File not found:", file_path, "\n")
    return(NULL)
  }
  
  # Extract text
  text <- pdf_text(file_path) %>% paste(collapse = " ")
  
  # Tokenization
  tokens <- unlist(str_split(tolower(text), "\\W+"))
  unique_words <- length(unique(tokens))
  total_words <- length(tokens)
  ttr <- unique_words / total_words
  
  # Flesch Reading Ease Score
  total_sentences <- length(unlist(str_split(text, "[.!?]")))
  total_syllables <- sum(nchar(unlist(str_extract_all(text, "[aeiouy]+"))))
  flesch_score <- 206.835 - (1.015 * (total_words / total_sentences)) - (84.6 * (total_syllables / total_words))
  
  # Sentiment Analysis
  sentences <- unlist(str_split(text, "(?<=[.!?])\\s+"))
  sentence_sentiments <- lapply(sentences, vader_df)
  compound_scores <- sapply(sentence_sentiments, function(x) x$compound)
  avg_sentiment <- mean(compound_scores, na.rm = TRUE)
  
  # Return document-level metrics as a list
  return(list(
    total_words = total_words,
    ttr = ttr,
    flesch_score = flesch_score,
    avg_sentiment = avg_sentiment
  ))
}
