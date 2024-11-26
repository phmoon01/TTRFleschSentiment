# Function to calculate document set features
calculate_document_set_features <- function(documents, file_mapping) {
  metrics_list <- list()
  
  for (category in names(documents)) {
    for (subcategory in documents[[category]]) {
      if (is.list(subcategory)) {
        for (doc_name in unlist(subcategory)) {
          metrics <- analyze_document_metrics(doc_name, file_mapping)
          if (!is.null(metrics)) {
            metrics_list[[doc_name]] <- metrics
          }
        }
      }
    }
  }
  
  # Combine metrics into a data frame
  metrics_df <- do.call(rbind, lapply(metrics_list, as.data.frame))
  
  # Calculate aggregated features
  document_set_features <- data.frame(
    avg_ttr = mean(metrics_df$ttr, na.rm = TRUE),
    var_ttr = var(metrics_df$ttr, na.rm = TRUE),
    avg_flesch_score = mean(metrics_df$flesch_score, na.rm = TRUE),
    var_flesch_score = var(metrics_df$flesch_score, na.rm = TRUE),
    avg_sentiment = mean(metrics_df$avg_sentiment, na.rm = TRUE),
    var_sentiment = var(metrics_df$avg_sentiment, na.rm = TRUE),
    total_word_count = sum(metrics_df$total_words, na.rm = TRUE),
    avg_word_count = mean(metrics_df$total_words, na.rm = TRUE),
    var_word_count = var(metrics_df$total_words, na.rm = TRUE)
  )
  
  return(document_set_features)
}
