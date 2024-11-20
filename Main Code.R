library(pdftools)
library(dplyr)
library(stringr)
library(tidytext)
library(quanteda)
library(quanteda.textstats)
library(vader)

# Define file mapping: Maps display names to actual file names
file_mapping <- list(
  # Financial Statements -> Financial Company
  "Citi 2022 Annual Report" = "citi-2022-annual-report.pdf",
  "Citi 2Q24 10-Q" = "Citi-2Q24-10-Q.pdf",
  "Citi 2023 Annual Report" = "Citi-2023-Annual-Report.pdf",
  "2022 Form 10-K" = "2022-10-k.pdf",
  "2021 Form 10-K" = "2021-10-k.pdf",
  "2023 Form 10-K" = "2023-10-k.pdf",
  "2023 Form 10-K Amendment" = "2023-10-k-a.pdf",
  "Morgan Stanley 2022 Form 10-K" = "Morgan_Stanley_2022_Form_10-K.pdf",
  "Morgan Stanley Q1 2024 10-Q" = "10q2024.pdf",
  "Morgan Stanley 2023 Form 10-K" = "MS10k2023.pdf",
  "Wells Fargo 2023 Form 10-K" = "10k2023.pdf",
  "Wells Fargo 2022 Form 10-K" = "10k2022.pdf",
  # Financial Statements -> Biomedical Company
  "2018 Biogen Annual Report" = "FINAL_BIIB_Annual Report-full.pdf",
  "2019 Biogen Annual Report" = "Biogen - 2019 Annual Report - Final.PDF",
  "2020 Biogen Annual Report" = "Biogen - 2020 Annual Report - final - 04.09.2021(432755.1).pdf",
  "2021 Biogen Annual Report" = "Biogen 2021 Annual Report.pdf",
  "2022 Biogen Annual Report" = "Final Biogen Annual Report 2022_Web Print Version.pdf",
  "2023 Biogen Annual Report" = "2023 Biogen Annual Report_Final to Upload COLOR.pdf",
  "Q1 2024 Earnings PR" = "Q1-2024-Earnings-PR_050124_Final.pdf",
  "Moderna 2023 Digital Annual Report" = "MRNA008_Moderna_2023-Digital-Annual-Report_Bookmarked-1.pdf",
  "Moderna 2022 Annual Report" = "Moderna-2022-Annual-ReportvF.pdf",
  "Moderna Reports Third Quarter 2023 Financial Results" = "Moderna-Reports-Third-Quarter-2023-Financial-Results-and-Provides-Business-Updates---2023.pdf",
  "Moderna 2021 Annual Report" = "Moderna-2021-Annual-Report.pdf",
  "Moderna 2020 Annual Report" = "Chasen-Richter-Moderna-Annual-Report-2020.pdf",
  "Q4 2023 PR Final" = "Q4-23-PR_Final.pdf",
  "Q2 2024 Earnings PR" = "q2-2024-earnings-pr_080124_final.pdf",
  "Pfizer 2022 Form 10-K" = "PFE-2022-Form-10K-FINAL-(without-Exhibits).pdf",
  "Pfizer 2021 Form 10-K" = "PFE-2021-Form-10K-FINAL.pdf",
  "Pfizer 2020 Form 10-K" = "PFE-2020-Form-10K-FINAL.pdf",
  "Pfizer 2023 Form 10-K" = "2023-10k.pdf",
  "Pfizer 2018 Financial Report" = "2018-Financial-Report.pdf",
  "Pfizer 2019 Financial Report" = "Pfizer-2019-Financial-Report.pdf",
  # Financial Statements -> Tech&Motor Company
  "GOOG 10-Q Q2 2024" = "goog-10-q-q2-2024.pdf",
  "GOOG 10-Q Q1 2024" = "goog-10-q-q1-2024.pdf",
  "FY24 Q2 Consolidated Financial Statements" = "FY24_Q2_Consolidated_Financial_Statements.pdf",
  "FY24 Q1 Consolidated Financial Statements" = "FY24_Q1_Consolidated_Financial_Statements.pdf",
  "FY24 Q3 Consolidated Financial Statements" = "FY24_Q3_Consolidated_Financial_Statements.pdf",
  "Ford 2023 Annual Report" = "2023-Ford-Annual-Report.pdf",
  "Ford 2019 Annual Report" = "Ford-2019-Printed-Annual-Report.pdf",
  "Ford 2022 Annual Report" = "2022-Annual-Report-1.pdf",
  "Ford 2018 Annual Report" = "2018-Annual-Report.pdf",
  "Ford 2021 Annual Report" = "Ford-2021-Annual-Report.pdf",
  "Ford 2020 Annual Report" = "Ford-2020-Annual-Report-April-2020.pdf",
  "GM Releases 2023 Fourth-Quarter and Full-Year Results" = "GM Releases 2023 Fourth-Quarter and Full-Year Results, and 2024 Guidance _ General Motors Company.pdf",
  "GM Releases 2023 Second-Quarter Results" = "GM Releases 2023 Second-Quarter Results and Raises Full-Year Earnings Guidance _ General Motors Company.pdf",
  "GM Releases 2023 First-Quarter Results" = "GM Releases 2023 First-Quarter Results and Raises Full-Year Guidance _ General Motors Company.pdf",
  "GM Releases 2022 Fourth-Quarter and Full-Year Results" = "GM Releases 2022 Fourth-Quarter and Full-Year Results, and 2023 Guidance _ General Motors Company.pdf",
  "GM Releases 2023 Third-Quarter Results" = "GM Releases 2023 Third Quarter Results _ General Motors Company.pdf",
  "GM Releases 2024 First-Quarter Results" = "GM Releases 2024 First-Quarter Results and Raises Full-Year Guidance _ General Motors Company.pdf",
  "GM Reports Second-Quarter 2022 Results" = "GM Reports Second-Quarter 2022 Results _ General Motors Company.pdf", 
  "GM Reports First-Quarter 2022 Results" = "GM Reports First-Quarter 2022 Results _ General Motors Company.pdf",
  "GM Releases 2024 Second-Quarter Results" = "GM Releases 2024 Second-Quarter Results and Raises Full-Year Guidance _ General Motors Company.pdf",
  "GM Reports 2021 Full-Year and Fourth-Quarter Results" = "GM Reports 2021 Full-Year and Fourth-Quarter Results, Including Record Earnings _ General Motors Company.pdf",
  "GM Reports Third-Quarter 2022 Results" = "GM Reports Third-Quarter 2022 Results _ General Motors Company.pdf",
  "Evaluating Electric Vehicle Policy Effectiveness and Equity" = "annurev-resource-111820-022834.pdf",
  "Alternative Fuels Data Center: Electricity Laws and Incentives in California" = "Alternative Fuels Data Center_ Electricity Laws and Incentives in California.pdf",
  "Tesla 2020 Q1 Update" = "TSLA_Update_Letter_2020-1Q.pdf",
  "Tesla 2020 Q2 Update" = "TSLA_Update_Letter_2020-2Q.pdf",
  "Tesla 2021 Q1 Update" = "TSLA-Q1-2021-Update.pdf",
  "Tesla 2021 Q2 Update" = "TSLA-Q2-2021-Update.pdf",
  "Tesla 2020 Q3 Update" = "TSLA-Q3-2020-Update.pdf",
  "Tesla 2020 Q4 Update" = "TSLA-Q4-2020-Update.pdf",
  "Tesla 2021 Q3 Update" = "TSLA-Q3-2021-Quarterly-Update.pdf",
  "Tesla 2021 Q4 Update" = "TSLA-Q4-2021-Update.pdf",
  "Tesla 2022 Q1 Update" = "TSLA-Q1-2022-Update.pdf",
  "Tesla 2022 Q2 Update" = "TSLA-Q2-2022-Update.pdf",
  "Tesla 2022 Q3 Update" = "TSLA-Q3-2022-Update.pdf",
  "Tesla 2022 Q4 Update" = "TSLA-Q4-2022-Update.pdf",
  "Tesla 2023 Q1 Update" = "TSLA-Q1-2023-Update.pdf",
  "Tesla 2023 Q2 Update" = "TSLA-Q2-2023-Update.pdf",
  "Tesla 2023 Q3 Update" = "TSLA-Q3-2023-Update-3.pdf",
  "Tesla 2023 Q4 Update" = "TSLA-Q4-2023-Update.pdf",
  "Tesla 2024 Q1 Update" = "TSLA-Q1-2024-Update.pdf",
  "Tesla 2024 Q2 Update" = "TSLA-Q2-2024-Update.pdf",
  "2024-25 NBA Regular Season Schedule" = "2024-25-NBA-Regular-Season-Schedule-By-Team.pdf",
  "2023 NBA Collective Bargaining Agreement" = "2023-NBA-Collective-Bargaining-Agreement.pdf",
  "2023-24 NBA Rule Book" = "2023-24-NBA-Season-Official-Playing-Rules.pdf",
  "2022-23 NBA Rule Book" = "2022-2023-NBA-RULE-BOOK.pdf",
  "2021-22 NBA Rule Book" = "2021-22-NBA-Rule-Book.pdf",
  "2021 Hall of Fame Financial Statements" = "2021_NMBHOF_Financial_Statements_Parent_-_Signed_2.pdf",
  "Scikit-Learn Ch1" = "C1_P7-156_L1_Welcome-to-scikit-learn.pdf",
  "Scikit-Learn Ch2" = "C2_P157-202_L1_scikit-learn-Tutorials.pdf",
  "Scikit-Learn Ch3" = "C3_P203-668_L1_User-Guide.pdf",
  "Scikit-Learn Ch4" = "C4_P669-688_L1_Glossary-of-Common-Terms-and-API-Elements.pdf",
  "Scikit-Learn Ch5" = "C5_P689-1464_L1_Examples.pdf",
  "Scikit-Learn Ch6" = "C6_P1465-2412_L1_API-Reference.pdf",
  "Scikit-Learn Ch7" = "C7_P2413-2456_L1_Developer8217s-Guide.pdf",
  "Scikit-Learn Bibliography" = "C8_P2457-2464_L1_Bibliography.pdf",
  "Scikit-Learn Index" = "C9_P2465-2503_L1_Index.pdf",
  "Estimating Traffic Volume" = "0361198119837236.pdf",
  "Image Recognition" = "CVPR.2016.90.pdf",
  "A Systematic Review" = "exsy.12400.pdf",
  "Mechanical Systems" = "j.ymssp.2018.05.050.pdf",
  "IoT in Edge" = "MNET.2018.1700202.pdf",
  "Deep Learning" = "nature14539.pdf",
  "Natural Language Processing" = "nsr_nwx110.pdf",
  "Financial Analysis" = "ssrn.3358252.pdf",
  "Biological Data" = "TNNLS.2018.2790388.pdf",
  "Purchase Behavior" = "0965254X.2018.1447984.pdf",
  "Purchase Intention" = "07363761211259223.pdf",
  "Who are Organic Food Consumers?" = "cb.210.pdf",
  "Food Quality" = "j.foodqual.2017.07.011.pdf",
  "Consumer Services II" = "j.jretconser.2018.04.011.pdf",
  "Consumer Services I" = "j.jretconser.2017.06.004.pdf",
  "Buying Organic Food" = "shsconf_20207404018.pdf",
  "Consumer Services III" = "j.jretconser.2019.05.005.pdf"
)

# Define document hierarchy
documents <- list(
  "Financial Statements" = list(
    "Financial Company" = list(
      "Citi" = list("Citi 2022 Annual Report", "Citi 2023 Annual Report", "Citi 2Q24 10-Q"),
      "Goldman Sachs" = list("2021 Form 10-K", "2022 Form 10-K", "2023 Form 10-K", "2023 Form 10-K Amendment"),
      "Morgan Stanley" = list("Morgan Stanley 2022 Form 10-K", "Morgan Stanley 2023 Form 10-K", "Morgan Stanley Q1 2024 10-Q"),
      "Wells Fargo" = list("Wells Fargo 2022 Form 10-K", "Wells Fargo 2023 Form 10-K")
    ),
    "Biomedical Company" = list(
      "biogen" = list("2018 Biogen Annual Report", "2019 Biogen Annual Report", "2020 Biogen Annual Report", 
                      "2021 Biogen Annual Report", "2022 Biogen Annual Report", "2023 Biogen Annual Report"),
      "Moderna" = list("Moderna 2020 Annual Report", "Moderna 2021 Annual Report", "Moderna 2022 Annual Report", 
                       "Moderna Reports Third Quarter 2023 Financial Results", "Q4 2023 PR Final", 
                       "Moderna 2023 Digital Annual Report", "Q1 2024 Earnings PR", "Q2 2024 Earnings PR"),
      "Pfizer" = list("Pfizer 2018 Financial Report", "Pfizer 2019 Financial Report", "Pfizer 2020 Form 10-K", 
                      "Pfizer 2021 Form 10-K", "Pfizer 2022 Form 10-K", "Pfizer 2023 Form 10-K")
    ),
    "Tech&Motor Company" = list(
      "Alphabet" = list("GOOG 10-Q Q1 2024", "GOOG 10-Q Q2 2024"),
      "Apple" = list("FY24 Q1 Consolidated Financial Statements", "FY24 Q2 Consolidated Financial Statements",
                     "FY24 Q3 Consolidated Financial Statements"),
      "Ford" = list("Ford 2018 Annual Report", "Ford 2019 Annual Report", "Ford 2020 Annual Report", 
                    "Ford 2021 Annual Report", "Ford 2022 Annual Report", "Ford 2023 Annual Report"),
      "GM" = list("GM Reports 2021 Full-Year and Fourth-Quarter Results", "GM Reports First-Quarter 2022 Results", 
                  "GM Reports Second-Quarter 2022 Results", "GM Reports Third-Quarter 2022 Results", 
                  "GM Releases 2022 Fourth-Quarter and Full-Year Results",
                  "GM Releases 2023 First-Quarter Results", "GM Releases 2023 Second-Quarter Results", 
                  "GM Releases 2023 Third-Quarter Results","GM Releases 2023 Fourth-Quarter and Full-Year Results", 
                  "GM Releases 2024 First-Quarter Results", "GM Releases 2024 Second-Quarter Results"),
      "Related Documents" = list("Evaluating Electric Vehicle Policy Effectiveness and Equity", 
                                 "Alternative Fuels Data Center: Electricity Laws and Incentives in California"),
      "Tesla" = list("Tesla 2020 Q1 Update", "Tesla 2020 Q2 Update", "Tesla 2020 Q3 Update",
                     "Tesla 2020 Q4 Update", "Tesla 2021 Q1 Update", "Tesla 2021 Q2 Update",
                     "Tesla 2021 Q3 Update", "Tesla 2021 Q4 Update", "Tesla 2022 Q1 Update",
                     "Tesla 2022 Q2 Update", "Tesla 2022 Q3 Update", "Tesla 2022 Q4 Update",
                     "Tesla 2023 Q1 Update", "Tesla 2023 Q2 Update", "Tesla 2023 Q3 Update",
                     "Tesla 2023 Q4 Update", "Tesla 2024 Q1 Update", "Tesla 2024 Q2 Update")
      )
  ),
  "Good Scikit Docs" = list("Scikit-Learn Ch1", "Scikit-Learn Ch2", "Scikit-Learn Ch3",
                            "Scikit-Learn Ch4", "Scikit-Learn Ch5", "Scikit-Learn Ch6",
                            "Scikit-Learn Ch7", "Scikit-Learn Bibliography", "Scikit-Learn Index"
  ),
  "NBA" = list("2021-22 NBA Rule Book", "2022-23 NBA Rule Book", "2023-24 NBA Rule Book",
               "2021 Hall of Fame Financial Statements", 
               "2023 NBA Collective Bargaining Agreement", 
               "2024-25 NBA Regular Season Schedule"
  ),
  "Other Topics" = list(
    "deep_learning" = list("Estimating Traffic Volume", "Image Recognition", 
                           "A Systematic Review", "Mechanical Systems", 
                           "IoT in Edge", "Deep Learning", "Natural Language Processing", 
                           "Financial Analysis", "Biological Data"),
    "organic_food" = list("Purchase Behavior", "Purchase Intention",
                          "Who are Organic Food Consumers?", "Food Quality",
                          "Buying Organic Food", "Consumer Services I", 
                          "Consumer Services II", "Consumer Services III")
  )
)

navigate <- function(hierarchy) {
  current_level <- hierarchy
  while (TRUE) {
    # If the current level is a character vector (list of documents)
    if (all(sapply(current_level, is.character))) {
      cat("\nDocuments available:\n")
      for (i in seq_along(current_level)) {
        cat(i, "-", current_level[[i]], "\n")
      }
      doc_selection <- as.integer(readline(prompt = "Enter the document number to select: "))
      if (!is.na(doc_selection) && doc_selection >= 1 && doc_selection <= length(current_level)) {
        return(current_level[[doc_selection]]) # Return the selected document
      } else {
        cat("Invalid selection. Try again.\n")
      }
    } else {
      # Display choices for the current level (nested list)
      choices <- names(current_level)
      cat("\nSelect an option:\n")
      for (i in seq_along(choices)) {
        cat(i, "-", choices[i], "\n")
      }
      selection <- as.integer(readline(prompt = "Enter your choice: "))
      if (!is.na(selection) && selection >= 1 && selection <= length(choices)) {
        current_level <- current_level[[selection]] # Navigate deeper into the hierarchy
      } else {
        cat("Invalid selection. Try again.\n")
      }
    }
  }
}

analyze_document <- function(doc_name) {
  cat("\nYou selected:", doc_name, "\n")
  
  # Check if the document exists in file_mapping
  if (!doc_name %in% names(file_mapping)) {
    cat("Error: No file mapping found for the selected document.\n")
    return(FALSE)
  }
  
  # Construct the file path
  file_path <- paste0("C:/Users/paulh/Documents/TTRFleschSentiment/", file_mapping[[doc_name]])
  
  if (!file.exists(file_path)) {
    cat("Error: File not found:", file_path, "\n")
    return(FALSE)
  }
  
  # Extract text from the PDF
  text <- pdf_text(file_path) %>% paste(collapse = " ")
  
  # Debugging: Ensure text is extracted
  if (nchar(text) == 0) {
    cat("Error: No text extracted from the PDF. Check if the PDF is readable.\n")
    return(FALSE)
  }
  
  # Tokenize and calculate TTR
  tokens <- unlist(str_split(tolower(text), "\\W+"))
  unique_words <- length(unique(tokens))
  total_words <- length(tokens)
  ttr <- unique_words / total_words
  
  # Calculate Flesch Reading Ease Score
  total_sentences <- length(unlist(str_split(text, "[.!?]")))
  total_syllables <- sum(nchar(unlist(str_extract_all(text, "[aeiouy]+"))))  # Approximation
  flesch_score <- 206.835 - (1.015 * (total_words / total_sentences)) - (84.6 * (total_syllables / total_words))
  
  # Display analysis results
  cat("\nAnalysis Results:")
  cat("\nTotal Words:", total_words)
  cat("\nUnique Words:", unique_words)
  cat("\nType-Token Ratio (TTR):", round(ttr, 3))
  cat("\nFlesch Reading Ease Score:", round(flesch_score, 2))
  
  # Prompt to continue or exit
  while (TRUE) {
    cat("\n\n\nDo you want to select another document?")
    cat("\n1 - Yes")
    cat("\n2 - No")
    user_choice <- as.integer(readline(prompt = "\nEnter your choice: "))
    
    if (user_choice == 1) {
      return(TRUE)  # Continue to the next document
    } else if (user_choice == 2) {
      cat("\nThank you for using the Document Navigator. Goodbye!\n")
      return(FALSE)  # Exit the program
    } else {
      cat("\nInvalid selection. Please enter 1 for Yes or 2 for No.\n")
    }
  }
}


main <- function() {
  repeat {
    # Display welcome message
    cat("\nWelcome to the Document Navigator\n")
    
    # Navigate the document hierarchy to select a document
    selected_doc <- navigate(documents)
    
    # Analyze the selected document
    if (!analyze_document(selected_doc)) {
      break  # Exit the program if the user chooses not to select another document
    }
  }
}


# Call the main function to start the program
main()