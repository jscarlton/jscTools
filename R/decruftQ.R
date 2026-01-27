#' Import and clean Qualtrics survey data
#'
#' This function imports Qualtrics CSV exports and handles the various metadata
#' rows that Qualtrics adds. It can automatically detect the export format,
#' rename columns based on a codebook, and convert Likert-scale responses to
#' ordered factors. See http://www.jscarlton.net/post/2017-04-04QualtricsImport/
#' for more information.
#'
#' @param input_csv Path to the downloaded Qualtrics CSV file. Can use here::here()
#'   for project-relative paths.
#' @param skip Number of rows to skip. Defaults to 3 for standard Qualtrics exports.
#'   Set to 1 if exported without extra header rows.
#' @param auto_detect If TRUE, automatically detect the export format and adjust
#'   skip accordingly. Defaults to FALSE for backward compatibility.
#' @param codebook Path to a CSV file with columns 'original' and 'new_name' for
#'   renaming variables. If NULL (default), no renaming is performed.
#' @param likert_cols Character vector of column names to convert to ordered factors
#'   using standard 5-point Likert scale (Strongly Disagree to Strongly Agree).
#'   If NULL (default), no conversion is performed.
#' @param likert_levels Character vector of factor levels for Likert conversion.
#'   Defaults to standard 5-point scale.
#'
#' @return A tibble with cleaned Qualtrics data
#' @export
#' @importFrom readr read_csv read_lines
#' @importFrom dplyr rename_with
#' @importFrom here here
#'
#' @examples
#' \dontrun{
#' # Basic usage with standard Qualtrics export
#' survey_data <- decruftQ(here::here("data", "raw", "survey.csv"))
#'
#' # Auto-detect export format
#' survey_data <- decruftQ("survey.csv", auto_detect = TRUE)
#'
#' # With codebook renaming
#' survey_data <- decruftQ("survey.csv", codebook = "codebook.csv")
#'
#' # Convert specific columns to Likert factors
#' survey_data <- decruftQ("survey.csv", likert_cols = c("Q1", "Q2", "Q3"))
#' }
decruftQ <- function(input_csv,
                     skip = 3,
                     auto_detect = FALSE,
                     codebook = NULL,
                     likert_cols = NULL,
                     likert_levels = c("Strongly Disagree", "Disagree",
                                       "Neither Agree nor Disagree",
                                       "Agree", "Strongly Agree")) {

  library(tidyverse)

  # Auto-detect export format if requested
  if (auto_detect) {
    skip <- detect_qualtrics_format(input_csv)
  }

  # Read column names from first row
  colnamesQ <- unlist(read_csv(input_csv, n_max = 1, col_names = FALSE,
                               show_col_types = FALSE))

  # Read the data, skipping header rows

  output_data <- read_csv(input_csv, skip = skip, col_names = colnamesQ,
                          show_col_types = FALSE)

  # Apply codebook renaming if provided
  if (!is.null(codebook)) {
    output_data <- apply_codebook(output_data, codebook)
  }

  # Convert Likert columns to ordered factors if specified
  if (!is.null(likert_cols)) {
    output_data <- convert_likert_cols(output_data, likert_cols, likert_levels)
  }

  return(output_data)
}

#' Detect Qualtrics export format
#'
#' Internal function to detect the export format of a Qualtrics CSV file
#' by examining the first few rows.
#'
#' @param input_csv Path to the Qualtrics CSV file
#' @return Number of rows to skip
#' @keywords internal
detect_qualtrics_format <- function(input_csv) {
  # Read first few lines to detect format
  first_lines <- read_lines(input_csv, n_max = 5)

  # Standard Qualtrics export has 3 header rows:

# Row 1: Variable names
  # Row 2: Question text
  # Row 3: ImportId or JSON metadata

  # Check if row 2 looks like question text (typically longer, contains "?")
  if (length(first_lines) >= 2) {
    row2_fields <- strsplit(first_lines[2], ",")[[1]]

    # If row 2 fields are very long or contain question marks, it's likely question text
    avg_length <- mean(nchar(row2_fields), na.rm = TRUE)

    if (avg_length > 50 || any(grepl("\\?", row2_fields))) {
      # Standard export with question text row
      # Check if row 3 looks like ImportId row
      if (length(first_lines) >= 3) {
        row3_fields <- strsplit(first_lines[3], ",")[[1]]
        if (any(grepl("ImportId|\\{\"ImportId\"", row3_fields))) {
          return(3)  # Standard 3-row header
        }
      }
      return(2)  # Only question text row, no ImportId
    }
  }

  # If we can't detect extra headers, assume minimal export
  return(1)
}

#' Apply codebook column renaming
#'
#' Internal function to rename columns based on a codebook CSV file.
#'
#' @param data A data frame to rename
#' @param codebook_path Path to CSV with 'original' and 'new_name' columns
#' @return Data frame with renamed columns
#' @keywords internal
apply_codebook <- function(data, codebook_path) {
  codebook_df <- read_csv(codebook_path, show_col_types = FALSE)

  # Validate codebook structure
  if (!all(c("original", "new_name") %in% names(codebook_df))) {
    stop("Codebook must contain 'original' and 'new_name' columns")
  }

  # Create named vector for renaming
  rename_vec <- setNames(codebook_df$original, codebook_df$new_name)

  # Only rename columns that exist in the data
  existing_cols <- rename_vec[rename_vec %in% names(data)]

  if (length(existing_cols) > 0) {
    data <- data %>%
      rename(!!!existing_cols)
  }

  return(data)
}

#' Convert columns to Likert ordered factors
#'
#' Internal function to convert specified columns to ordered factors.
#'
#' @param data A data frame
#' @param cols Character vector of column names to convert
#' @param levels Character vector of factor levels in order
#' @return Data frame with converted columns
#' @keywords internal
convert_likert_cols <- function(data, cols, levels) {
  # Only convert columns that exist
  existing_cols <- cols[cols %in% names(data)]

  for (col in existing_cols) {
    data[[col]] <- factor(data[[col]], levels = levels, ordered = TRUE)
  }

  return(data)
}
