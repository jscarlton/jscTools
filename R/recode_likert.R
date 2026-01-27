#' Recode Likert scale responses to ordered factors with numeric codes
#'
#' This function standardizes Likert scale responses from Qualtrics surveys,
#' converting text responses to ordered factors and optionally adding numeric
#' codes. Handles common variations in response text (e.g., "Strongly disagree"
#' vs "Strongly Disagree").
#'
#' @param x A character or factor vector of Likert scale responses
#' @param scale Type of Likert scale. Options are:
#'   \itemize{
#'     \item "agree5": 5-point agreement scale (Strongly Disagree to Strongly Agree)
#'     \item "agree7": 7-point agreement scale
#'     \item "frequency5": 5-point frequency scale (Never to Always)
#'     \item "satisfaction5": 5-point satisfaction scale (Very Dissatisfied to Very Satisfied)
#'     \item "custom": Use custom levels provided in \code{levels} parameter
#'   }
#' @param levels Character vector of custom factor levels (required if scale = "custom").
#'   Should be in order from lowest to highest.
#' @param numeric If TRUE, return numeric codes instead of ordered factor. Defaults to FALSE.
#' @param reverse If TRUE, reverse the scale direction. Defaults to FALSE.
#' @param na_values Character vector of values to treat as NA. Defaults to common
#'   Qualtrics NA indicators.
#'
#' @return An ordered factor (or numeric vector if \code{numeric = TRUE})
#' @export
#' @importFrom stringr str_to_title str_trim
#' @importFrom dplyr case_when
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Basic usage - convert to ordered factor
#' survey_data <- survey_data %>%
#'   mutate(trust_q1 = recode_likert(Q1_trust))
#'
#' # Get numeric codes
#' survey_data <- survey_data %>%
#'   mutate(trust_q1_num = recode_likert(Q1_trust, numeric = TRUE))
#'
#' # Use 7-point scale
#' survey_data <- survey_data %>%
#'   mutate(satisfaction = recode_likert(Q2_sat, scale = "agree7"))
#'
#' # Reverse code (for negatively worded items)
#' survey_data <- survey_data %>%
#'   mutate(trust_q2_rev = recode_likert(Q2_distrust, reverse = TRUE))
#'
#' # Apply to multiple columns at once
#' survey_data <- survey_data %>%
#'   mutate(across(starts_with("trust_"), ~recode_likert(.x, numeric = TRUE)))
#' }
recode_likert <- function(x,
                          scale = "agree5",
                          levels = NULL,
                          numeric = FALSE,
                          reverse = FALSE,
                          na_values = c("", "N/A", "NA", "n/a", "Not applicable",
                                        "Prefer not to answer")) {

  library(stringr)

  # Define standard scales
  scales <- list(
    agree5 = c("Strongly Disagree", "Disagree", "Neither Agree nor Disagree",
               "Agree", "Strongly Agree"),
    agree7 = c("Strongly Disagree", "Disagree", "Somewhat Disagree",
               "Neither Agree nor Disagree", "Somewhat Agree", "Agree",
               "Strongly Agree"),
    frequency5 = c("Never", "Rarely", "Sometimes", "Often", "Always"),
    satisfaction5 = c("Very Dissatisfied", "Dissatisfied",
                      "Neither Satisfied nor Dissatisfied",
                      "Satisfied", "Very Satisfied")
  )

  # Get appropriate levels
  if (scale == "custom") {
    if (is.null(levels)) {
      stop("Must provide 'levels' when using scale = 'custom'")
    }
    scale_levels <- levels
  } else {
    if (!scale %in% names(scales)) {
      stop("Unknown scale type: ", scale,
           ". Options are: ", paste(names(scales), collapse = ", "), ", custom")
    }
    scale_levels <- scales[[scale]]
  }

  # Reverse if requested
  if (reverse) {
    scale_levels <- rev(scale_levels)
  }

  # Clean and standardize input
  x_clean <- str_trim(as.character(x))

  # Handle NA values
  x_clean[x_clean %in% na_values] <- NA

  # Standardize case for matching (title case)
  x_standardized <- str_to_title(x_clean)
  scale_levels_title <- str_to_title(scale_levels)

  # Create mapping from various formats to standard levels
  # Handle common variations
  x_matched <- x_standardized
  for (i in seq_along(scale_levels_title)) {
    # Match the title case version
    x_matched[x_standardized == scale_levels_title[i]] <- scale_levels[i]
  }

  # Convert to ordered factor
  result <- factor(x_matched, levels = scale_levels, ordered = TRUE)

  # Return numeric if requested
  if (numeric) {
    return(as.numeric(result))
  }

  return(result)
}

#' Recode multiple Likert columns in a data frame
#'
#' Convenience function to recode multiple Likert scale columns at once.
#'
#' @param data A data frame
#' @param cols Character vector of column names or tidyselect specification
#' @param suffix Suffix to add to new column names. If NULL, overwrites original columns.
#' @param ... Additional arguments passed to \code{recode_likert}
#'
#' @return Data frame with recoded columns
#' @export
#' @importFrom dplyr mutate across all_of
#' @importFrom rlang enquo
#'
#' @examples
#' \dontrun{
#' # Recode specific columns, creating new numeric versions
#' survey_data <- recode_likert_cols(survey_data,
#'                                   cols = c("Q1", "Q2", "Q3"),
#'                                   suffix = "_num",
#'                                   numeric = TRUE)
#'
#' # Recode and overwrite original columns
#' survey_data <- recode_likert_cols(survey_data,
#'                                   cols = c("Q1", "Q2", "Q3"))
#' }
recode_likert_cols <- function(data, cols, suffix = NULL, ...) {
  library(dplyr)

  if (is.null(suffix)) {
    # Overwrite original columns
    data <- data %>%
      mutate(across(all_of(cols), ~recode_likert(.x, ...)))
  } else {
    # Create new columns with suffix
    for (col in cols) {
      if (col %in% names(data)) {
        new_col <- paste0(col, suffix)
        data[[new_col]] <- recode_likert(data[[col]], ...)
      }
    }
  }

  return(data)
}
