#' Flag respondents who completed the survey too quickly
#'
#' Identifies respondents who likely rushed through the survey based on
#' completion time. Speeders often provide low-quality data and may need
#' to be excluded from analysis.
#'
#' @param data A data frame of Qualtrics survey data
#' @param method Method for determining speeders:
#'   \itemize{
#'     \item "median": Flag responses below a fraction of the median duration (default)
#'     \item "sd": Flag responses more than X standard deviations below the mean
#'     \item "absolute": Flag responses below an absolute threshold in minutes
#'     \item "iqr": Flag responses below Q1 - 1.5*IQR (outlier detection)
#'   }
#' @param threshold Threshold value depending on method:
#'   \itemize{
#'     \item For "median": Fraction of median (default 0.5 = half the median time)
#'     \item For "sd": Number of SDs below mean (default 2)
#'     \item For "absolute": Minimum time in minutes (default 2)
#'     \item For "iqr": Multiplier for IQR (default 1.5)
#'   }
#' @param duration_col Name of the duration column. Will auto-detect common names.
#' @param flag_col Name of the new flag column. Defaults to "speeder".
#' @param remove If TRUE, remove speeders instead of flagging. Defaults to FALSE.
#' @param report If TRUE (default), print a summary.
#'
#' @return Data frame with a new logical column indicating speeders (or filtered
#'   data frame if \code{remove = TRUE})
#' @export
#'
#' @examples
#' \dontrun{
#' # Flag speeders using default method (< 50% of median time)
#' survey_data <- flag_speeders(survey_data)
#'
#' # Use stricter threshold (< 33% of median)
#' survey_data <- flag_speeders(survey_data, threshold = 0.33)
#'
#' # Flag anyone under 2 minutes
#' survey_data <- flag_speeders(survey_data, method = "absolute", threshold = 2)
#'
#' # Flag statistical outliers (> 2 SD below mean)
#' survey_data <- flag_speeders(survey_data, method = "sd", threshold = 2)
#'
#' # Remove speeders instead of flagging
#' clean_data <- flag_speeders(survey_data, remove = TRUE)
#'
#' # Check how many speeders before deciding
#' survey_data <- flag_speeders(survey_data)
#' table(survey_data$speeder)
#'
#' # Use in analysis pipeline
#' library(dplyr)
#' clean_data <- decruftQ(here::here("data", "survey.csv")) %>%
#'   remove_preview() %>%
#'   flag_speeders(remove = TRUE)
#' }
flag_speeders <- function(data,
                          method = c("median", "sd", "absolute", "iqr"),
                          threshold = NULL,
                          duration_col = "Duration (in seconds)",
                          flag_col = "speeder",
                          remove = FALSE,
                          report = TRUE) {

  method <- match.arg(method)

 # Set default thresholds based on method
  if (is.null(threshold)) {
    threshold <- switch(method,
                        median = 0.5,
                        sd = 2,
                        absolute = 2,
                        iqr = 1.5)
  }

  # Find duration column
  duration_col <- find_column(data, c(
    duration_col,
    "Duration..in.seconds.",
    "Duration (in seconds)",
    "duration_seconds",
    "Duration",
    "duration",
    "DURATION",
    "Q_TotalDuration"
  ))

  if (is.null(duration_col)) {
    stop("Could not find duration column. Please specify 'duration_col' parameter.\n",
         "Common Qualtrics names: 'Duration (in seconds)', 'Q_TotalDuration'")
  }

  # Get duration in minutes
  duration_sec <- as.numeric(data[[duration_col]])
  duration_min <- duration_sec / 60

  # Calculate cutoff based on method
  cutoff_min <- switch(method,
    median = {
      med <- median(duration_min, na.rm = TRUE)
      med * threshold
    },
    sd = {
      m <- mean(duration_min, na.rm = TRUE)
      s <- sd(duration_min, na.rm = TRUE)
      m - (threshold * s)
    },
    absolute = {
      threshold
    },
    iqr = {
      q1 <- quantile(duration_min, 0.25, na.rm = TRUE)
      iqr_val <- IQR(duration_min, na.rm = TRUE)
      q1 - (threshold * iqr_val)
    }
  )

  # Ensure cutoff isn't negative
  cutoff_min <- max(cutoff_min, 0)

  # Flag speeders
  is_speeder <- duration_min < cutoff_min
  is_speeder[is.na(is_speeder)] <- FALSE

  n_speeders <- sum(is_speeder)
  n_total <- nrow(data)

  # Report
 if (report) {
    cat("Speeder Detection (method: ", method, ")\n", sep = "")
    cat("================================\n")
    cat("Duration stats:\n")
    cat("  Median:", round(median(duration_min, na.rm = TRUE), 1), "min\n")
    cat("  Mean:", round(mean(duration_min, na.rm = TRUE), 1), "min\n")
    cat("  Range:", round(min(duration_min, na.rm = TRUE), 1), "-",
        round(max(duration_min, na.rm = TRUE), 1), "min\n")
    cat("Cutoff: <", round(cutoff_min, 2), "min\n")
    cat("Speeders:", n_speeders, "of", n_total,
        "(", round(n_speeders/n_total * 100, 1), "%)\n", sep = "")

    if (n_speeders > 0 && n_speeders <= 10) {
      cat("Speeder durations:", paste(round(duration_min[is_speeder], 1), collapse = ", "), "min\n")
    }
  }

  if (remove) {
    if (report && n_speeders > 0) {
      cat("Removing", n_speeders, "speeders.\n")
    }
    return(data[!is_speeder, ])
  } else {
    data[[flag_col]] <- is_speeder
    return(data)
  }
}


#' Flag respondents who straightlined their responses
#'
#' Identifies respondents who gave the same response to all (or most) items
#' in a set of questions. Straightlining often indicates inattentive responding.
#'
#' @param data A data frame of survey data
#' @param items Character vector of column names to check for straightlining
#' @param threshold Proportion of items that must be identical to flag as
#'   straightlining. Defaults to 0.9 (90% same response).
#' @param flag_col Name of the new flag column. Defaults to "straightliner".
#' @param remove If TRUE, remove straightliners instead of flagging. Defaults to FALSE.
#' @param report If TRUE (default), print a summary.
#'
#' @return Data frame with a new logical column indicating straightliners
#' @export
#'
#' @examples
#' \dontrun{
#' # Flag straightliners across a set of Likert items
#' survey_data <- flag_straightliners(survey_data,
#'                                    items = c("Q1", "Q2", "Q3", "Q4", "Q5"))
#'
#' # Use stricter threshold (100% same response)
#' survey_data <- flag_straightliners(survey_data,
#'                                    items = paste0("trust_", 1:10),
#'                                    threshold = 1.0)
#'
#' # Remove straightliners
#' clean_data <- flag_straightliners(survey_data,
#'                                   items = c("Q1", "Q2", "Q3", "Q4", "Q5"),
#'                                   remove = TRUE)
#' }
flag_straightliners <- function(data,
                                items,
                                threshold = 0.9,
                                flag_col = "straightliner",
                                remove = FALSE,
                                report = TRUE) {

  # Validate items exist
  missing_items <- items[!items %in% names(data)]
  if (length(missing_items) > 0) {
    stop("Items not found in data: ", paste(missing_items, collapse = ", "))
  }

  n_items <- length(items)

  # Calculate proportion of same response for each row
  same_prop <- apply(data[items], 1, function(row) {
    row <- row[!is.na(row)]
    if (length(row) == 0) return(NA)
    max(table(row)) / length(row)
  })

  # Flag straightliners
  is_straightliner <- same_prop >= threshold
  is_straightliner[is.na(is_straightliner)] <- FALSE

  n_straightliners <- sum(is_straightliner)
  n_total <- nrow(data)

  if (report) {
    cat("Straightliner Detection\n")
    cat("=======================\n")
    cat("Items checked:", n_items, "\n")
    cat("Threshold:", threshold * 100, "% same response\n")
    cat("Straightliners:", n_straightliners, "of", n_total,
        "(", round(n_straightliners/n_total * 100, 1), "%)\n", sep = "")

    if (n_straightliners > 0 && n_straightliners <= 20) {
      # Show what responses the straightliners gave
      sl_data <- data[is_straightliner, items]
      most_common <- apply(sl_data, 1, function(row) {
        row <- row[!is.na(row)]
        if (length(row) == 0) return(NA)
        names(sort(table(row), decreasing = TRUE))[1]
      })
      cat("Most common straightline responses:\n")
      print(table(most_common))
    }
  }

  if (remove) {
    if (report && n_straightliners > 0) {
      cat("Removing", n_straightliners, "straightliners.\n")
    }
    return(data[!is_straightliner, ])
  } else {
    data[[flag_col]] <- is_straightliner
    return(data)
  }
}
