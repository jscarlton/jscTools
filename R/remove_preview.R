#' Remove preview and test responses from Qualtrics data
#'
#' Filters out preview responses, incomplete responses, and test data from
#' Qualtrics survey exports. Qualtrics includes metadata columns that identify
#' response types, and this function uses those to clean your data.
#'
#' @param data A data frame of Qualtrics survey data (typically from \code{decruftQ})
#' @param remove_preview Logical. Remove responses where Status is "Survey Preview".
#'   Defaults to TRUE.
#' @param remove_incomplete Logical. Remove responses that weren't finished.
#'   Defaults to FALSE.
#' @param remove_spam Logical. Remove responses flagged as spam by Qualtrics.
#'   Defaults to TRUE.
#' @param preview_channel Logical. Remove responses from the "preview" distribution
#'   channel. Defaults to TRUE.
#' @param min_progress Numeric. Minimum progress percentage (0-100) to keep a
#'   response. Set to NULL to ignore progress. Defaults to NULL.
#' @param status_col Name of the status column. Defaults to "Status", but will
#'   also check for common variations.
#' @param finished_col Name of the finished column. Defaults to "Finished".
#' @param progress_col Name of the progress column. Defaults to "Progress".
#' @param channel_col Name of the distribution channel column. Defaults to
#'   "DistributionChannel".
#' @param report Logical. If TRUE (default), print a summary of removed responses.
#'
#' @return A filtered data frame with preview/test responses removed
#' @export
#'
#' @examples
#' \dontrun{
#' # Basic usage - remove previews and spam
#' clean_data <- remove_preview(survey_data)
#'
#' # Also remove incomplete responses
#' clean_data <- remove_preview(survey_data, remove_incomplete = TRUE)
#'
#' # Only keep responses with at least 80% progress
#' clean_data <- remove_preview(survey_data, min_progress = 80)
#'
#' # Quiet mode (no summary printed)
#' clean_data <- remove_preview(survey_data, report = FALSE)
#'
#' # Chain with decruftQ
#' library(dplyr)
#' survey_data <- decruftQ(here::here("data", "survey.csv")) %>%
#'   remove_preview(remove_incomplete = TRUE)
#' }
remove_preview <- function(data,
                           remove_preview = TRUE,
                           remove_incomplete = FALSE,
                           remove_spam = TRUE,
                           preview_channel = TRUE,
                           min_progress = NULL,
                           status_col = "Status",
                           finished_col = "Finished",
                           progress_col = "Progress",
                           channel_col = "DistributionChannel",
                           report = TRUE) {

  n_start <- nrow(data)
  removal_reasons <- list()

 # Find actual column names (handle variations)
  status_col <- find_column(data, c(status_col, "status", "STATUS", "ResponseType"))
  finished_col <- find_column(data, c(finished_col, "finished", "FINISHED", "Completed"))
  progress_col <- find_column(data, c(progress_col, "progress", "PROGRESS"))
  channel_col <- find_column(data, c(channel_col, "distributionchannel",
                                      "Distribution Channel", "DISTRIBUTIONCHANNEL"))

  # Remove preview responses
  if (remove_preview && !is.null(status_col)) {
    preview_mask <- grepl("preview", data[[status_col]], ignore.case = TRUE)
    n_preview <- sum(preview_mask, na.rm = TRUE)
    if (n_preview > 0) {
      data <- data[!preview_mask | is.na(preview_mask), ]
      removal_reasons$preview <- n_preview
    }
  }

  # Remove spam responses
  if (remove_spam && !is.null(status_col)) {
    spam_mask <- grepl("spam", data[[status_col]], ignore.case = TRUE)
    n_spam <- sum(spam_mask, na.rm = TRUE)
    if (n_spam > 0) {
      data <- data[!spam_mask | is.na(spam_mask), ]
      removal_reasons$spam <- n_spam
    }
  }

  # Remove preview distribution channel
  if (preview_channel && !is.null(channel_col)) {
    channel_mask <- grepl("preview", data[[channel_col]], ignore.case = TRUE)
    n_channel <- sum(channel_mask, na.rm = TRUE)
    if (n_channel > 0) {
      data <- data[!channel_mask | is.na(channel_mask), ]
      removal_reasons$preview_channel <- n_channel
    }
  }

  # Remove incomplete responses
  if (remove_incomplete && !is.null(finished_col)) {
    finished_vals <- data[[finished_col]]

    # Handle different formats (TRUE/FALSE, 1/0, "True"/"False")
    if (is.logical(finished_vals)) {
      incomplete_mask <- !finished_vals
    } else if (is.numeric(finished_vals)) {
      incomplete_mask <- finished_vals != 1
    } else {
      incomplete_mask <- !grepl("^(true|1|yes)$", as.character(finished_vals),
                                ignore.case = TRUE)
    }

    n_incomplete <- sum(incomplete_mask, na.rm = TRUE)
    if (n_incomplete > 0) {
      data <- data[!incomplete_mask | is.na(incomplete_mask), ]
      removal_reasons$incomplete <- n_incomplete
    }
  }

  # Remove by minimum progress
  if (!is.null(min_progress) && !is.null(progress_col)) {
    progress_vals <- as.numeric(data[[progress_col]])
    low_progress_mask <- progress_vals < min_progress
    n_low_progress <- sum(low_progress_mask, na.rm = TRUE)
    if (n_low_progress > 0) {
      data <- data[!low_progress_mask | is.na(low_progress_mask), ]
      removal_reasons$low_progress <- n_low_progress
    }
  }

  # Report summary
  if (report) {
    n_end <- nrow(data)
    n_removed <- n_start - n_end

    if (n_removed > 0) {
      cat("Removed", n_removed, "of", n_start, "responses:\n")
      if (!is.null(removal_reasons$preview)) {
        cat("  - Preview responses:", removal_reasons$preview, "\n")
      }
      if (!is.null(removal_reasons$spam)) {
        cat("  - Spam responses:", removal_reasons$spam, "\n")
      }
      if (!is.null(removal_reasons$preview_channel)) {
        cat("  - Preview channel:", removal_reasons$preview_channel, "\n")
      }
      if (!is.null(removal_reasons$incomplete)) {
        cat("  - Incomplete:", removal_reasons$incomplete, "\n")
      }
      if (!is.null(removal_reasons$low_progress)) {
        cat("  - Low progress (<", min_progress, "%):", removal_reasons$low_progress, "\n")
      }
      cat("Remaining responses:", n_end, "\n")
    } else {
      cat("No responses removed.", n_start, "responses retained.\n")
    }
  }

  return(data)
}

#' Find a column by checking multiple possible names
#'
#' @param data A data frame
#' @param possible_names Character vector of possible column names to check
#' @return The first matching column name, or NULL if none found
#' @keywords internal
find_column <- function(data, possible_names) {
  for (name in possible_names) {
    if (name %in% names(data)) {
      return(name)
    }
  }
  return(NULL)
}

#' Summarize Qualtrics response quality
#'
#' Provides a quick overview of response quality metrics without removing any data.
#' Useful for understanding your data before cleaning.
#'
#' @param data A data frame of Qualtrics survey data
#' @param status_col Name of the status column
#' @param finished_col Name of the finished column
#' @param progress_col Name of the progress column
#' @param channel_col Name of the distribution channel column
#' @param duration_col Name of the duration column (in seconds)
#'
#' @return A list with quality metrics (also prints a summary)
#' @export
#'
#' @examples
#' \dontrun{
#' # Get overview of response quality
#' quality <- response_quality(survey_data)
#'
#' # Access specific metrics
#' quality$n_preview
#' quality$completion_rate
#' }
response_quality <- function(data,
                             status_col = "Status",
                             finished_col = "Finished",
                             progress_col = "Progress",
                             channel_col = "DistributionChannel",
                             duration_col = "Duration (in seconds)") {

  n_total <- nrow(data)

  # Find actual column names
  status_col <- find_column(data, c(status_col, "status", "STATUS"))
  finished_col <- find_column(data, c(finished_col, "finished", "FINISHED"))
  progress_col <- find_column(data, c(progress_col, "progress", "PROGRESS"))
  channel_col <- find_column(data, c(channel_col, "distributionchannel"))
  duration_col <- find_column(data, c(duration_col, "Duration..in.seconds.",
                                       "duration", "Duration"))

  results <- list(n_total = n_total)

  cat("Response Quality Summary\n")
  cat("========================\n")
  cat("Total responses:", n_total, "\n\n")

  # Status breakdown
  if (!is.null(status_col)) {
    status_table <- table(data[[status_col]], useNA = "ifany")
    cat("Status:\n")
    for (i in seq_along(status_table)) {
      cat("  ", names(status_table)[i], ": ", status_table[i], "\n", sep = "")
    }
    results$status_breakdown <- status_table
    results$n_preview <- sum(grepl("preview", names(status_table), ignore.case = TRUE) *
                               as.numeric(status_table))
    cat("\n")
  }

  # Completion
 if (!is.null(finished_col)) {
    finished_vals <- data[[finished_col]]
    if (is.logical(finished_vals)) {
      n_complete <- sum(finished_vals, na.rm = TRUE)
    } else if (is.numeric(finished_vals)) {
      n_complete <- sum(finished_vals == 1, na.rm = TRUE)
    } else {
      n_complete <- sum(grepl("^(true|1|yes)$", as.character(finished_vals),
                              ignore.case = TRUE))
    }
    completion_rate <- round(n_complete / n_total * 100, 1)
    cat("Completion: ", n_complete, "/", n_total, " (", completion_rate, "%)\n", sep = "")
    results$n_complete <- n_complete
    results$completion_rate <- completion_rate
  }

  # Progress
  if (!is.null(progress_col)) {
    progress_vals <- as.numeric(data[[progress_col]])
    cat("Progress: mean =", round(mean(progress_vals, na.rm = TRUE), 1), "%,",
        "median =", round(median(progress_vals, na.rm = TRUE), 1), "%\n")
    results$progress_mean <- mean(progress_vals, na.rm = TRUE)
    results$progress_median <- median(progress_vals, na.rm = TRUE)
  }

  # Duration
  if (!is.null(duration_col)) {
    duration_vals <- as.numeric(data[[duration_col]])
    duration_mins <- duration_vals / 60
    cat("Duration: median =", round(median(duration_mins, na.rm = TRUE), 1), "min,",
        "range =", round(min(duration_mins, na.rm = TRUE), 1), "-",
        round(max(duration_mins, na.rm = TRUE), 1), "min\n")
    results$duration_median_min <- median(duration_mins, na.rm = TRUE)
    results$duration_range <- range(duration_mins, na.rm = TRUE)
  }

  # Distribution channels
  if (!is.null(channel_col)) {
    channel_table <- table(data[[channel_col]], useNA = "ifany")
    cat("\nDistribution Channels:\n")
    for (i in seq_along(channel_table)) {
      cat("  ", names(channel_table)[i], ": ", channel_table[i], "\n", sep = "")
    }
    results$channel_breakdown <- channel_table
  }

  invisible(results)
}
