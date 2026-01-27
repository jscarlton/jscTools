#' Calculate composite trust scale scores
#'
#' Computes composite trust scores from multiple survey items, handling reverse
#' coding and providing options for different aggregation methods. Designed for
#' use with Qualtrics survey data measuring trust constructs.
#'
#' @param data A data frame containing the trust items
#' @param items Character vector of column names containing the trust items
#' @param reverse_items Character vector of column names that should be reverse
#'   coded before computing the composite. Defaults to NULL.
#' @param method Aggregation method: "mean" (default), "sum", or "median"
#' @param min_valid Minimum number of valid (non-NA) responses required to
#'   compute a score. Defaults to half the number of items (rounded up).
#' @param scale_range Numeric vector of length 2 specifying the min and max of
#'   the response scale (e.g., c(1, 5) for 5-point Likert). Used for reverse
#'   coding. Defaults to c(1, 5).
#' @param new_col Name for the new composite score column. Defaults to "trust_score".
#'
#' @return The input data frame with an additional column containing the composite score
#' @export
#' @importFrom dplyr mutate rowwise c_across ungroup
#' @importFrom tidyselect all_of
#'
#' @examples
#' \dontrun{
#' # Basic usage - compute mean of trust items
#' survey_data <- calculate_trust_scale(
#'   survey_data,
#'   items = c("trust_1", "trust_2", "trust_3", "trust_4")
#' )
#'
#' # With reverse coding
#' survey_data <- calculate_trust_scale(
#'   survey_data,
#'   items = c("trust_1", "trust_2", "distrust_1", "distrust_2"),
#'   reverse_items = c("distrust_1", "distrust_2")
#' )
#'
#' # Using sum instead of mean, with 7-point scale
#' survey_data <- calculate_trust_scale(
#'   survey_data,
#'   items = c("t1", "t2", "t3"),
#'   method = "sum",
#'   scale_range = c(1, 7)
#' )
#'
#' # Create multiple trust subscales
#' survey_data <- survey_data %>%
#'   calculate_trust_scale(items = c("comp_1", "comp_2"), new_col = "competence") %>%
#'   calculate_trust_scale(items = c("benev_1", "benev_2"), new_col = "benevolence") %>%
#'   calculate_trust_scale(items = c("integ_1", "integ_2"), new_col = "integrity")
#' }
calculate_trust_scale <- function(data,
                                  items,
                                  reverse_items = NULL,
                                  method = c("mean", "sum", "median"),
                                  min_valid = NULL,
                                  scale_range = c(1, 5),
                                  new_col = "trust_score") {

  library(dplyr)

  method <- match.arg(method)

  # Set default min_valid
  if (is.null(min_valid)) {
    min_valid <- ceiling(length(items) / 2)
  }

  # Validate items exist in data
  missing_items <- items[!items %in% names(data)]
  if (length(missing_items) > 0) {
    stop("The following items are not in the data: ",
         paste(missing_items, collapse = ", "))
  }

  # Create working copy of item columns
  work_data <- data[items]

  # Ensure all items are numeric
  for (col in items) {
    if (is.factor(work_data[[col]]) || is.ordered(work_data[[col]])) {
      work_data[[col]] <- as.numeric(work_data[[col]])
    } else if (is.character(work_data[[col]])) {
      work_data[[col]] <- as.numeric(work_data[[col]])
    }
  }

  # Apply reverse coding
  if (!is.null(reverse_items)) {
    # Validate reverse items
    invalid_reverse <- reverse_items[!reverse_items %in% items]
    if (length(invalid_reverse) > 0) {
      stop("Reverse items must be in the items list: ",
           paste(invalid_reverse, collapse = ", "))
    }

    scale_max <- scale_range[2]
    scale_min <- scale_range[1]

    for (col in reverse_items) {
      # Reverse code: new = max + min - old
      work_data[[col]] <- scale_max + scale_min - work_data[[col]]
    }
  }

  # Calculate composite score
  data[[new_col]] <- apply(work_data, 1, function(row) {
    valid_values <- row[!is.na(row)]
    n_valid <- length(valid_values)

    if (n_valid < min_valid) {
      return(NA_real_)
    }

    switch(method,
           mean = mean(valid_values, na.rm = TRUE),
           sum = sum(valid_values, na.rm = TRUE),
           median = median(valid_values, na.rm = TRUE))
  })

  return(data)
}

#' Calculate Cronbach's alpha for scale reliability
#'
#' Computes Cronbach's alpha coefficient for a set of scale items to assess
#' internal consistency reliability.
#'
#' @param data A data frame containing the scale items
#' @param items Character vector of column names containing the scale items
#'
#' @return A list with alpha coefficient and item statistics
#' @export
#'
#' @examples
#' \dontrun{
#' # Check reliability of trust scale
#' reliability <- cronbach_alpha(survey_data, c("trust_1", "trust_2", "trust_3"))
#' print(reliability$alpha)
#' }
cronbach_alpha <- function(data, items) {
  # Validate items exist
  missing_items <- items[!items %in% names(data)]
  if (length(missing_items) > 0) {
    stop("The following items are not in the data: ",
         paste(missing_items, collapse = ", "))
  }

  # Extract and convert items to numeric matrix
  item_data <- data[items]

  for (col in items) {
    if (is.factor(item_data[[col]]) || is.ordered(item_data[[col]])) {
      item_data[[col]] <- as.numeric(item_data[[col]])
    }
  }

  item_matrix <- as.matrix(item_data)

  # Remove rows with any NA
  complete_cases <- complete.cases(item_matrix)
  item_matrix <- item_matrix[complete_cases, , drop = FALSE]

  if (nrow(item_matrix) < 3) {
    warning("Too few complete cases to compute alpha reliably")
    return(list(alpha = NA, n_items = length(items), n_obs = nrow(item_matrix)))
  }

  # Calculate alpha
  k <- ncol(item_matrix)
  item_vars <- apply(item_matrix, 2, var)
  total_var <- var(rowSums(item_matrix))

  alpha <- (k / (k - 1)) * (1 - sum(item_vars) / total_var)

  # Item-total correlations
  total_scores <- rowSums(item_matrix)
  item_total_cors <- sapply(1:k, function(i) {
    cor(item_matrix[, i], total_scores - item_matrix[, i])
  })
  names(item_total_cors) <- items

  return(list(
    alpha = alpha,
    n_items = k,
    n_obs = nrow(item_matrix),
    item_total_correlations = item_total_cors,
    item_variances = item_vars
  ))
}
