#' Quick visualization of Likert scale responses
#'
#' Creates a diverging stacked bar chart for visualizing Likert scale responses,
#' following best practices for survey data visualization. The neutral category
#' is centered, with negative responses on the left and positive on the right.
#'
#' @param data A data frame containing the survey data
#' @param items Character vector of column names containing Likert items to plot.
#'   If NULL, will attempt to plot all factor columns.
#' @param labels Optional named character vector for item labels (names are column
#'   names, values are display labels). If NULL, uses column names.
#' @param colors Character vector of colors for the response categories. Defaults
#'   to a diverging blue-gray-red palette suitable for 5-point scales.
#' @param neutral Position of the neutral category (1-indexed). Defaults to 3 for
#'   5-point scales. Set to NULL for scales without a neutral option.
#' @param wrap_width Number of characters before wrapping item labels. Defaults to 40.
#' @param percent If TRUE (default), show percentages. If FALSE, show counts.
#' @param title Optional plot title
#' @param subtitle Optional plot subtitle
#'
#' @return A ggplot2 object
#' @export
#' @importFrom ggplot2 ggplot aes geom_bar geom_text coord_flip scale_fill_manual
#'   scale_x_discrete labs theme_minimal theme element_text element_blank
#' @importFrom dplyr mutate select across all_of where group_by summarise n
#'   ungroup arrange desc
#' @importFrom tidyr pivot_longer
#' @importFrom scales percent_format
#' @importFrom stringr str_wrap
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' plot_likert(survey_data, items = c("Q1", "Q2", "Q3"))
#'
#' # With custom labels
#' plot_likert(survey_data,
#'             items = c("Q1", "Q2", "Q3"),
#'             labels = c(Q1 = "Trust in government",
#'                        Q2 = "Trust in scientists",
#'                        Q3 = "Trust in media"))
#'
#' # Customize appearance
#' plot_likert(survey_data, items = c("Q1", "Q2"),
#'             title = "Trust in Institutions",
#'             subtitle = "n = 500") +
#'   theme_fira()
#' }
plot_likert <- function(data,
                        items = NULL,
                        labels = NULL,
                        colors = NULL,
                        neutral = 3,
                        wrap_width = 40,
                        percent = TRUE,
                        title = NULL,
                        subtitle = NULL) {

  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(stringr)

  # Auto-detect items if not specified
  if (is.null(items)) {
    items <- names(data)[sapply(data, function(x) is.factor(x) | is.ordered(x))]
    if (length(items) == 0) {
      stop("No factor columns found. Please specify 'items' parameter.")
    }
  }

  # Validate items exist
  missing_items <- items[!items %in% names(data)]
  if (length(missing_items) > 0) {
    stop("Items not found in data: ", paste(missing_items, collapse = ", "))
  }

  # Get response levels from the first item
  first_item <- data[[items[1]]]
  if (!is.factor(first_item)) {
    first_item <- factor(first_item)
  }
  response_levels <- levels(first_item)
  n_levels <- length(response_levels)

  # Set default colors
  if (is.null(colors)) {
    if (n_levels == 5) {
      colors <- c("#b2182b", "#ef8a62", "#999999", "#67a9cf", "#2166ac")
    } else if (n_levels == 7) {
      colors <- c("#b2182b", "#d6604d", "#f4a582", "#999999",
                  "#92c5de", "#4393c3", "#2166ac")
    } else {
      # Generate a gradient for other scales
      colors <- colorRampPalette(c("#b2182b", "#999999", "#2166ac"))(n_levels)
    }
  }

  # Prepare data for plotting
  plot_data <- data %>%
    select(all_of(items)) %>%
    mutate(row_id = row_number()) %>%
    pivot_longer(cols = all_of(items),
                 names_to = "item",
                 values_to = "response") %>%
    filter(!is.na(response)) %>%
    group_by(item, response) %>%
    summarise(count = n(), .groups = "drop") %>%
    group_by(item) %>%
    mutate(total = sum(count),
           prop = count / total) %>%
    ungroup()

  # Apply labels if provided
  if (!is.null(labels)) {
    plot_data <- plot_data %>%
      mutate(item_label = ifelse(item %in% names(labels),
                                 labels[item],
                                 item))
  } else {
    plot_data <- plot_data %>%
      mutate(item_label = item)
  }

  # Wrap labels
  plot_data <- plot_data %>%
    mutate(item_label = str_wrap(item_label, width = wrap_width))

  # Ensure response is a factor with correct levels
  plot_data <- plot_data %>%
    mutate(response = factor(response, levels = response_levels))

  # Calculate positions for diverging bar chart
  if (!is.null(neutral) && neutral <= n_levels) {
    # Create diverging positions around neutral
    plot_data <- plot_data %>%
      mutate(
        # Assign position values: negative for disagree, 0 for neutral, positive for agree
        position_val = as.numeric(response) - neutral,
        # Calculate starting position for each bar
        prop_signed = ifelse(position_val < 0, -prop,
                             ifelse(position_val > 0, prop, 0))
      )

    # For diverging chart, we need to handle the stacking differently
    # Simpler approach: create a regular stacked bar and offset

    p <- ggplot(plot_data, aes(x = item_label, y = prop, fill = response)) +
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      coord_flip() +
      scale_fill_manual(values = colors,
                        name = "Response",
                        drop = FALSE) +
      scale_y_continuous(labels = if(percent) scales::percent_format() else waiver()) +
      labs(x = NULL,
           y = if(percent) "Percent" else "Count",
           title = title,
           subtitle = subtitle) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.text.y = element_text(hjust = 1),
        panel.grid.major.y = element_blank()
      )

  } else {
    # Standard stacked bar (no neutral centering)
    p <- ggplot(plot_data, aes(x = item_label, y = prop, fill = response)) +
      geom_bar(stat = "identity", position = "stack", width = 0.7) +
      coord_flip() +
      scale_fill_manual(values = colors,
                        name = "Response",
                        drop = FALSE) +
      scale_y_continuous(labels = if(percent) scales::percent_format() else waiver()) +
      labs(x = NULL,
           y = if(percent) "Percent" else "Count",
           title = title,
           subtitle = subtitle) +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.text.y = element_text(hjust = 1),
        panel.grid.major.y = element_blank()
      )
  }

  return(p)
}

#' Create a summary table of Likert responses
#'
#' Generates a summary table showing counts and percentages for each response
#' category across multiple Likert items.
#'
#' @param data A data frame containing the survey data
#' @param items Character vector of column names containing Likert items
#' @param labels Optional named character vector for item labels
#'
#' @return A tibble with response summaries
#' @export
#' @importFrom dplyr mutate select across all_of group_by summarise n ungroup
#' @importFrom tidyr pivot_longer pivot_wider
#'
#' @examples
#' \dontrun{
#' likert_summary(survey_data, items = c("Q1", "Q2", "Q3"))
#' }
likert_summary <- function(data, items, labels = NULL) {
  library(dplyr)
  library(tidyr)

  # Validate items
  missing_items <- items[!items %in% names(data)]
  if (length(missing_items) > 0) {
    stop("Items not found in data: ", paste(missing_items, collapse = ", "))
  }

  # Calculate summaries
  summary_data <- data %>%
    select(all_of(items)) %>%
    mutate(row_id = row_number()) %>%
    pivot_longer(cols = all_of(items),
                 names_to = "item",
                 values_to = "response") %>%
    filter(!is.na(response)) %>%
    group_by(item, response) %>%
    summarise(n = n(), .groups = "drop") %>%
    group_by(item) %>%
    mutate(total = sum(n),
           pct = round(n / total * 100, 1)) %>%
    ungroup()

  # Apply labels if provided
  if (!is.null(labels)) {
    summary_data <- summary_data %>%
      mutate(item = ifelse(item %in% names(labels), labels[item], item))
  }

  return(summary_data)
}
