#' Format regression and correlation tables for APA-style publication
#'
#' Converts regression model output or correlation matrices into APA-formatted
#' tables ready for publication. Handles multiple model types and can export
#' to various formats.
#'
#' @param model A model object (lm, glm) or correlation matrix, or a list of models
#'   for side-by-side comparison
#' @param type Type of table: "regression", "correlation", or "auto" (default)
#'   to detect automatically
#' @param stars If TRUE (default), add significance stars
#' @param ci If TRUE, include 95% confidence intervals for regression coefficients
#' @param digits Number of decimal places. Defaults to 3 for correlations, 2 for regression
#' @param var_labels Optional named character vector to rename variables
#' @param caption Table caption for output
#' @param note Table note (appears below table)
#' @param output Output format: "tibble" (default), "kable", "gt", or "clipboard"
#'
#' @return Formatted table in the specified format
#' @export
#' @importFrom dplyr mutate select rename across
#' @importFrom tibble tibble as_tibble rownames_to_column
#' @importFrom stats coef confint vcov cor.test
#'
#' @examples
#' \dontrun{
#' # Regression table
#' model <- lm(mpg ~ wt + hp + cyl, data = mtcars)
#' export_table_apa(model)
#'
#' # With confidence intervals and custom labels
#' export_table_apa(model,
#'                  ci = TRUE,
#'                  var_labels = c(wt = "Weight",
#'                                 hp = "Horsepower",
#'                                 cyl = "Cylinders"))
#'
#' # Correlation matrix
#' cor_mat <- cor(mtcars[, c("mpg", "wt", "hp")])
#' export_table_apa(cor_mat, type = "correlation")
#'
#' # Multiple models for comparison
#' model1 <- lm(mpg ~ wt, data = mtcars)
#' model2 <- lm(mpg ~ wt + hp, data = mtcars)
#' model3 <- lm(mpg ~ wt + hp + cyl, data = mtcars)
#' export_table_apa(list(model1, model2, model3))
#'
#' # Copy to clipboard for pasting into Word
#' export_table_apa(model, output = "clipboard")
#' }
export_table_apa <- function(model,
                             type = "auto",
                             stars = TRUE,
                             ci = FALSE,
                             digits = NULL,
                             var_labels = NULL,
                             caption = NULL,
                             note = NULL,
                             output = "tibble") {

  library(dplyr)
  library(tibble)

  # Auto-detect type
  if (type == "auto") {
    if (is.matrix(model) && isSymmetric(model)) {
      type <- "correlation"
    } else if (inherits(model, c("lm", "glm")) || is.list(model)) {
      type <- "regression"
    } else {
      stop("Cannot auto-detect table type. Please specify 'type' parameter.")
    }
  }

  # Set default digits
  if (is.null(digits)) {
    digits <- if (type == "correlation") 3 else 2
  }

  # Process based on type
  if (type == "regression") {
    result <- format_regression_apa(model, stars, ci, digits, var_labels)
  } else if (type == "correlation") {
    result <- format_correlation_apa(model, stars, digits, var_labels)
  } else {
    stop("Unknown table type: ", type)
  }

  # Add caption and note attributes
  attr(result, "caption") <- caption
  attr(result, "note") <- note

  # Handle output format
  if (output == "clipboard") {
    copy_to_clipboard(result)
    message("Table copied to clipboard")
    return(invisible(result))
  } else if (output == "kable") {
    if (!requireNamespace("knitr", quietly = TRUE)) {
      warning("knitr not available, returning tibble")
      return(result)
    }
    return(knitr::kable(result, caption = caption, digits = digits))
  } else if (output == "gt") {
    if (!requireNamespace("gt", quietly = TRUE)) {
      warning("gt not available, returning tibble")
      return(result)
    }
    gt_table <- gt::gt(result)
    if (!is.null(caption)) gt_table <- gt::tab_header(gt_table, title = caption)
    if (!is.null(note)) gt_table <- gt::tab_footnote(gt_table, footnote = note)
    return(gt_table)
  }

  return(result)
}

#' Format regression output for APA style
#' @keywords internal
format_regression_apa <- function(model, stars, ci, digits, var_labels) {
  # Handle list of models
  if (is.list(model) && !inherits(model, c("lm", "glm"))) {
    return(format_multiple_regressions(model, stars, ci, digits, var_labels))
  }

  # Get model summary
  model_summary <- summary(model)
  coef_table <- as.data.frame(coef(model_summary))

  # Standard column names depending on model type
  if (inherits(model, "glm")) {
    names(coef_table) <- c("B", "SE", "z", "p")
  } else {
    names(coef_table) <- c("B", "SE", "t", "p")
  }

  # Add confidence intervals if requested
  if (ci) {
    ci_vals <- confint(model)
    coef_table$CI_lower <- ci_vals[, 1]
    coef_table$CI_upper <- ci_vals[, 2]
  }

  # Format values
  coef_table <- coef_table %>%
    mutate(
      B = format_num(B, digits),
      SE = format_num(SE, digits)
    )

  if ("t" %in% names(coef_table)) {
    coef_table <- coef_table %>%
      mutate(t = format_num(t, digits))
  }
  if ("z" %in% names(coef_table)) {
    coef_table <- coef_table %>%
      mutate(z = format_num(z, digits))
  }

  # Add significance stars
  if (stars) {
    coef_table <- coef_table %>%
      mutate(
        sig = case_when(
          p < .001 ~ "***",
          p < .01 ~ "**",
          p < .05 ~ "*",
          TRUE ~ ""
        ),
        p = paste0(format_p(p), sig)
      ) %>%
      select(-sig)
  } else {
    coef_table <- coef_table %>%
      mutate(p = format_p(p))
  }

  # Format CI if present
  if (ci) {
    coef_table <- coef_table %>%
      mutate(
        `95% CI` = paste0("[", format_num(CI_lower, digits), ", ",
                          format_num(CI_upper, digits), "]")
      ) %>%
      select(-CI_lower, -CI_upper)
  }

  # Add variable names as column
  coef_table <- coef_table %>%
    rownames_to_column("Variable")

  # Apply variable labels
  if (!is.null(var_labels)) {
    coef_table <- coef_table %>%
      mutate(Variable = ifelse(Variable %in% names(var_labels),
                               var_labels[Variable],
                               Variable))
  }

  # Add model fit statistics
  r_sq <- model_summary$r.squared
  adj_r_sq <- model_summary$adj.r.squared
  f_stat <- model_summary$fstatistic

  fit_note <- paste0("R\u00B2 = ", format_num(r_sq, 3),
                     ", Adj. R\u00B2 = ", format_num(adj_r_sq, 3))
  if (!is.null(f_stat)) {
    fit_note <- paste0(fit_note,
                       ", F(", f_stat[2], ", ", f_stat[3], ") = ",
                       format_num(f_stat[1], 2))
  }

  attr(coef_table, "fit_stats") <- fit_note

  return(as_tibble(coef_table))
}

#' Format multiple regression models for comparison
#' @keywords internal
format_multiple_regressions <- function(models, stars, ci, digits, var_labels) {
  library(tibble)

  # Get all unique variable names across models
  all_vars <- unique(unlist(lapply(models, function(m) names(coef(m)))))

  # Apply labels to variable names
  display_vars <- all_vars
  if (!is.null(var_labels)) {
    display_vars <- ifelse(all_vars %in% names(var_labels),
                           var_labels[all_vars],
                           all_vars)
  }

  # Initialize result table
  result <- tibble(Variable = display_vars)

  # Add each model's coefficients
  for (i in seq_along(models)) {
    model <- models[[i]]
    model_summary <- summary(model)
    coefs <- coef(model_summary)

    # Create columns for this model
    b_col <- rep(NA_character_, length(all_vars))
    se_col <- rep(NA_character_, length(all_vars))

    for (var in rownames(coefs)) {
      idx <- which(all_vars == var)
      if (length(idx) > 0) {
        b_val <- coefs[var, 1]
        p_val <- coefs[var, 4]

        star_str <- ""
        if (stars) {
          if (p_val < .001) star_str <- "***"
          else if (p_val < .01) star_str <- "**"
          else if (p_val < .05) star_str <- "*"
        }

        b_col[idx] <- paste0(format_num(b_val, digits), star_str)
        se_col[idx] <- paste0("(", format_num(coefs[var, 2], digits), ")")
      }
    }

    col_name <- paste0("Model ", i)
    result[[col_name]] <- b_col
    result[[paste0(col_name, " SE")]] <- se_col
  }

  # Add fit statistics as attributes
  fit_stats <- sapply(models, function(m) {
    s <- summary(m)
    paste0("R\u00B2 = ", format_num(s$r.squared, 3))
  })
  attr(result, "fit_stats") <- fit_stats

  return(result)
}

#' Format correlation matrix for APA style
#' @keywords internal
format_correlation_apa <- function(cor_matrix, stars, digits, var_labels) {
  library(tibble)

  if (!is.matrix(cor_matrix)) {
    stop("Correlation input must be a matrix")
  }

  var_names <- colnames(cor_matrix)
  if (is.null(var_names)) {
    var_names <- paste0("V", 1:ncol(cor_matrix))
  }

  # Apply labels
  display_names <- var_names
  if (!is.null(var_labels)) {
    display_names <- ifelse(var_names %in% names(var_labels),
                            var_labels[var_names],
                            var_names)
  }

  # Create lower triangle table (APA style)
  n_vars <- length(var_names)
  result <- matrix("", nrow = n_vars, ncol = n_vars)

  for (i in 1:n_vars) {
    for (j in 1:n_vars) {
      if (i == j) {
        result[i, j] <- "\u2014"  # em dash for diagonal
      } else if (i > j) {
        r_val <- cor_matrix[i, j]
        result[i, j] <- format_num(r_val, digits)
      }
      # Upper triangle left blank
    }
  }

  result <- as.data.frame(result)
  names(result) <- display_names
  result <- cbind(Variable = display_names, result)

  return(as_tibble(result))
}

#' Helper function to format numbers
#' @keywords internal
format_num <- function(x, digits = 2) {
  formatC(x, digits = digits, format = "f")
}

#' Helper function to format p-values
#' @keywords internal
format_p <- function(p) {
  ifelse(p < .001, "< .001",
         sub("^0", "", formatC(p, digits = 3, format = "f")))
}

#' Copy table to clipboard
#' @keywords internal
copy_to_clipboard <- function(df) {
  # Convert to tab-separated for easy paste into Excel/Word
  txt <- capture.output(write.table(df, sep = "\t", row.names = FALSE, quote = FALSE))
  txt <- paste(txt, collapse = "\n")

  # Platform-specific clipboard handling
  if (Sys.info()["sysname"] == "Darwin") {
    # macOS
    con <- pipe("pbcopy", "w")
    writeLines(txt, con)
    close(con)
  } else if (Sys.info()["sysname"] == "Windows") {
    # Windows
    writeClipboard(txt)
  } else {
    # Linux (requires xclip)
    tryCatch({
      con <- pipe("xclip -selection clipboard", "w")
      writeLines(txt, con)
      close(con)
    }, error = function(e) {
      warning("Could not copy to clipboard. Install xclip on Linux.")
    })
  }
}
