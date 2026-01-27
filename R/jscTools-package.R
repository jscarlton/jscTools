#' jscTools: Miscellaneous Helper Tools for Data Analysis
#'
#' A collection of tools and scripts for data analysis, including functions for
#' importing and cleaning Qualtrics survey data, analyzing Likert scale responses,
#' calculating composite scale scores, creating publication-ready tables, and
#' custom ggplot2 themes.
#'
#' @section Qualtrics Data Import:
#' \itemize{
#'   \item \code{\link{decruftQ}}: Import Qualtrics CSV exports with automatic
#'     format detection, codebook-based renaming, and Likert factor conversion
#' }
#'
#' @section Likert Scale Functions:
#' \itemize{
#'   \item \code{\link{recode_likert}}: Convert text responses to ordered factors
#'   \item \code{\link{recode_likert_cols}}: Batch recode multiple Likert columns
#'   \item \code{\link{plot_likert}}: Visualize Likert responses with diverging bar charts
#'   \item \code{\link{likert_summary}}: Generate summary tables of Likert responses
#' }
#'
#' @section Scale Computation:
#' \itemize{
#'   \item \code{\link{calculate_trust_scale}}: Compute composite scores with reverse coding
#'   \item \code{\link{cronbach_alpha}}: Calculate scale reliability
#' }
#'
#' @section Publication Tables:
#' \itemize{
#'   \item \code{\link{export_table_apa}}: Format regression/correlation tables for APA style
#' }
#'
#' @section Statistical Helpers:
#' \itemize{
#'   \item \code{\link{stderr}}: Calculate standard error of the mean
#'   \item \code{\link{ci_high}}: Calculate upper bound of 95\% CI
#'   \item \code{\link{ci_low}}: Calculate lower bound of 95\% CI
#' }
#'
#' @section Visualization:
#' \itemize{
#'   \item \code{\link{theme_fira}}: Custom ggplot2 theme using Fira fonts
#'   \item \code{\link{theme_hmr}}: Half Moon Reef project theme
#'   \item \code{\link{multiplot}}: Display multiple ggplot2 objects in a grid
#' }
#'
#' @section Text Analysis:
#' \itemize{
#'   \item \code{\link{transcript_cloud}}: Generate word clouds from text transcripts
#'   \item \code{\link{wordCloudExport}}: Generate word clouds from survey responses
#' }
#'
#' @keywords internal
#' @import dplyr
#' @import ggplot2
#' @importFrom readr read_csv read_lines
#' @importFrom stringr str_to_title str_trim str_wrap
#' @importFrom tibble tibble as_tibble rownames_to_column
#' @importFrom tidyr pivot_longer pivot_wider
#' @importFrom scales percent_format
#' @importFrom stats coef confint vcov cor.test var median complete.cases
#' @importFrom rlang enquo
#' @importFrom grid unit
"_PACKAGE"
