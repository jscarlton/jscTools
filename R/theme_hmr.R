#' A theme for plots for the Half Moon Reef paper
#' Based on ggthemes' fivethirtyeight, but only in that I used that as a shell
#' @param base_size Base font size for the plot. Defaults to 12.
#' @param base_family Base font family. Defaults to Whitney Medium.
#' @param title_family Family for the title font. Defaults to Whitney Semibold
#' @export

theme_hmr <- function(base_size = 12, base_family = "Whitney-Medium", title_family = "Whitney-Semibold") {
  theme(
     line = element_line(colour = "black", size = rel(.75)),
     rect = element_rect(fill = "white",
                         linetype = 0, colour = NA),
     #text = element_text(colour = ggthemes_data$fivethirtyeight["dkgray"]),
     text = element_text(size = base_size, family = base_family),
     axis.title = element_text(),
     axis.text = element_text(),
     axis.ticks = element_blank(),
     axis.line = element_blank(),
     legend.background = element_rect(),
     legend.position = "bottom",
     legend.direction = "horizontal",
     legend.box = "vertical",
     legend.text = element_text(size = rel(.6)),
     legend.title = element_text(size = rel(.75)),
     panel.background = element_rect(fill = "white"),
     panel.grid = element_line(colour = NULL),
     panel.grid.major =
       element_line(colour = rgb(210, 210, 210, max = 255)),
     panel.grid.minor = element_blank(),
     plot.caption = element_text(face = "italic", size = rel(0.75)),
     plot.title = element_text(hjust = 0, size = rel(1.25), family = title_family),
     plot.margin = unit(c(1, 1, 1, 1), "lines"),
     strip.background = element_rect())
}
