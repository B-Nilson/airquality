#' Save a ggplot2 figure 
#'
#' @param gg a single ggplot object you would like to save.
#' @param out_path A single character value indicating where to save the plot to (i.e. "./plots/my_figure_name.png")
#' @param taller A single numeric value indicating how many inches to add/remove to the output plot height. Default makes a 5"x6.5" image.
#' @param page_width A single numeric value indicating the width of the page (minus margins) in inches that the image is intended for.
#' @param quality A single character value equal to "high" (dpi = 300), "medium" (dpi = 200), or "low" (dpi = 100) indicating the output quality of the figure. 
#'   Text sizes may need to be adjusted for differenct quality levels. 
#' @param ... (Optional) addition arguments passed on to ggplot2::ggsave()
#'
#' @description
#' This provides a relatviley simple way to quickly save a ggplot figure as a png etc. 
#' `save_figure()` is a wrapper around `ggplot2::ggsave()` that saves a high quality 5x7 figure by default. 
#' You can adjust the sizing from there by changing `wider` and `taller`,
#' and you can quickly adjust to a lower quality if file size matters.
#'
#' @family Data Visualisation
#'
#' @return invisible NULL
#' @export
#'
#' @examples
#' gg = ggplot2::ggplot() + 
#'   ggplot2::geom_line(
#'     data = data.frame(x = 1:100, y = (0:99)^2), 
#'     ggplot2::aes(x, y))
#' # save_figure(gg, "./test.png", taller = 1)
save_figure = function(gg, out_path, taller = 0, page_width = 6.5, quality = "high", ...) {
  dpi = if (quality == "high") 300 else if (quality == "medium") 200 else 100
  ggplot2::ggsave(
    filename = out_path,
    plot = gg, 
    width = page_width, 
    height = 5 + taller,
    units = "in", 
    dpi = dpi,
    ...)
  return(invisible(NULL))
}

add_default_theme = function(gg) {
  gg  +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.direction = "horizontal",
      legend.position = "bottom",
      strip.text = ggplot2::element_text(
        size = 12, face = "italic", 
        margin = ggplot2::margin(b = 5)),
      plot.background = ggplot2::element_rect(
        fill = "white", colour = "black"),
      panel.border = ggplot2::element_rect(
        colour = NA, fill = NA))
}

facet_plot = function(gg, by = NULL, rows = 1) {
  if (is.null(by)) return(gg)
  gg + 
    ggplot2::facet_wrap(
      by, nrow = rows,
      axes = "all",
      labeller = "label_both")
}
