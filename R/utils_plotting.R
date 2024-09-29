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