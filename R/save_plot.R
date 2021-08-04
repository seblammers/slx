save_plot <- function(path, svg = FALSE) {
  # helper function to save the most recent
  # ggplot as pdf and as png with reasonable defaults

  # input: path like this
  #   here("plots", "plotname")

  # output: a pdf and a png with the {plotname}
  #         in the specified folder {plots}

  # optionally you can set svg to TRUE and save as a svg

  # save as pdf
  ggsave(glue::glue("{path}.pdf"),
         width = 10,
         height = 6,
         device = cairo_pdf)

  # convert to png
  pdftools::pdf_convert(
    pdf = glue::glue("{path}.pdf"),
    format = "png",
    dpi = 250,
    filenames = glue::glue("{path}.png")
  )

  if (svg) {
    ggsave(glue::glue("{path}.svg"),
           width = 10,
           height = 6)
  }
}
