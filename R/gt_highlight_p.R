
gt_highlight_p <- function(x) {

  out <- x %>%
    gt::tab_style(
      style = gt::cell_text(weight = "bold"),
      locations = gt::cells_body(rows = `p.value` <= 0.05)
    )

  out
}
