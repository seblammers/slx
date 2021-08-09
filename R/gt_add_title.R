
gt_add_title <- function(x, text) {

  out <- x %>%
    gt::tab_header(
      title = text
    )

  out
}
