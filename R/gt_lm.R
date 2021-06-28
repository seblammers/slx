gt_lm <- function(x, pretty = TRUE, highlight = FALSE) {

  out <- x %>%

    # convert to tidy tibble
    broom.mixed::tidy() %>%
    dplyr::mutate(

      # pretty print the p-values
      `p-value` = scales::pvalue(p.value),

      # round all numbers to 2 decimals
      across(where(is.double), round, 2)
    ) %>%

    select(-p.value)

  if (pretty) {
    out <- out %>%
      # convert to gt-table
      gt::gt()

    if (highlight) {
      out <- out %>%

        # optionally highlight rows with sign. p-values
        gt::tab_style(
          style = gt::cell_text(weight = "bold"),
          locations = gt::cells_body(
            rows = `p-value` <= 0.05)
        )
    }
  }

  out

}

