gt_round_numeric <- function(x) {

  # add a new column called p-value that has the usual
  # p-value-labels (e.g. < .05)
  # keep the existing numeric p-value for highlighting
  #
  # round all numeric values down to 2 decimals
  out <- x %>%
    mutate(
      `p-value` = scales::pvalue(p.value),
      across(where(is.numeric), round, 2)
    ) %>%
    gt::gt() %>%
    # hide the numeric version
    # it will not be displayed but can be used to highlight
    # where p <= .05
    gt::cols_hide(
      columns = p.value
    )

  out

}
