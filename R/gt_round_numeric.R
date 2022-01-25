gt_round_numeric <- function(x) {

  # add a new column called p-value that has the usual
  # p-value-labels (e.g. < .05)
  # keep the existing numeric p-value for highlighting
  #
  # round all numeric values down to 2 decimals
  #
  # first of:
  # check if the current tbl has a column named p.value
  # that IS character (e.g. when receiving a tbl from
  # afex::aov() -> afex::nice())
  # if so convert to numeric and add the other column further below
  # as we would if it were numeric to begin with
  if (is.character(x$p.value)) {
    x <- x %>%
      mutate(
        # extract only the numeric part
        p.value = readr::parse_number(p.value)
      )
  }

  out <- x %>%
    mutate(
      # add a beautified labled version
      # and keep the numeric one for highlighting purposes
      p_value = scales::pvalue(p.value),
      # round all numbers
      across(where(is.numeric), round, 2)
    ) %>%
    gt::gt() %>%
    # hide the numeric version of the p-values
    # it will not be displayed but can be used to highlight
    # where p <= .05
    gt::cols_hide(
      columns = p.value
    )

  out

}
