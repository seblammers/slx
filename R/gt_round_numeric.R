gt_round_numeric <- function(df) {

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
  if (is.character(df$p.value)) {
    out <- df %>%
      mutate(
        # rename existing column for consistency
        p_value = p.value,
        # extract only the numeric part for highlighting
        p.value = readr::parse_number(p.value),
        # round all numbers
        across(where(is.numeric), round, 2)
      )
  } else {

    # in case the p-value column is numeric
    # we add a new column with labels and keep the original
    # for highlighting
    out <- df %>%
      mutate(
        # add a beautified labelled version
        # (and keep the numeric one for highlighting purposes)
        p_value = scales::pvalue(p.value),
        # round all numbers
        across(where(is.numeric), round, 2)
      )
  }

  # lastly, convert the df to gt and hide the numeric
  # p-value column
  out <- out %>%
    gt::gt() %>%
    # hide the numeric version of the p-values
    # it will not be displayed but can be used to highlight
    # where p <= .05
    gt::cols_hide(
      columns = p.value
    )

  # return gt table
  out

}
