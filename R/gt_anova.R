gt_anova <- function(x, pretty = TRUE, highlight = FALSE) {

  out <- x %>%

    # convert to tidy tibble
    broom.mixed::tidy() %>%
    dplyr::mutate(

      # pretty print the p-values
      `p-value` = scales::pvalue(p.value),

      # round all numbers to 2 decimals
      across(where(is.double), round, 2)
    ) %>%

    rename(Model = term) %>%

    select(-p.value)

  # get model with lowest AIC
  # this only pulls out the models name
  # as specified during assignment
  # e.g. m1 <- lmer(y ~ x + (1 | subject), df)
  # here: "m1"
  min_aic <- out %>%
    filter(AIC == min(AIC)) %>%
    pull(Model)

  # get formula from ANOVA object
  # this pulls out the model formula
  # here: "m1: y ~ x + (1 | subject)"
  model_formular <- x %>%
    attr(., "heading") %>%
    str_subset({min_aic})


  # by default the output will be a {gt} table
  if (pretty) {
    out <- out %>%
      # convert to gt-table
      gt::gt() %>%

      # add table footer with model formula of lowest aic model
      gt::tab_source_note(source_note =
                            gt::md(glue::glue("AIC emo::ji('down_arrow') *{model_formular}*")))

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

  # optionally include second model
  # only interesting for comparisons

  out

}
