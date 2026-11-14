test_that("every Google exampleNumber agrees with phonenumbers 9.0.43 across 5 input forms", {
  df <- readRDS(test_path("_fixtures", "py_examples.rds"))
  expect_identical(nrow(df), 1141L)

  # 1. International input forms (E164, INTERNATIONAL, RFC3966) with region = NULL
  for (col in c("f_e164", "f_intl", "f_rfc")) {
    p <- phone_parse(df[[col]], region = NULL)
    expect_identical(as.character(p$country_code), df$p_cc, info = col)
    expect_identical(p$national_number, df$nsn, info = col)
    expect_identical(ifelse(p$possible, "TRUE", "FALSE"), df$poss, info = col)
    expect_identical(ifelse(p$valid, "TRUE", "FALSE"), df$val, info = col)
    expect_identical(p$type, df$tp, info = col)
    expect_identical(ifelse(is.na(p$region), "", p$region), df$rc, info = col)
    expect_identical(phone_format(p, format = "E164"), df$f_e164, info = col)
    expect_identical(phone_format(p, format = "INTERNATIONAL"), df$f_intl, info = col)
    expect_identical(phone_format(p, format = "NATIONAL"), df$f_nat, info = col)
    expect_identical(phone_format(p, format = "RFC3966"), df$f_rfc, info = col)
  }

  # 2. Raw exampleNumber and NATIONAL formatted string with default region = reg
  groups <- split(seq_len(nrow(df)), df$reg)
  for (i in seq_along(groups)) {
    reg_val <- names(groups)[i]
    idx <- groups[[i]]
    r_arg <- if (nzchar(reg_val)) reg_val else NULL
    for (col in c("inp", "f_nat")) {
      in_vec <- if (col == "f_nat" && is.null(r_arg)) df$f_intl[idx] else df[[col]][idx]
      p <- phone_parse(in_vec, region = r_arg)
      tag <- paste(reg_val, col)
      expect_identical(as.character(p$country_code), df$p_cc[idx], info = tag)
      expect_identical(p$national_number, df$nsn[idx], info = tag)
      expect_identical(ifelse(p$possible, "TRUE", "FALSE"), df$poss[idx], info = tag)
      expect_identical(ifelse(p$valid, "TRUE", "FALSE"), df$val[idx], info = tag)
      expect_identical(p$type, df$tp[idx], info = tag)
      expect_identical(ifelse(is.na(p$region), "", p$region), df$rc[idx], info = tag)
      expect_identical(phone_format(p, format = "E164"), df$f_e164[idx], info = tag)
      expect_identical(phone_format(p, format = "INTERNATIONAL"), df$f_intl[idx], info = tag)
      expect_identical(phone_format(p, format = "NATIONAL"), df$f_nat[idx], info = tag)
      expect_identical(phone_format(p, format = "RFC3966"), df$f_rfc[idx], info = tag)
    }
  }
})
