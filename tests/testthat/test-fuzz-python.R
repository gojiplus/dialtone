test_that("20,000 random and perturbed numbers agree with Python phonenumbers 9.0.43", {
  df <- readRDS(test_path("_fixtures", "py_fuzz20k.rds"))
  expect_identical(nrow(df), 20000L)

  groups <- split(seq_len(nrow(df)), df$reg)
  for (i in seq_along(groups)) {
    reg_val <- names(groups)[i]
    idx <- groups[[i]]
    r_arg <- if (nzchar(reg_val)) reg_val else NULL
    p <- phone_parse(df$s[idx], region = r_arg)

    f_intl_raw <- phone_format(p, format = "INTERNATIONAL")
    f_nat_raw <- phone_format(p, format = "NATIONAL")
    f_rfc_raw <- phone_format(p, format = "RFC3966")

    r_cc <- ifelse(is.na(p$country_code), "", as.character(p$country_code))
    r_nsn <- ifelse(is.na(p$national_number), "", p$national_number)
    r_ext <- ifelse(is.na(p$extension), "", p$extension)
    r_poss <- ifelse(p$possible, "TRUE", "FALSE")
    r_val <- ifelse(p$valid, "TRUE", "FALSE")
    r_tp <- ifelse(is.na(p$type), "", p$type)
    r_rc <- ifelse(is.na(p$region), "", p$region)
    f_e164 <- ifelse(is.na(p$e164), "", p$e164)
    f_intl <- ifelse(is.na(f_intl_raw), "", f_intl_raw)
    f_nat <- ifelse(is.na(f_nat_raw), "", f_nat_raw)
    f_rfc <- ifelse(is.na(f_rfc_raw), "", f_rfc_raw)

    expect_identical(r_cc, df$p_cc[idx], info = reg_val)
    expect_identical(r_nsn, df$nsn[idx], info = reg_val)
    expect_identical(r_ext, df$ext[idx], info = reg_val)
    expect_identical(r_poss, df$poss[idx], info = reg_val)
    expect_identical(r_val, df$val[idx], info = reg_val)
    expect_identical(r_tp, df$tp[idx], info = reg_val)
    expect_identical(r_rc, df$rc[idx], info = reg_val)
    expect_identical(f_e164, df$f_e164[idx], info = reg_val)
    expect_identical(f_intl, df$f_intl[idx], info = reg_val)
    expect_identical(f_nat, df$f_nat[idx], info = reg_val)
    expect_identical(f_rfc, df$f_rfc[idx], info = reg_val)
  }
})
