test_that("hand-written awkward cases match Python phonenumbers 9.0.43", {
  df <- readRDS(test_path("_fixtures", "py_awkward.rds"))

  for (i in seq_len(nrow(df))) {
    r_arg <- if (nzchar(df$reg[i])) df$reg[i] else NULL
    p <- phone_parse(df$s[i], region = r_arg)
    label <- paste0("row ", i, " (", df$s[i], ", reg=", df$reg[i], ")")

    r_cc <- ifelse(is.na(p$country_code), "", as.character(p$country_code))
    r_nsn <- ifelse(is.na(p$national_number), "", p$national_number)
    r_ext <- ifelse(is.na(p$extension), "", p$extension)
    f_intl <- phone_format(p, format = "INTERNATIONAL")
    f_nat <- phone_format(p, format = "NATIONAL")
    f_rfc <- phone_format(p, format = "RFC3966")

    expect_identical(r_cc, df$p_cc[i], info = label)
    expect_identical(r_nsn, df$nsn[i], info = label)
    expect_identical(r_ext, df$ext[i], info = label)
    expect_identical(ifelse(p$possible, "TRUE", "FALSE"), df$poss[i], info = label)
    expect_identical(ifelse(p$valid, "TRUE", "FALSE"), df$val[i], info = label)
    expect_identical(ifelse(is.na(p$type), "", p$type), df$tp[i], info = label)
    expect_identical(ifelse(is.na(p$region), "", p$region), df$rc[i], info = label)
    expect_identical(ifelse(is.na(p$e164), "", p$e164), df$f_e164[i], info = label)
    expect_identical(ifelse(is.na(f_intl), "", f_intl), df$f_intl[i], info = label)
    expect_identical(ifelse(is.na(f_nat), "", f_nat), df$f_nat[i], info = label)
    expect_identical(ifelse(is.na(f_rfc), "", f_rfc), df$f_rfc[i], info = label)
  }
})

test_that("shared calling codes, leading zeros and national prefix transforms work as documented", {
  # Shared calling codes (+1, +44, +7, +61, +39)
  expect_identical(
    phone_region(c("+1 650-253-0000", "+1 416-555-0199", "+44 20 7946 0192", "+44 1534 456789")),
    c("US", "CA", "GB", "JE")
  )

  # Italian leading zero preserved in national_number and E164/NATIONAL formatting
  it <- phone_parse("+39 06 6981")
  expect_identical(it$national_number, "066981")
  expect_identical(it$e164, "+39066981")
  expect_identical(phone_format(it, format = "NATIONAL"), "06 6981")

  # Argentina 0... 15-... mobile transform vs fixed line
  ar <- phone_parse(c("011 15-2345-6789", "011 2345-6789"), region = "AR")
  expect_identical(ar$national_number, c("91123456789", "1123456789"))
  expect_identical(ar$type, c("mobile", "fixed_line"))
  expect_identical(
    phone_format(ar, format = "NATIONAL"),
    c("011 15-2345-6789", "011 2345-6789")
  )
  expect_identical(
    phone_format(ar, format = "INTERNATIONAL"),
    c("+54 9 11 2345-6789", "+54 11 2345-6789")
  )

  # Local-only 7-digit US number is possible but not valid
  expect_true(phone_possible("253-0000", region = "US"))
  expect_false(phone_valid("253-0000", region = "US"))

  # strict = TRUE checks region membership
  expect_true(phone_valid("+1 416-555-0199", region = "US", strict = FALSE))
  expect_false(phone_valid("+1 416-555-0199", region = "US", strict = TRUE))
  expect_true(phone_valid("+1 416-555-0199", region = "CA", strict = TRUE))
})
