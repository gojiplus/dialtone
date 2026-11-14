test_that("NA in gives NA out", {
  x <- c(NA, "+1 650-253-0000", NA_character_)
  expect_identical(phone_possible(x), c(NA, TRUE, NA))
  expect_identical(phone_valid(x), c(NA, TRUE, NA))
  expect_identical(phone_valid(x, region = "US", strict = TRUE), c(NA, TRUE, NA))
  expect_identical(phone_type(x), c(NA, "fixed_line_or_mobile", NA))
  expect_identical(phone_region(x), c(NA, "US", NA))
  expect_identical(phone_format(x, format = "E164"), c(NA, "+16502530000", NA))
  expect_identical(phone_format(x, format = "INTERNATIONAL"), c(NA, "+1 650-253-0000", NA))
  expect_identical(phone_format(x, format = "NATIONAL"), c(NA, "(650) 253-0000", NA))
  expect_identical(phone_format(x, format = "RFC3966"), c(NA, "tel:+1-650-253-0000", NA))

  p <- phone_parse(x)
  expect_identical(is.na(p$country_code), c(TRUE, FALSE, TRUE))
  expect_identical(is.na(p$national_number), c(TRUE, FALSE, TRUE))
  expect_identical(p$possible, c(NA, TRUE, NA))
  expect_identical(p$valid, c(NA, TRUE, NA))
})

test_that("empty input gives empty output of the right type", {
  expect_identical(phone_possible(character()), logical())
  expect_identical(phone_valid(character()), logical())
  expect_identical(phone_valid(character(), region = "US", strict = TRUE), logical())
  expect_identical(phone_type(character()), character())
  expect_identical(phone_region(character()), character())
  expect_identical(phone_format(character()), character())

  p <- phone_parse(character())
  expect_s3_class(p, "dialtone_phone")
  expect_identical(nrow(p), 0L)
})

test_that("hostile values return FALSE or NA, never an error", {
  x <- hostile_values()
  expect_no_condition(poss <- phone_possible(x, region = "US"))
  expect_identical(poss, rep(FALSE, length(x)))

  expect_no_condition(val <- phone_valid(x, region = "US"))
  expect_identical(val, rep(FALSE, length(x)))

  expect_no_condition(tp <- phone_type(x, region = "US"))
  expect_identical(tp, rep(NA_character_, length(x)))

  expect_no_condition(reg <- phone_region(x, region = "US"))
  expect_identical(reg, rep(NA_character_, length(x)))

  expect_no_condition(fmt <- phone_format(x, region = "US"))
  expect_identical(fmt, rep(NA_character_, length(x)))
})

test_that("already-parsed dialtone_phone objects can be passed to downstream functions", {
  x <- c("+44 20 7946 0192", "+1 416-555-0199", "junk", NA)
  p <- phone_parse(x)
  expect_identical(phone_parse(p), p)
  expect_identical(phone_possible(p), phone_possible(x))
  expect_identical(phone_valid(p), phone_valid(x))
  expect_identical(phone_valid(p, region = "GB", strict = TRUE), c(TRUE, FALSE, FALSE, NA))
  expect_identical(phone_type(p), phone_type(x))
  expect_identical(phone_region(p), phone_region(x))
  expect_identical(
    phone_format(p, format = "INTERNATIONAL"),
    phone_format(x, format = "INTERNATIONAL")
  )
})

test_that("non-character input and invalid arguments raise informative errors", {
  expect_error(phone_valid(6502530000), "must be a character vector", class = "rlang_error")
  expect_error(phone_parse(factor("+16502530000")), "must be a character vector")
  expect_error(phone_possible(123L), "must be a character vector")
  expect_error(phone_valid("+16502530000", region = "XX"), "not a supported CLDR region code")
  expect_error(phone_valid("+16502530000", region = c("US", "GB")), "single two-letter region code")
  expect_error(phone_valid("+16502530000", strict = TRUE), "not `NULL`")
  expect_error(phone_valid("+16502530000", region = "US", strict = NA), "must be `TRUE` or `FALSE`")
  expect_error(phone_format("+16502530000", format = "UNKNOWN"), "must be one of")
  expect_error(phone_format("+16502530000", format = NA_character_), "must be one of")
})

test_that("phone_regions returns all 254 territories", {
  regs <- phone_regions()
  expect_s3_class(regs, "data.frame")
  expect_identical(nrow(regs), 254L)
  expect_identical(
    names(regs),
    c(
      "region", "country_code", "main_country",
      "national_prefix", "example_fixed_line", "example_mobile"
    )
  )
  expect_identical(sum(regs$region == "001"), 9L)
  expect_identical(sum(regs$region != "001"), 245L)
})
