number_type_vec <- function(nsn, meta) {
  n <- length(nsn)
  out <- rep("unknown", n)
  if (is.null(meta) || n == 0L) {
    return(out)
  }
  rem <- which(match_desc_vec(nsn, meta$general_desc))
  if (length(rem) == 0L) {
    return(out)
  }

  ordered_types <- c(
    "premium_rate", "toll_free", "shared_cost", "voip",
    "personal_number", "pager", "uan", "voicemail"
  )
  for (tp in ordered_types) {
    desc <- meta[[tp]]
    if (!is.null(desc)) {
      hit <- match_desc_vec(nsn[rem], desc)
      if (any(hit)) {
        out[rem[hit]] <- tp
        rem <- rem[!hit]
        if (length(rem) == 0L) {
          return(out)
        }
      }
    }
  }

  fl <- match_desc_vec(nsn[rem], meta$fixed_line)
  if (isTRUE(meta$same_mobile_and_fixed_line_pattern)) {
    if (any(fl)) {
      out[rem[fl]] <- "fixed_line_or_mobile"
    }
  } else {
    mb <- match_desc_vec(nsn[rem], meta$mobile)
    out[rem[fl & mb]] <- "fixed_line_or_mobile"
    out[rem[fl & !mb]] <- "fixed_line"
    out[rem[!fl & mb]] <- "mobile"
  }
  out
}

resolve_region_and_type_unique <- function(cc, nsn) {
  n <- length(cc)
  reg_out <- rep(NA_character_, n)
  type_out <- rep(NA_character_, n)
  parsed <- !is.na(cc) & !is.na(nsn)
  if (!any(parsed)) {
    return(list(region = reg_out, type = type_out))
  }

  p_idx <- which(parsed)
  type_out[p_idx] <- "unknown"

  cc_groups <- split(p_idx, cc[p_idx])
  for (g_idx in cc_groups) {
    g_cc <- cc[g_idx[1L]]
    cc_key <- as.character(g_cc)
    regions <- cc_to_regions[[cc_key]]
    if (is.null(regions)) {
      next
    }
    if (length(regions) == 1L) {
      r_code <- regions[1L]
      reg_out[g_idx] <- r_code
      meta <- get_meta_for_region_or_cc(g_cc, r_code)
      type_out[g_idx] <- number_type_vec(nsn[g_idx], meta)
    } else {
      rem <- g_idx
      for (r_code in regions) {
        if (length(rem) == 0L) {
          break
        }
        meta <- region_meta[[r_code]]
        if (is.null(meta)) {
          next
        }
        if (!is.null(meta$leading_digits_re)) {
          ld_hit <- stringi::stri_detect_regex(nsn[rem], meta$leading_digits_re)
          ld_hit[is.na(ld_hit)] <- FALSE
          if (any(ld_hit)) {
            hit_idx <- rem[ld_hit]
            reg_out[hit_idx] <- r_code
            type_out[hit_idx] <- number_type_vec(nsn[hit_idx], meta)
            rem <- rem[!ld_hit]
          }
        } else {
          tp <- number_type_vec(nsn[rem], meta)
          valid_hit <- tp != "unknown"
          if (any(valid_hit)) {
            hit_idx <- rem[valid_hit]
            reg_out[hit_idx] <- r_code
            type_out[hit_idx] <- tp[valid_hit]
            rem <- rem[!valid_hit]
          }
        }
      }
    }
  }
  list(region = reg_out, type = type_out)
}

resolve_region_and_type <- function(core) {
  n <- length(core$country_code)
  if (n == 0L) {
    return(list(region = character(), type = character()))
  }
  parsed <- !is.na(core$country_code) & !is.na(core$national_number)
  reg_out <- rep(NA_character_, n)
  type_out <- rep(NA_character_, n)
  if (!any(parsed)) {
    return(list(region = reg_out, type = type_out))
  }

  key <- paste0(core$country_code[parsed], ":", core$national_number[parsed])
  u_key <- unique(key)
  first_pos <- match(u_key, key)
  p_idx <- which(parsed)
  u_res <- resolve_region_and_type_unique(
    core$country_code[p_idx[first_pos]],
    core$national_number[p_idx[first_pos]]
  )
  back <- match(key, u_key)
  reg_out[parsed] <- u_res$region[back]
  type_out[parsed] <- u_res$type[back]
  list(region = reg_out, type = type_out)
}

compute_possible <- function(core) {
  n <- length(core$country_code)
  if (n == 0L) {
    return(logical())
  }
  out <- rep(FALSE, n)
  out[core$is_na] <- NA
  parsed <- !is.na(core$country_code) & !is.na(core$national_number)
  if (!any(parsed)) {
    return(out)
  }
  p_idx <- which(parsed)
  cc_groups <- split(p_idx, core$country_code[p_idx])
  for (g_idx in cc_groups) {
    g_cc <- core$country_code[g_idx[1L]]
    cc_key <- as.character(g_cc)
    regions <- cc_to_regions[[cc_key]]
    if (is.null(regions)) {
      next
    }
    main_reg <- regions[1L]
    meta <- get_meta_for_region_or_cc(g_cc, main_reg)
    if (!is.null(meta)) {
      v_res <- test_number_length_vec(core$national_number[g_idx], meta)
      out[g_idx] <- v_res %in% c("IS_POSSIBLE", "IS_POSSIBLE_LOCAL_ONLY")
    }
  }
  out
}

#' Test whether strings are possible or valid phone numbers
#'
#' @description
#' * `phone_possible()` checks length only (matching
#'   `libphonenumber`'s `isPossibleNumber()`): whether the number has a length
#'   that exists for its country calling code, including local-only numbers.
#' * `phone_valid()` checks the full numbering plan (matching
#'   `libphonenumber`'s `isValidNumber()`): country calling code, prefix digits
#'   and length.
#'
#' A `TRUE` from `phone_valid()` means the number is well-formed under that
#' country's numbering plan. It does not mean the number was assigned or is
#' active.
#'
#' @param x A character vector of phone numbers (or a `dialtone_phone` data
#'   frame from [phone_parse()]).
#' @param region A single two-letter CLDR region code (such as `"US"`, `"GB"` or
#'   `"IN"`) giving the default country for numbers written without a leading
#'   `+`, or `NULL` when numbers carry an international `+` prefix.
#' @param strict Logical scalar. When `FALSE` (the default), `region` is the
#'   default region used to parse national-format strings, and any valid
#'   international number returns `TRUE`. When `TRUE`, `region` must be non-null
#'   and the number must also belong to `region` (matching
#'   `isValidNumberForRegion()`).
#'
#' @returns A logical vector the same length as `x`: `TRUE`, `FALSE`, or `NA`
#'   where `x` is `NA`. Malformed values return `FALSE`, never an error.
#'
#' @seealso [phone_parse()], [phone_type()], [phone_region()], [phone_format()].
#' @export
#' @examples
#' phone_possible(c("+1 650-253-0000", "+1 111-111-1111", "123", NA))
#' phone_valid(c("+1 650-253-0000", "+1 111-111-1111", "123", NA))
#'
#' # National format requires a default region
#' phone_valid(c("020 7946 0192", "020 0000 0000"), region = "GB")
#'
#' # strict = TRUE requires the number to belong to `region`
#' phone_valid(c("+1 650-253-0000", "+1 416-555-0199"), region = "US", strict = TRUE)
phone_valid <- function(x, region = NULL, strict = FALSE) {
  if (!is.logical(strict) || length(strict) != 1L || is.na(strict)) {
    cli::cli_abort("{.arg strict} must be {.code TRUE} or {.code FALSE}.")
  }
  if (strict) {
    region <- check_region(region, allow_null = FALSE)
  } else {
    region <- check_region(region, allow_null = TRUE)
  }

  if (inherits(x, "dialtone_phone")) {
    if (!strict) {
      return(x$valid)
    }
    is_na <- is.na(x$valid)
    cc <- x$country_code
    nsn <- x$national_number
    n <- length(cc)
    if (n == 0L) {
      return(logical())
    }
    out <- rep(FALSE, n)
    out[is_na] <- NA
    meta <- region_meta[[region]]
    same_cc <- !is.na(cc) & !is.na(nsn) & cc == meta$country_code
    if (any(same_cc)) {
      idx <- which(same_cc)
      tp <- number_type_vec(nsn[idx], meta)
      out[idx] <- tp != "unknown"
    }
    return(out)
  }

  check_character(x)
  dedup <- parse_core_dedup(x, region)
  if (dedup$n == 0L) {
    return(logical())
  }
  u_core <- dedup$u_core
  if (!strict) {
    u_resolved <- resolve_region_and_type(u_core)
    u_valid <- !is.na(u_resolved$type) & u_resolved$type != "unknown"
    return(u_valid[dedup$idx])
  }

  u_cc <- u_core$country_code
  u_nsn <- u_core$national_number
  u_out <- rep(FALSE, length(u_cc))
  meta <- region_meta[[region]]
  same_cc <- !is.na(u_cc) & !is.na(u_nsn) & u_cc == meta$country_code
  if (any(same_cc)) {
    s_idx <- which(same_cc)
    tp <- number_type_vec(u_nsn[s_idx], meta)
    u_out[s_idx] <- tp != "unknown"
  }
  u_out[dedup$idx]
}

#' @rdname phone_valid
#' @export
phone_possible <- function(x, region = NULL) {
  if (inherits(x, "dialtone_phone")) {
    return(x$possible)
  }
  check_character(x)
  region <- check_region(region, allow_null = TRUE)
  dedup <- parse_core_dedup(x, region)
  if (dedup$n == 0L) {
    return(logical())
  }
  u_poss <- compute_possible(dedup$u_core)
  u_poss[dedup$idx]
}

#' Classify phone numbers by type or region
#'
#' @description
#' * `phone_type()` returns the number type (`"fixed_line"`, `"mobile"`,
#'   `"fixed_line_or_mobile"`, `"toll_free"`, `"premium_rate"`, `"shared_cost"`,
#'   `"voip"`, `"personal_number"`, `"pager"`, `"uan"`, `"voicemail"`, or
#'   `"unknown"` when parsed but not valid under the numbering plan).
#' * `phone_region()` returns the two-letter CLDR region code (or `"001"` for
#'   non-geographic calling codes such as `+800`).
#'
#' @inheritParams phone_parse
#'
#' @returns A character vector the same length as `x`, with `NA_character_`
#'   where `x` is `NA` or cannot be parsed.
#'
#' @seealso [phone_valid()], [phone_parse()], [phone_regions()].
#' @export
#' @examples
#' phone_type(c("+44 20 7946 0192", "+44 7700 900077", "+1 800-234-5678", "not a number"))
#' phone_region(c("+1 650-253-0000", "+1 416-555-0199", "+44 20 7946 0192", "+800 1234 5678"))
phone_type <- function(x, region = NULL) {
  if (inherits(x, "dialtone_phone")) {
    return(x$type)
  }
  check_character(x)
  region <- check_region(region, allow_null = TRUE)
  dedup <- parse_core_dedup(x, region)
  if (dedup$n == 0L) {
    return(character())
  }
  u_tp <- resolve_region_and_type(dedup$u_core)$type
  u_tp[dedup$idx]
}

#' @rdname phone_type
#' @export
phone_region <- function(x, region = NULL) {
  if (inherits(x, "dialtone_phone")) {
    return(x$region)
  }
  check_character(x)
  region <- check_region(region, allow_null = TRUE)
  dedup <- parse_core_dedup(x, region)
  if (dedup$n == 0L) {
    return(character())
  }
  u_reg <- resolve_region_and_type(dedup$u_core)$region
  u_reg[dedup$idx]
}

#' List supported regions and calling codes
#'
#' @description
#' `phone_regions()` returns the table of territories in the bundled
#' `libphonenumber` metadata (`v9.0.43`).
#'
#' @returns A data frame with one row per territory (245 geographical regions
#'   and 9 non-geographic `"001"` calling codes) and the columns:
#'   * `region`: two-letter CLDR region code, or `"001"` for non-geographic
#'     entities.
#'   * `country_code`: integer country calling code.
#'   * `main_country`: logical; `TRUE` for the primary region of a shared
#'     country calling code.
#'   * `national_prefix`: national trunk prefix, or `NA_character_`.
#'   * `example_fixed_line`: upstream example fixed-line number, or
#'     `NA_character_`.
#'   * `example_mobile`: upstream example mobile number, or `NA_character_`.
#'
#' @export
#' @examples
#' head(phone_regions())
phone_regions <- function() {
  regions_df
}
