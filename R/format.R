format_nsn_vec <- function(nsn, meta, fmt_mode) {
  if (is.null(meta) || length(nsn) == 0L) {
    return(nsn)
  }
  available_formats <- if (fmt_mode == "NATIONAL" || length(meta$intl_number_format) == 0L) {
    meta$number_format
  } else {
    meta$intl_number_format
  }
  if (length(available_formats) == 0L) {
    return(nsn)
  }

  out <- nsn
  rem <- seq_along(nsn)

  for (nf in available_formats) {
    if (length(rem) == 0L) {
      break
    }
    cand_idx <- rem
    if (!is.null(nf$last_leading_digits_re)) {
      ld_ok <- stringi::stri_detect_regex(nsn[cand_idx], nf$last_leading_digits_re)
      ld_ok[is.na(ld_ok)] <- FALSE
      cand_idx <- cand_idx[ld_ok]
    }
    if (length(cand_idx) == 0L) {
      next
    }
    pat_ok <- stringi::stri_detect_regex(nsn[cand_idx], nf$pattern_re)
    pat_ok[is.na(pat_ok)] <- FALSE
    if (any(pat_ok)) {
      hit_idx <- cand_idx[pat_ok]
      rep_rule <- if (fmt_mode == "NATIONAL") nf$nat_format else nf$format
      formatted <- stringi::stri_replace_first_regex(nsn[hit_idx], nf$pattern_re, rep_rule)
      if (fmt_mode == "RFC3966") {
        formatted <- stringi::stri_replace_first_regex(formatted, leading_separator_re, "")
        formatted <- stringi::stri_replace_all_regex(formatted, separator_re, "-")
      }
      out[hit_idx] <- formatted
      rem <- setdiff(rem, hit_idx)
    }
  }
  out
}

#' Format phone numbers in standard representations
#'
#' @description
#' `phone_format()` formats parsed phone numbers according to Google's
#' `libphonenumber` territory rules into `"E164"`, `"INTERNATIONAL"`,
#' `"NATIONAL"` or `"RFC3966"` format.
#'
#' @inheritParams phone_parse
#' @param format A single string naming the target format: `"E164"` (the
#'   default, e.g. `"+41446681800"`), `"INTERNATIONAL"` (`"+41 44 668 1800"`),
#'   `"NATIONAL"` (`"044 668 1800"`), or `"RFC3966"`
#'   (`"tel:+41-44-668-1800"`). Case-insensitive.
#'
#' @returns A character vector the same length as `x`, with `NA_character_`
#'   where `x` is `NA` or cannot be parsed.
#'
#' @seealso [phone_parse()], [phone_valid()].
#' @export
#' @examples
#' x <- c("+41 44 668 1800", "+1 650-253-0000 ext. 1234", "not a number", NA)
#' phone_format(x, format = "E164")
#' phone_format(x, format = "INTERNATIONAL")
#' phone_format(x, format = "NATIONAL")
#' phone_format(x, format = "RFC3966")
phone_format <- function(x, region = NULL,
                         format = c("E164", "INTERNATIONAL", "NATIONAL", "RFC3966")) {
  if (!is.character(format) || length(format) < 1L || is.na(format[1L])) {
    cli::cli_abort(
      paste0(
        "{.arg format} must be one of {.val E164}, {.val INTERNATIONAL}, ",
        "{.val NATIONAL}, or {.val RFC3966}."
      )
    )
  }
  fmt_mode <- rlang::arg_match0(
    toupper(format[1L]),
    c("E164", "INTERNATIONAL", "NATIONAL", "RFC3966"),
    arg_nm = "format"
  )

  if (inherits(x, "dialtone_phone")) {
    cc <- x$country_code
    nsn <- x$national_number
    ext <- x$extension
    idx <- NULL
  } else {
    check_character(x)
    region <- check_region(region, allow_null = TRUE)
    dedup <- parse_core_dedup(x, region)
    if (dedup$n == 0L) {
      return(character())
    }
    cc <- dedup$u_core$country_code
    nsn <- dedup$u_core$national_number
    ext <- dedup$u_core$extension
    idx <- dedup$idx
  }

  n <- length(cc)
  if (n == 0L) {
    return(if (is.null(idx)) character() else rep(NA_character_, length(idx)))
  }
  out <- rep(NA_character_, n)
  parsed <- !is.na(cc) & !is.na(nsn)
  if (!any(parsed)) {
    return(if (is.null(idx)) out else out[idx])
  }

  p_idx <- which(parsed)
  if (fmt_mode == "E164") {
    out[p_idx] <- paste0("+", cc[p_idx], nsn[p_idx])
    return(if (is.null(idx)) out else out[idx])
  }

  cc_groups <- split(p_idx, cc[p_idx])
  for (g_idx in cc_groups) {
    g_cc <- cc[g_idx[1L]]
    cc_key <- as.character(g_cc)
    regions <- cc_to_regions[[cc_key]]
    if (is.null(regions)) {
      out[g_idx] <- nsn[g_idx]
      next
    }
    main_reg <- regions[1L]
    meta <- get_meta_for_region_or_cc(g_cc, main_reg)
    formatted <- format_nsn_vec(nsn[g_idx], meta, fmt_mode)

    g_ext <- ext[g_idx]
    has_ext <- !is.na(g_ext) & nzchar(g_ext)
    if (any(has_ext)) {
      ext_prefix <- if (fmt_mode == "RFC3966") {
        ";ext="
      } else if (!is.null(meta$preferred_extn_prefix)) {
        meta$preferred_extn_prefix
      } else {
        " ext. "
      }
      formatted[has_ext] <- paste0(formatted[has_ext], ext_prefix, g_ext[has_ext])
    }

    if (fmt_mode == "INTERNATIONAL") {
      formatted <- paste0("+", g_cc, " ", formatted)
    } else if (fmt_mode == "RFC3966") {
      formatted <- paste0("tel:+", g_cc, "-", formatted)
    }
    out[g_idx] <- formatted
  }
  if (is.null(idx)) out else out[idx]
}
