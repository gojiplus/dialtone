# Constants and regular expressions ported from Google's libphonenumber (v9.0.43)
min_length_for_nsn <- 2L
max_length_for_nsn <- 17L
max_length_country_code <- 3L
max_input_string_length <- 250L

plus_chars <- "+\uFF0B"
plus_chars_re <- "^[+\uFF0B]+"

valid_punctuation <- paste0(
  "-x\u2010-\u2015\u2212\u30FC\uFF0D-\uFF0F ",
  "\u00A0\u00AD\u200B\u2060\u3000()\uFF08\uFF09\uFF3B\uFF3D.\\[\\]/~\u2053\u223C\uFF5E"
)
separator_re <- paste0("[", valid_punctuation, "]+")
leading_separator_re <- paste0("^[", valid_punctuation, "]+")

digits_re <- "\\p{Nd}"
capturing_digit_re <- "(\\p{Nd})"
valid_alpha <- "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"

valid_start_char_re <- paste0("[", plus_chars, digits_re, "]")
second_number_start_re <- "[\\\\/] *x.*$"
unwanted_end_char_re <- "(?:_|[^#\\p{L}\\p{N}])+$"
valid_alpha_phone_re <- "^(?:.*?[A-Za-z]){3}.*$"

valid_phone_number_core <- paste0(
  digits_re, "{", min_length_for_nsn, "}|",
  "[", plus_chars, "]*(?:[", valid_punctuation, "*]*", digits_re, "){3,}[",
  valid_punctuation, "*", valid_alpha, digits_re, "]*"
)

create_extn_pattern <- function(for_parsing = TRUE) {
  ext_limit_after_explicit_label <- 20L
  ext_limit_after_likely_label <- 15L
  ext_limit_after_ambiguous_char <- 9L
  ext_limit_when_not_sure <- 6L

  sep_before <- "[ \u00A0\\t,]*"
  chars_after <- "[:\\.\uFF0E]?[ \u00A0\\t,-]*"
  opt_suffix <- "#?"

  explicit_labels <- paste0(
    "(?:e?xt(?:ensi(?:o\u0301?|\u00F3))?n?|",
    "\uFF45?\uFF58\uFF54\uFF4E?|\u0434\u043E\u0431|anexo)"
  )
  ambiguous_labels <- "(?:[x\uFF58#\uFF03~\uFF5E]|int|\uFF49\uFF4E\uFF54)"
  ambiguous_sep <- "[- ]+"

  extn_digits <- function(n) paste0("(", digits_re, "{1,", n, "})")

  rfc_extn <- paste0(";ext=", extn_digits(ext_limit_after_explicit_label))
  explicit_extn <- paste0(
    sep_before, explicit_labels, chars_after,
    extn_digits(ext_limit_after_explicit_label), opt_suffix
  )
  ambiguous_extn <- paste0(
    sep_before, ambiguous_labels, chars_after,
    extn_digits(ext_limit_after_ambiguous_char), opt_suffix
  )
  american_extn <- paste0(
    ambiguous_sep, extn_digits(ext_limit_when_not_sure), "#"
  )

  base <- paste(rfc_extn, explicit_extn, ambiguous_extn, american_extn, sep = "|")
  if (!for_parsing) {
    return(base)
  }

  sep_no_comma <- "[ \u00A0\\t]*"
  auto_dialling <- paste0(
    sep_no_comma, "(?:,{2}|;)", chars_after,
    extn_digits(ext_limit_after_likely_label), opt_suffix
  )
  only_commas <- paste0(
    sep_no_comma, "(?:,)+", chars_after,
    extn_digits(ext_limit_after_ambiguous_char), opt_suffix
  )
  paste(base, auto_dialling, only_commas, sep = "|")
}

extn_patterns_for_parsing <- create_extn_pattern(TRUE)
extn_re <- paste0("(?i)(?:", extn_patterns_for_parsing, ")$")
valid_phone_number_re <- paste0(
  "(?is)^(?:", valid_phone_number_core, ")(?:", extn_patterns_for_parsing, ")?$"
)

rfc3966_global_digits_re <- paste0(
  "^\\+(?:", digits_re, "|[\\-\\.\\(\\)])*", digits_re,
  "(?:", digits_re, "|[\\-\\.\\(\\)])*$"
)
alphanum_re <- paste0(valid_alpha, digits_re)
rfc3966_domainlabel_re <- paste0("[", alphanum_re, "]+(?:-*[", alphanum_re, "])*")
rfc3966_toplabel_re <- paste0("[", valid_alpha, "]+(?:-*[", alphanum_re, "])*")
rfc3966_domainname_re <- paste0(
  "^(?:", rfc3966_domainlabel_re, "\\.)*", rfc3966_toplabel_re, "\\.?$"
)

check_character <- function(x, arg = rlang::caller_arg(x), call = rlang::caller_env()) {
  if (!is.character(x)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a character vector, not {.obj_type_friendly {x}}.",
        i = "Numbers stored as numeric have already lost leading zeros; read them as text."
      ),
      call = call
    )
  }
}

check_region <- function(region, allow_null = TRUE, arg = rlang::caller_arg(region),
                         call = rlang::caller_env()) {
  if (is.null(region)) {
    if (allow_null) {
      return(NULL)
    }
    cli::cli_abort("{.arg {arg}} must be a two-letter region code, not {.code NULL}.", call = call)
  }
  if (!is.character(region) || length(region) != 1L || is.na(region)) {
    cli::cli_abort(
      "{.arg {arg}} must be a single two-letter region code or {.code NULL}.",
      call = call
    )
  }
  region <- toupper(region)
  if (!exists(region, envir = region_meta, inherits = FALSE)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} {.val {region}} is not a supported CLDR region code.",
        i = "See {.fn phone_regions} for supported regions."
      ),
      call = call
    )
  }
  region
}

get_meta_for_region_or_cc <- function(cc, region) {
  if (identical(region, "001")) {
    nongeo_meta[[as.character(cc)]]
  } else if (!is.null(region) && !is.na(region) &&
               exists(region, envir = region_meta, inherits = FALSE)) {
    region_meta[[region]]
  } else {
    NULL
  }
}

normalize_digits_vec <- function(x) {
  out <- rep("", length(x))
  non_empty <- nzchar(x) & !is.na(x)
  if (!any(non_empty)) {
    return(out)
  }
  sub_x <- x[non_empty]
  is_ascii <- stringi::stri_enc_isascii(sub_x)
  if (any(is_ascii)) {
    out_ascii <- gsub("[^0-9]+", "", sub_x[is_ascii])
    out[non_empty][is_ascii] <- out_ascii
  }
  if (any(!is_ascii)) {
    u_sub <- sub_x[!is_ascii]
    digits_only <- stringi::stri_replace_all_regex(u_sub, "[^\\p{Nd}]+", "")
    out[non_empty][!is_ascii] <- stringi::stri_trans_general(
      digits_only, "Any-Latin; Latin-ASCII"
    )
  }
  out
}

normalize_alpha_vec <- function(x) {
  upper <- toupper(x)
  mapped <- chartr("ABCDEFGHIJKLMNOPQRSTUVWXYZ", "22233344455566677778889999", upper)
  gsub("[^0-9]+", "", mapped)
}

normalize_phone_vec <- function(x) {
  is_alpha <- stringi::stri_detect_regex(x, valid_alpha_phone_re)
  is_alpha[is.na(is_alpha)] <- FALSE
  out <- character(length(x))
  if (any(is_alpha)) {
    out[is_alpha] <- normalize_alpha_vec(x[is_alpha])
  }
  if (any(!is_alpha)) {
    out[!is_alpha] <- normalize_digits_vec(x[!is_alpha])
  }
  out
}

build_national_number_vec <- function(x) {
  n <- length(x)
  out <- rep(NA_character_, n)
  pc_loc <- stringi::stri_locate_first_fixed(x, ";phone-context=")
  has_pc <- !is.na(pc_loc[, 1L])

  if (any(has_pc)) {
    idx <- which(has_pc)
    for (i in idx) {
      s <- x[i]
      pc_start <- pc_loc[i, 1L]
      pc_val_start <- pc_loc[i, 2L] + 1L
      s_len <- nchar(s)
      if (pc_val_start > s_len) {
        next
      }
      rest <- substr(s, pc_val_start, s_len)
      semi_pos <- regexpr(";", rest, fixed = TRUE)[1L]
      pc_val <- if (semi_pos > 0L) substr(rest, 1L, semi_pos - 1L) else rest
      if (!nzchar(pc_val)) {
        next
      }
      valid_pc <- stringi::stri_detect_regex(pc_val, rfc3966_global_digits_re) ||
        stringi::stri_detect_regex(pc_val, rfc3966_domainname_re)
      if (!isTRUE(valid_pc)) {
        next
      }
      prefix <- if (substr(pc_val, 1L, 1L) == "+") pc_val else ""
      tel_loc <- stringi::stri_locate_first_fixed(s, "tel:")
      num_start <- if (!is.na(tel_loc[1L, 2L])) tel_loc[1L, 2L] + 1L else 1L
      mid <- if (pc_start > num_start) substr(s, num_start, pc_start - 1L) else ""
      out[i] <- paste0(prefix, mid)
    }
  }

  if (any(!has_pc)) {
    idx <- which(!has_pc)
    sub_x <- x[idx]
    start_loc <- stringi::stri_locate_first_regex(sub_x, valid_start_char_re)
    found <- !is.na(start_loc[, 1L])
    if (any(found)) {
      f_idx <- idx[found]
      cand <- stringi::stri_sub(sub_x[found], start_loc[found, 1L], -1L)
      cand <- stringi::stri_replace_first_regex(cand, unwanted_end_char_re, "")
      cand <- stringi::stri_replace_first_regex(cand, second_number_start_re, "")
      out[f_idx] <- cand
    }
  }

  has_out <- !is.na(out) & nzchar(out)
  if (any(has_out)) {
    isdn_loc <- stringi::stri_locate_first_fixed(out[has_out], ";isub=")
    has_isdn <- !is.na(isdn_loc[, 1L]) & isdn_loc[, 1L] > 1L
    if (any(has_isdn)) {
      h_idx <- which(has_out)[has_isdn]
      out[h_idx] <- stringi::stri_sub(out[h_idx], 1L, isdn_loc[has_isdn, 1L] - 1L)
    }
  }
  out
}

is_viable_phone_vec <- function(x) {
  ok <- !is.na(x) & nchar(x) >= min_length_for_nsn
  res <- rep(FALSE, length(x))
  if (any(ok)) {
    m <- stringi::stri_detect_regex(x[ok], valid_phone_number_re)
    m[is.na(m)] <- FALSE
    res[ok] <- m
  }
  res
}

strip_extension_vec <- function(x) {
  ext <- rep(NA_character_, length(x))
  num <- x
  loc <- stringi::stri_locate_first_regex(x, extn_re)
  has_ext <- !is.na(loc[, 1L]) & loc[, 1L] > 1L
  if (!any(has_ext)) {
    return(list(number = num, extension = ext))
  }
  idx <- which(has_ext)
  prefix <- stringi::stri_sub(x[idx], 1L, loc[idx, 1L] - 1L)
  viable <- is_viable_phone_vec(prefix)
  if (any(viable)) {
    v_idx <- idx[viable]
    v_prefix <- prefix[viable]
    matches <- stringi::stri_match_first_regex(x[v_idx], extn_re)
    # Columns 2..ncol are the capturing groups; pick the first non-NA per row
    ext_vals <- apply(matches[, -1L, drop = FALSE], 1L, function(row) {
      non_na <- row[!is.na(row) & nzchar(row)]
      if (length(non_na) > 0L) non_na[1L] else NA_character_
    })
    # Normalize any non-ASCII digits inside the captured extension ONLY if needed?
    # Note: phonenumbers keeps the captured extension string as matched.
    has_captured <- !is.na(ext_vals)
    num[v_idx[has_captured]] <- v_prefix[has_captured]
    ext[v_idx[has_captured]] <- ext_vals[has_captured]
  }
  list(number = num, extension = ext)
}

extract_cc_vec <- function(full_number) {
  n <- length(full_number)
  cc <- integer(n)
  rest <- full_number
  ok <- nchar(full_number) > 0L & substr(full_number, 1L, 1L) != "0"
  if (!any(ok)) {
    return(list(cc = cc, rest = rest))
  }
  idx <- which(ok)
  sub_num <- full_number[idx]
  len <- nchar(sub_num)

  c1 <- substr(sub_num, 1L, 1L)
  m1 <- c1 %in% names(cc_to_regions)

  c2 <- ifelse(len >= 2L, substr(sub_num, 1L, 2L), "")
  m2 <- !m1 & (c2 %in% names(cc_to_regions))

  c3 <- ifelse(len >= 3L, substr(sub_num, 1L, 3L), "")
  m3 <- !m1 & !m2 & (c3 %in% names(cc_to_regions))

  if (any(m1)) {
    cc[idx[m1]] <- as.integer(c1[m1])
    rest[idx[m1]] <- substr(sub_num[m1], 2L, len[m1])
  }
  if (any(m2)) {
    cc[idx[m2]] <- as.integer(c2[m2])
    rest[idx[m2]] <- substr(sub_num[m2], 3L, len[m2])
  }
  if (any(m3)) {
    cc[idx[m3]] <- as.integer(c3[m3])
    rest[idx[m3]] <- substr(sub_num[m3], 4L, len[m3])
  }
  list(cc = cc, rest = rest)
}

match_desc_vec <- function(nsn, desc) {
  if (is.null(desc) || is.null(desc$pattern_re)) {
    return(rep(FALSE, length(nsn)))
  }
  lens <- nchar(nsn)
  pl <- desc$possible_length
  ok <- if (length(pl) > 0L) lens %in% pl else rep(TRUE, length(nsn))
  res <- rep(FALSE, length(nsn))
  if (any(ok)) {
    m <- stringi::stri_detect_regex(nsn[ok], desc$pattern_re)
    m[is.na(m)] <- FALSE
    res[ok] <- m
  }
  res
}

test_number_length_vec <- function(nsn, meta) {
  pl <- meta$general_desc$possible_length
  loc <- meta$general_desc$possible_length_local_only
  if (length(pl) == 0L || identical(pl, -1L)) {
    return(rep("INVALID_LENGTH", length(nsn)))
  }
  lens <- nchar(nsn)
  min_len <- pl[1L]
  max_len <- pl[length(pl)]
  out <- rep("INVALID_LENGTH", length(nsn))
  out[lens < min_len] <- "TOO_SHORT"
  out[lens > max_len] <- "TOO_LONG"
  out[lens %in% pl] <- "IS_POSSIBLE"
  if (length(loc) > 0L) {
    out[lens %in% loc] <- "IS_POSSIBLE_LOCAL_ONLY"
  }
  out
}

strip_national_prefix_vec <- function(number, meta) {
  np_re <- meta$np_parse_re
  if (is.null(np_re) || length(number) == 0L) {
    return(number)
  }
  loc <- stringi::stri_locate_first_regex(number, np_re)
  matched <- !is.na(loc[, 1L]) & nchar(number) > 0L
  if (!any(matched)) {
    return(number)
  }
  out <- number
  idx <- which(matched)
  sub_num <- number[idx]
  sub_loc <- loc[idx, , drop = FALSE]
  mat <- stringi::stri_match_first_regex(sub_num, np_re)
  num_groups <- ncol(mat) - 1L
  transform_rule <- meta$national_prefix_transform_rule
  gd <- meta$general_desc
  viable_orig <- match_desc_vec(sub_num, gd)

  for (k in seq_along(idx)) {
    s <- sub_num[k]
    m_end <- sub_loc[k, 2L]
    last_group_na <- num_groups == 0L || is.na(mat[k, num_groups + 1L])
    if (is.null(transform_rule) || !nzchar(transform_rule) || last_group_na) {
      cand <- if (m_end < nchar(s)) substr(s, m_end + 1L, nchar(s)) else ""
      if (viable_orig[k] && !match_desc_vec(cand, gd)) {
        next
      }
      out[idx[k]] <- cand
    } else {
      cand <- stringi::stri_replace_first_regex(s, np_re, transform_rule)
      if (viable_orig[k] && !match_desc_vec(cand, gd)) {
        next
      }
      out[idx[k]] <- cand
    }
  }
  out
}

parse_idd_prefix_vec <- function(number, idd_re) {
  if (is.null(idd_re)) {
    return(list(stripped = rep(FALSE, length(number)), number = number))
  }
  loc <- stringi::stri_locate_first_regex(number, idd_re)
  matched <- !is.na(loc[, 1L])
  stripped <- rep(FALSE, length(number))
  out <- number
  if (!any(matched)) {
    return(list(stripped = stripped, number = out))
  }
  idx <- which(matched)
  for (i in idx) {
    s <- number[i]
    m_end <- loc[i, 2L]
    after <- if (m_end < nchar(s)) substr(s, m_end + 1L, nchar(s)) else ""
    d_match <- stringi::stri_extract_first_regex(after, capturing_digit_re)
    if (!is.na(d_match) && normalize_digits_vec(d_match) == "0") {
      next
    }
    stripped[i] <- TRUE
    out[i] <- after
  }
  list(stripped = stripped, number = out)
}

maybe_extract_cc_vec <- function(national_number, default_meta) {
  n <- length(national_number)
  cc <- integer(n)
  nsn <- rep("", n)
  failed <- rep(FALSE, n)
  from_plus_err_invalid_cc <- rep(FALSE, n)

  plus_loc <- stringi::stri_locate_first_regex(national_number, plus_chars_re)
  has_plus <- !is.na(plus_loc[, 1L])

  # 1. Numbers starting with +
  if (any(has_plus)) {
    p_idx <- which(has_plus)
    after_plus <- stringi::stri_sub(national_number[p_idx], plus_loc[p_idx, 2L] + 1L, -1L)
    norm_plus <- normalize_phone_vec(after_plus)
    too_short <- nchar(norm_plus) <= min_length_for_nsn
    if (any(too_short)) {
      failed[p_idx[too_short]] <- TRUE
    }
    if (any(!too_short)) {
      ok_idx <- p_idx[!too_short]
      ext <- extract_cc_vec(norm_plus[!too_short])
      valid_cc <- ext$cc != 0L
      if (any(valid_cc)) {
        cc[ok_idx[valid_cc]] <- ext$cc[valid_cc]
        nsn[ok_idx[valid_cc]] <- ext$rest[valid_cc]
      }
      if (any(!valid_cc)) {
        failed[ok_idx[!valid_cc]] <- TRUE
        from_plus_err_invalid_cc[ok_idx[!valid_cc]] <- TRUE
      }
    }
  }

  # 2. Numbers not starting with +
  if (any(!has_plus)) {
    np_idx <- which(!has_plus)
    norm_no_plus <- normalize_phone_vec(national_number[np_idx])
    idd_re <- if (!is.null(default_meta)) default_meta$idd_re else NULL
    idd_res <- parse_idd_prefix_vec(norm_no_plus, idd_re)

    if (any(idd_res$stripped)) {
      s_sub <- np_idx[idd_res$stripped]
      after_idd <- idd_res$number[idd_res$stripped]
      too_short <- nchar(after_idd) <= min_length_for_nsn
      if (any(too_short)) {
        failed[s_sub[too_short]] <- TRUE
      }
      if (any(!too_short)) {
        ok_sub <- s_sub[!too_short]
        ext <- extract_cc_vec(after_idd[!too_short])
        valid_cc <- ext$cc != 0L
        if (any(valid_cc)) {
          cc[ok_sub[valid_cc]] <- ext$cc[valid_cc]
          nsn[ok_sub[valid_cc]] <- ext$rest[valid_cc]
        }
        if (any(!valid_cc)) {
          failed[ok_sub[!valid_cc]] <- TRUE
        }
      }
    }

    if (any(!idd_res$stripped) && !is.null(default_meta)) {
      d_sub <- np_idx[!idd_res$stripped]
      d_num <- norm_no_plus[!idd_res$stripped]
      def_cc <- default_meta$country_code
      def_cc_str <- as.character(def_cc)
      def_cc_len <- nchar(def_cc_str)
      starts_cc <- nchar(d_num) >= def_cc_len & substr(d_num, 1L, def_cc_len) == def_cc_str
      if (any(starts_cc)) {
        c_sub <- d_sub[starts_cc]
        c_full <- d_num[starts_cc]
        pot_nn <- substr(c_full, def_cc_len + 1L, nchar(c_full))
        pot_nn <- strip_national_prefix_vec(pot_nn, default_meta)
        gd <- default_meta$general_desc
        take_stripped <- (!match_desc_vec(c_full, gd) & match_desc_vec(pot_nn, gd)) |
          (test_number_length_vec(c_full, default_meta) == "TOO_LONG")
        if (any(take_stripped)) {
          cc[c_sub[take_stripped]] <- def_cc
          nsn[c_sub[take_stripped]] <- pot_nn[take_stripped]
        }
      }
    }
  }

  list(
    cc = cc,
    nsn = nsn,
    failed = failed,
    from_plus_err_invalid_cc = from_plus_err_invalid_cc
  )
}

parse_unique_vec <- function(u, default_region) {
  n <- length(u)
  cc_out <- rep(NA_integer_, n)
  nsn_out <- rep(NA_character_, n)
  ext_out <- rep(NA_character_, n)

  if (n == 0L) {
    return(list(country_code = cc_out, national_number = nsn_out, extension = ext_out))
  }

  len_ok <- !is.na(u) & nchar(u) <= max_input_string_length
  if (!any(len_ok)) {
    return(list(country_code = cc_out, national_number = nsn_out, extension = ext_out))
  }

  cand <- rep(NA_character_, n)
  cand[len_ok] <- build_national_number_vec(u[len_ok])
  viable <- is_viable_phone_vec(cand)

  if (!is.null(default_region)) {
    reg_ok <- viable
  } else {
    has_plus <- rep(FALSE, n)
    has_plus[viable] <- stringi::stri_detect_regex(cand[viable], plus_chars_re)
    has_plus[is.na(has_plus)] <- FALSE
    reg_ok <- viable & has_plus
  }

  if (!any(reg_ok)) {
    return(list(country_code = cc_out, national_number = nsn_out, extension = ext_out))
  }

  idx <- which(reg_ok)
  ext_res <- strip_extension_vec(cand[idx])
  work_num <- ext_res$number
  work_ext <- ext_res$extension

  default_meta <- if (!is.null(default_region)) region_meta[[default_region]] else NULL
  cc_res <- maybe_extract_cc_vec(work_num, default_meta)

  # Retry stripping leading + if INVALID_COUNTRY_CODE was raised on a +-prefixed string
  if (any(cc_res$from_plus_err_invalid_cc)) {
    r_mask <- cc_res$from_plus_err_invalid_cc
    plus_loc <- stringi::stri_locate_first_regex(work_num[r_mask], plus_chars_re)
    stripped_plus <- stringi::stri_sub(work_num[r_mask], plus_loc[, 2L] + 1L, -1L)
    retry_res <- maybe_extract_cc_vec(stripped_plus, default_meta)
    retry_ok <- !retry_res$failed & retry_res$cc != 0L
    r_positions <- which(r_mask)
    if (any(retry_ok)) {
      ok_pos <- r_positions[retry_ok]
      cc_res$cc[ok_pos] <- retry_res$cc[retry_ok]
      cc_res$nsn[ok_pos] <- retry_res$nsn[retry_ok]
      cc_res$failed[ok_pos] <- FALSE
    }
  }

  alive <- !cc_res$failed
  if (!any(alive)) {
    return(list(country_code = cc_out, national_number = nsn_out, extension = ext_out))
  }

  a_idx <- idx[alive]
  a_cc <- cc_res$cc[alive]
  a_nsn <- cc_res$nsn[alive]
  a_num <- work_num[alive]
  a_ext <- work_ext[alive]

  # Where cc == 0, normalize raw national number and assign default_region's country_code
  no_cc <- a_cc == 0L
  if (any(no_cc)) {
    if (is.null(default_meta)) {
      # Cannot resolve without default_meta
      keep <- !no_cc
      a_idx <- a_idx[keep]
      a_cc <- a_cc[keep]
      a_nsn <- a_nsn[keep]
      a_num <- a_num[keep]
      a_ext <- a_ext[keep]
    } else {
      a_nsn[no_cc] <- normalize_phone_vec(a_num[no_cc])
      a_cc[no_cc] <- default_meta$country_code
    }
  }

  if (length(a_idx) == 0L) {
    return(list(country_code = cc_out, national_number = nsn_out, extension = ext_out))
  }

  # Strip national prefix per country_code/region metadata
  # If cc was extracted, metadata is main region for cc unless default_region matches
  target_reg <- vapply(seq_along(a_idx), function(i) {
    cc_key <- as.character(a_cc[i])
    main_reg <- cc_to_regions[[cc_key]][1L]
    if (!no_cc[i]) {
      if (!is.null(default_region) && identical(main_reg, default_region)) {
        default_region
      } else {
        main_reg
      }
    } else {
      default_region
    }
  }, character(1L))

  groups <- split(seq_along(a_idx), paste0(a_cc, ":", target_reg))
  for (g in groups) {
    g_cc <- a_cc[g[1L]]
    g_reg <- target_reg[g[1L]]
    meta <- get_meta_for_region_or_cc(g_cc, g_reg)
    if (!is.null(meta)) {
      sub_nsn <- a_nsn[g]
      ok_len <- nchar(sub_nsn) >= min_length_for_nsn
      if (any(ok_len)) {
        pot <- strip_national_prefix_vec(sub_nsn[ok_len], meta)
        v_res <- test_number_length_vec(pot, meta)
        accept <- !(v_res %in% c("TOO_SHORT", "IS_POSSIBLE_LOCAL_ONLY", "INVALID_LENGTH"))
        if (any(accept)) {
          sub_nsn[ok_len][accept] <- pot[accept]
          a_nsn[g] <- sub_nsn
        }
      }
    }
  }

  final_len <- nchar(a_nsn)
  valid_final <- final_len >= min_length_for_nsn & final_len <= max_length_for_nsn
  if (any(valid_final)) {
    v_idx <- a_idx[valid_final]
    v_nsn <- a_nsn[valid_final]
    # Canonicalize leading zeros as in libphonenumber:
    # e.g. "0000" -> 3 leading zeros + "0" = "0000"; "066981" -> "066981".
    cc_out[v_idx] <- a_cc[valid_final]
    nsn_out[v_idx] <- v_nsn
    ext_out[v_idx] <- a_ext[valid_final]
  }

  list(country_code = cc_out, national_number = nsn_out, extension = ext_out)
}

parse_core_dedup <- function(x, region = NULL, call = rlang::caller_env()) {
  check_character(x, call = call)
  region <- check_region(region, allow_null = TRUE, call = call)

  n <- length(x)
  if (n == 0L) {
    return(list(
      u_core = list(
        country_code = integer(),
        national_number = character(),
        extension = character(),
        is_na = logical()
      ),
      idx = integer(),
      is_na = logical(),
      n = 0L
    ))
  }

  is_na <- is.na(x)
  non_na_x <- x[!is_na]
  u <- unique(non_na_x)
  u_parsed <- parse_unique_vec(u, region)
  idx <- rep(NA_integer_, n)
  idx[!is_na] <- match(non_na_x, u)

  list(
    u_core = list(
      country_code = u_parsed$country_code,
      national_number = u_parsed$national_number,
      extension = u_parsed$extension,
      is_na = rep(FALSE, length(u))
    ),
    idx = idx,
    is_na = is_na,
    n = n
  )
}

#' Parse phone numbers into their components
#'
#' @description
#' `phone_parse()` extracts the country calling code, national significant
#' number, extension, region, validity, number type and E.164 representation for
#' each element of a character vector.
#'
#' @param x A character vector of phone numbers (or an existing `dialtone_phone`
#'   data frame returned by `phone_parse()`).
#' @param region A single two-letter CLDR region code (such as `"US"`, `"GB"` or
#'   `"IN"`) giving the default country for numbers written without a leading
#'   `+`, or `NULL` when numbers are in international format.
#'
#' @returns A data frame of class `c("dialtone_phone", "data.frame")` with one
#'   row per element of `x` and the columns:
#'   * `country_code`: integer country calling code (e.g. `1L`, `44L`, `91L`),
#'     or `NA_integer_`.
#'   * `national_number`: character national significant number (preserving any
#'     significant leading zeros, as in Italian fixed-line numbers), or
#'     `NA_character_`.
#'   * `extension`: character extension digits if present, or `NA_character_`.
#'   * `region`: two-letter region code (or `"001"` for non-geographic numbers),
#'     or `NA_character_`.
#'   * `possible`: logical; `TRUE` if the length is possible for the calling
#'     code, `FALSE` otherwise, and `NA` where `x` is `NA`.
#'   * `valid`: logical; `TRUE` if the number matches a valid numbering-plan
#'     pattern, `FALSE` otherwise, and `NA` where `x` is `NA`.
#'   * `type`: character number type (see [phone_type()]), or `NA_character_`.
#'   * `e164`: character E.164 formatted number (`+<country_code><national_number>`),
#'     or `NA_character_`.
#'
#' @seealso [phone_valid()], [phone_possible()], [phone_type()],
#'   [phone_region()], [phone_format()], [phone_regions()].
#' @export
#' @examples
#' phone_parse(c("+1 650-253-0000", "020 7946 0192", "not a number", NA), region = "GB")
phone_parse <- function(x, region = NULL) {
  if (inherits(x, "dialtone_phone")) {
    return(x)
  }
  check_character(x)
  region <- check_region(region, allow_null = TRUE)

  dedup <- parse_core_dedup(x, region)
  u_core <- dedup$u_core
  idx <- dedup$idx

  u_resolved <- resolve_region_and_type(u_core)
  u_poss <- compute_possible(u_core)
  u_valid <- !is.na(u_resolved$type) & u_resolved$type != "unknown"
  u_e164 <- ifelse(
    !is.na(u_core$country_code) & !is.na(u_core$national_number),
    paste0("+", u_core$country_code, u_core$national_number),
    NA_character_
  )

  out <- data.frame(
    country_code = u_core$country_code[idx],
    national_number = u_core$national_number[idx],
    extension = u_core$extension[idx],
    region = u_resolved$region[idx],
    possible = u_poss[idx],
    valid = u_valid[idx],
    type = u_resolved$type[idx],
    e164 = u_e164[idx],
    stringsAsFactors = FALSE
  )
  class(out) <- c("dialtone_phone", "data.frame")
  out
}
