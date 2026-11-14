# Compiles data-raw/upstream/PhoneNumberMetadata.xml (google/libphonenumber v9.0.43)
# into R/sysdata.rda. Run from the package root with:
#   Rscript data-raw/build-metadata.R

xml_path <- "data-raw/upstream/PhoneNumberMetadata.xml"
expected_sha256 <- "5bbfb6f2e63ba1131fc867cd1cf48aa131047772523723a10c230db12fb6e2df"

if (!file.exists(xml_path)) {
  stop("Missing upstream file: ", xml_path, call. = FALSE)
}

clean_re <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x)) {
    return(NULL)
  }
  out <- gsub("[[:space:]]+", "", x)
  if (!nzchar(out)) NULL else out
}

expand_lengths <- function(spec) {
  if (is.null(spec) || length(spec) == 0L || is.na(spec) || !nzchar(spec)) {
    return(integer())
  }
  parts <- strsplit(spec, ",", fixed = TRUE)[[1L]]
  out <- integer()
  for (part in parts) {
    part <- trimws(part)
    if (grepl("^\\[[0-9]+-[0-9]+\\]$", part)) {
      bounds <- as.integer(strsplit(gsub("^\\[|\\]$", "", part), "-", fixed = TRUE)[[1L]])
      out <- c(out, seq.int(bounds[1L], bounds[2L]))
    } else {
      out <- c(out, as.integer(part))
    }
  }
  sort(unique(out))
}

attr_or_null <- function(node, name) {
  val <- xml2::xml_attr(node, name)
  if (is.na(val) || !nzchar(val)) NULL else val
}

child_text_or_null <- function(node, xpath) {
  child <- xml2::xml_find_first(node, xpath)
  if (inherits(child, "xml_missing")) {
    return(NULL)
  }
  txt <- xml2::xml_text(child)
  if (is.na(txt) || !nzchar(txt)) NULL else txt
}

type_tags <- c(
  fixed_line = "fixedLine",
  mobile = "mobile",
  toll_free = "tollFree",
  premium_rate = "premiumRate",
  shared_cost = "sharedCost",
  personal_number = "personalNumber",
  voip = "voip",
  pager = "pager",
  uan = "uan",
  voicemail = "voicemail",
  no_international_dialling = "noInternationalDialling"
)

doc <- xml2::read_xml(xml_path)
territory_nodes <- xml2::xml_find_all(doc, ".//territory")

region_meta <- new.env(hash = TRUE, parent = emptyenv())
nongeo_meta <- new.env(hash = TRUE, parent = emptyenv())
cc_to_regions <- list()
region_rows <- list()
example_rows <- list()

for (t_node in territory_nodes) {
  tid <- xml2::xml_attr(t_node, "id")
  cc <- as.integer(xml2::xml_attr(t_node, "countryCode"))
  cc_key <- as.character(cc)
  main_country <- identical(xml2::xml_attr(t_node, "mainCountryForCode"), "true")

  existing_regions <- cc_to_regions[[cc_key]]
  if (is.null(existing_regions)) {
    cc_to_regions[[cc_key]] <- tid
  } else if (main_country) {
    cc_to_regions[[cc_key]] <- c(tid, existing_regions)
  } else {
    cc_to_regions[[cc_key]] <- c(existing_regions, tid)
  }

  leading_digits <- clean_re(attr_or_null(t_node, "leadingDigits"))
  intl_prefix <- clean_re(attr_or_null(t_node, "internationalPrefix"))
  pref_intl_prefix <- attr_or_null(t_node, "preferredInternationalPrefix")
  nat_prefix <- attr_or_null(t_node, "nationalPrefix")
  nat_prefix_parse <- clean_re(attr_or_null(t_node, "nationalPrefixForParsing"))
  if (is.null(nat_prefix_parse)) {
    nat_prefix_parse <- nat_prefix
  }
  nat_prefix_transform <- attr_or_null(t_node, "nationalPrefixTransformRule")
  pref_extn_prefix <- attr_or_null(t_node, "preferredExtnPrefix")
  mobile_portable <- identical(xml2::xml_attr(t_node, "mobileNumberPortableRegion"), "true")

  gd_node <- xml2::xml_find_first(t_node, "generalDesc")
  gd_pat <- if (!inherits(gd_node, "xml_missing")) {
    clean_re(child_text_or_null(gd_node, "nationalNumberPattern"))
  } else {
    NULL
  }

  descs <- list()
  all_nat_lengths <- integer()
  all_loc_lengths <- integer()

  for (type_name in names(type_tags)) {
    xml_tag <- type_tags[[type_name]]
    d_node <- xml2::xml_find_first(t_node, xml_tag)
    if (inherits(d_node, "xml_missing")) {
      descs[[type_name]] <- NULL
    } else {
      pat <- clean_re(child_text_or_null(d_node, "nationalNumberPattern"))
      ex <- child_text_or_null(d_node, "exampleNumber")
      pl_node <- xml2::xml_find_first(d_node, "possibleLengths")
      nat_len <- if (!inherits(pl_node, "xml_missing")) {
        expand_lengths(xml2::xml_attr(pl_node, "national"))
      } else {
        integer()
      }
      loc_len <- if (!inherits(pl_node, "xml_missing")) {
        expand_lengths(xml2::xml_attr(pl_node, "localOnly"))
      } else {
        integer()
      }
      if (type_name != "no_international_dialling") {
        all_nat_lengths <- c(all_nat_lengths, nat_len)
        all_loc_lengths <- c(all_loc_lengths, loc_len)
        if (!is.null(ex)) {
          example_rows[[length(example_rows) + 1L]] <- data.frame(
            region = tid,
            country_code = cc,
            type = type_name,
            example_number = ex,
            stringsAsFactors = FALSE
          )
        }
      }
      descs[[type_name]] <- list(
        raw_pattern = pat,
        pattern_re = if (!is.null(pat)) paste0("^(?:", pat, ")$") else NULL,
        example_number = if (!is.null(ex)) ex else NA_character_,
        possible_length = nat_len,
        possible_length_local_only = loc_len
      )
    }
  }

  all_nat_lengths <- sort(unique(all_nat_lengths))
  all_loc_lengths <- sort(setdiff(unique(all_loc_lengths), all_nat_lengths))

  general_desc <- list(
    raw_pattern = gd_pat,
    pattern_re = if (!is.null(gd_pat)) paste0("^(?:", gd_pat, ")$") else NULL,
    possible_length = all_nat_lengths,
    possible_length_local_only = all_loc_lengths
  )

  fl_pat <- if (!is.null(descs$fixed_line)) descs$fixed_line$raw_pattern else NULL
  mb_pat <- if (!is.null(descs$mobile)) descs$mobile$raw_pattern else NULL
  same_fl_mb <- identical(fl_pat, mb_pat)

  # Compile numberFormat and intlNumberFormat
  af_node <- xml2::xml_find_first(t_node, "availableFormats")
  terr_np_rule <- attr_or_null(t_node, "nationalPrefixFormattingRule")
  terr_np_opt <- identical(xml2::xml_attr(t_node, "nationalPrefixOptionalWhenFormatting"), "true")
  terr_cc_rule <- attr_or_null(t_node, "carrierCodeFormattingRule")

  nf_list <- list()
  intl_list <- list()
  has_explicit_intl <- FALSE

  if (!inherits(af_node, "xml_missing")) {
    nf_nodes <- xml2::xml_find_all(af_node, "numberFormat")
    for (nf_node in nf_nodes) {
      pat <- clean_re(xml2::xml_attr(nf_node, "pattern"))
      fmt <- child_text_or_null(nf_node, "format")
      ifmt <- child_text_or_null(nf_node, "intlFormat")
      if (!is.null(ifmt)) {
        has_explicit_intl <- TRUE
      }
      ld_nodes <- xml2::xml_find_all(nf_node, "leadingDigits")
      lds <- vapply(ld_nodes, function(n) clean_re(xml2::xml_text(n)), character(1L))
      last_ld_re <- if (length(lds) > 0L) paste0("^(?:", lds[length(lds)], ")") else NULL

      np_rule <- attr_or_null(nf_node, "nationalPrefixFormattingRule")
      if (is.null(np_rule)) {
        np_rule <- terr_np_rule
      }
      if (!is.null(np_rule)) {
        if (!is.null(nat_prefix)) {
          np_rule <- gsub("$NP", nat_prefix, np_rule, fixed = TRUE)
          np_rule <- gsub("$FG", "$1", np_rule, fixed = TRUE)
        } else {
          np_rule <- gsub("$FG", "$1", np_rule, fixed = TRUE)
        }
      }

      # Effective rule used when formatting NATIONAL without carrier code:
      # When national_prefix is NULL, format_by_pattern clears national_prefix_formatting_rule,
      # whereas _format_nsn uses national_prefix_formatting_rule directly if present.
      nat_fmt_rule <- if (!is.null(np_rule) && nzchar(np_rule)) {
        stringi::stri_replace_first_regex(fmt, "(\\$[0-9])", np_rule)
      } else {
        fmt
      }

      np_opt_attr <- xml2::xml_attr(nf_node, "nationalPrefixOptionalWhenFormatting")
      np_opt <- if (!is.na(np_opt_attr)) identical(np_opt_attr, "true") else terr_np_opt

      cc_rule <- attr_or_null(nf_node, "carrierCodeFormattingRule")
      if (is.null(cc_rule)) {
        cc_rule <- terr_cc_rule
      }
      if (!is.null(cc_rule)) {
        if (!is.null(nat_prefix)) {
          cc_rule <- gsub("$NP", nat_prefix, cc_rule, fixed = TRUE)
        }
        cc_rule <- gsub("$FG", "$1", cc_rule, fixed = TRUE)
      }

      nf_entry <- list(
        raw_pattern = pat,
        pattern_re = paste0("^(?:", pat, ")$"),
        format = fmt,
        nat_format = nat_fmt_rule,
        leading_digits_pattern = lds,
        last_leading_digits_re = last_ld_re,
        national_prefix_formatting_rule = np_rule,
        national_prefix_optional_when_formatting = np_opt,
        carrier_code_formatting_rule = cc_rule
      )
      nf_list[[length(nf_list) + 1L]] <- nf_entry

      if (identical(ifmt, "NA")) {
        # Omit from intl_number_format
      } else if (!is.null(ifmt)) {
        intl_list[[length(intl_list) + 1L]] <- list(
          raw_pattern = pat,
          pattern_re = paste0("^(?:", pat, ")$"),
          format = ifmt,
          nat_format = ifmt,
          leading_digits_pattern = lds,
          last_leading_digits_re = last_ld_re,
          national_prefix_formatting_rule = NULL,
          national_prefix_optional_when_formatting = FALSE,
          carrier_code_formatting_rule = NULL
        )
      } else {
        intl_list[[length(intl_list) + 1L]] <- list(
          raw_pattern = pat,
          pattern_re = paste0("^(?:", pat, ")$"),
          format = fmt,
          nat_format = fmt,
          leading_digits_pattern = lds,
          last_leading_digits_re = last_ld_re,
          national_prefix_formatting_rule = NULL,
          national_prefix_optional_when_formatting = FALSE,
          carrier_code_formatting_rule = NULL
        )
      }
    }
  }

  if (!has_explicit_intl) {
    intl_list <- list()
  }

  meta_obj <- list(
    id = tid,
    country_code = cc,
    main_country = main_country,
    leading_digits = leading_digits,
    leading_digits_re = if (!is.null(leading_digits)) paste0("^(?:", leading_digits, ")") else NULL,
    international_prefix = intl_prefix,
    idd_re = if (!is.null(intl_prefix)) paste0("^(?:", intl_prefix, ")") else NULL,
    preferred_international_prefix = pref_intl_prefix,
    national_prefix = nat_prefix,
    national_prefix_for_parsing = nat_prefix_parse,
    np_parse_re = if (!is.null(nat_prefix_parse)) paste0("^(?:", nat_prefix_parse, ")") else NULL,
    national_prefix_transform_rule = nat_prefix_transform,
    preferred_extn_prefix = pref_extn_prefix,
    mobile_number_portable_region = mobile_portable,
    same_mobile_and_fixed_line_pattern = same_fl_mb,
    general_desc = general_desc,
    fixed_line = descs$fixed_line,
    mobile = descs$mobile,
    toll_free = descs$toll_free,
    premium_rate = descs$premium_rate,
    shared_cost = descs$shared_cost,
    personal_number = descs$personal_number,
    voip = descs$voip,
    pager = descs$pager,
    uan = descs$uan,
    voicemail = descs$voicemail,
    no_international_dialling = descs$no_international_dialling,
    number_format = nf_list,
    intl_number_format = intl_list
  )

  if (tid == "001") {
    nongeo_meta[[cc_key]] <- meta_obj
  } else {
    region_meta[[tid]] <- meta_obj
  }

  region_rows[[length(region_rows) + 1L]] <- data.frame(
    region = tid,
    country_code = cc,
    main_country = main_country || length(cc_to_regions[[cc_key]]) == 1L,
    national_prefix = if (!is.null(nat_prefix)) nat_prefix else NA_character_,
    example_fixed_line = if (!is.null(descs$fixed_line)) descs$fixed_line$example_number else NA_character_,
    example_mobile = if (!is.null(descs$mobile)) descs$mobile$example_number else NA_character_,
    stringsAsFactors = FALSE
  )
}

# Recompute main_country in region_rows after all territories for each country_code are known
regions_df <- do.call(rbind, region_rows)
for (i in seq_len(nrow(regions_df))) {
  cc_key <- as.character(regions_df$country_code[i])
  regions_df$main_country[i] <- identical(cc_to_regions[[cc_key]][1L], regions_df$region[i])
}
rownames(regions_df) <- NULL

example_numbers_df <- do.call(rbind, example_rows)
rownames(example_numbers_df) <- NULL

supported_regions <- sort(ls(region_meta))
supported_nongeo_codes <- sort(as.integer(ls(nongeo_meta)))
metadata_version <- "9.0.43"
metadata_sha256 <- expected_sha256

dir.create("R", showWarnings = FALSE)
save(
  region_meta,
  nongeo_meta,
  cc_to_regions,
  supported_regions,
  supported_nongeo_codes,
  regions_df,
  example_numbers_df,
  metadata_version,
  metadata_sha256,
  file = "R/sysdata.rda",
  compress = "xz"
)
message(
  "Built R/sysdata.rda: ", length(supported_regions), " regions, ",
  length(supported_nongeo_codes), " non-geo codes, ",
  nrow(example_numbers_df), " example numbers."
)
