required_packages <- c(
  "dplyr", "httr2", "lubridate", "purrr", "readr",
  "rvest", "stringr", "tibble", "tidyr", "xml2"
)

missing_packages <- required_packages[
  !vapply(required_packages, requireNamespace, logical(1), quietly = TRUE)
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Install the missing packages before running the scraper: ",
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}

weekday_rx <- paste0(
  "(?:Monday|Tuesday|Wednesday|Thursday|Friday|Saturday|Sunday)"
)
month_rx <- paste0(
  "(?:January|February|March|April|May|June|July|August|September|",
  "October|November|December)"
)
date_token_rx <- paste0(
  "(?i)(?:", weekday_rx, "\\s+)?\\d{1,2}\\s+", month_rx,
  "(?:\\s+\\d{4})?"
)
shared_month_range_rx <- paste0(
  "(?i)(?:", weekday_rx, "\\s+)?(\\d{1,2})\\s+",
  "(?:and|&|to|-)\\s+(?:", weekday_rx, "\\s+)?",
  "(\\d{1,2})\\s+(", month_rx, ")\\s+((?:19|20)\\d{2})"
)
compact_date_range_rx <- paste0(
  "(?i)\\b(\\d{1,2})\\s*[-–]\\s*(\\d{1,2})\\s+(",
  month_rx,
  ")\\s+((?:19|20)\\d{2})\\b"
)

empty_to_na <- function(x) {
  x <- stringr::str_squish(x)
  dplyr::na_if(x, "")
}

safe_file_stem <- function(url) {
  stem <- sub("[?#].*$", "", url)
  stem <- sub(".*/", "", stem)
  stem <- stringr::str_replace_all(stem, "[^A-Za-z0-9_-]+", "-")
  stem <- stringr::str_remove_all(stem, "^-|-$")
  ifelse(nzchar(stem), stem, "index")
}

fetch_html_document <- function(url, raw_html_dir) {
  dir.create(raw_html_dir, recursive = TRUE, showWarnings = FALSE)
  raw_path <- file.path(raw_html_dir, paste0(safe_file_stem(url), ".html"))

  response <- httr2::request(url) |>
    httr2::req_user_agent(
      "Public-interest research scraper; reproducible archival request"
    ) |>
    httr2::req_headers(
      Accept = "text/html,application/xhtml+xml"
    ) |>
    httr2::req_retry(
      max_tries = 4,
      backoff = function(tries) min(2^(tries - 1), 10)
    ) |>
    httr2::req_timeout(seconds = 45) |>
    httr2::req_perform()

  httr2::resp_check_status(response)
  raw_body <- httr2::resp_body_raw(response)

  connection <- file(raw_path, open = "wb")
  on.exit(close(connection), add = TRUE)
  writeBin(raw_body, connection)
  close(connection)
  on.exit(NULL, add = FALSE)

  list(
    document = xml2::read_html(raw_body),
    status_code = httr2::resp_status(response),
    raw_html_file = raw_path
  )
}

extract_notice_links <- function(index_document, index_url) {
  nodes <- rvest::html_elements(index_document, "h2, h3, h4, a")
  section <- NA_character_
  current_type <- NA_character_
  rows <- list()

  for (i in seq_along(nodes)) {
    node <- nodes[[i]]
    tag <- xml2::xml_name(node)
    text <- rvest::html_text2(node) |>
      stringr::str_squish()
    lower_text <- stringr::str_to_lower(text)

    if (tag %in% c("h2", "h3", "h4")) {
      if (stringr::str_detect(lower_text, "^current public notices$")) {
        section <- "current"
        current_type <- NA_character_
      } else if (stringr::str_detect(
        lower_text, "^previous public notices$"
      )) {
        section <- "previous"
        current_type <- NA_character_
      }

      if (identical(section, "current")) {
        if (stringr::str_detect(lower_text, "^designated areas")) {
          current_type <- "designated_area"
        } else if (stringr::str_detect(lower_text, "^designated places")) {
          current_type <- "designated_place"
        }
      }
    }

    if (identical(tag, "a")) {
      href <- xml2::xml_attr(node, "href")
      if (is.na(href) || !nzchar(href)) {
        next
      }

      absolute_url <- xml2::url_absolute(href, index_url)
      is_detail <- stringr::str_detect(
        absolute_url,
        "^https://www\\.police\\.vic\\.gov\\.au/public-notice-"
      )

      if (is_detail) {
        rows[[length(rows) + 1]] <- tibble::tibble(
          source_url = absolute_url,
          index_title = text,
          index_section = section,
          index_type_hint = current_type,
          current_notice = identical(section, "current") &&
            identical(current_type, "designated_area")
        )
      }
    }
  }

  if (length(rows) == 0) {
    return(tibble::tibble(
      source_url = character(),
      index_title = character(),
      index_section = character(),
      index_type_hint = character(),
      current_notice = logical()
    ))
  }

  dplyr::bind_rows(rows) |>
    dplyr::distinct(.data$source_url, .keep_all = TRUE)
}

document_main_text <- function(document) {
  main_node <- rvest::html_element(document, "main")
  main_text <- rvest::html_text2(main_node)

  if (is.na(main_text) || !nzchar(stringr::str_squish(main_text))) {
    main_text <- rvest::html_element(document, "body") |>
      rvest::html_text2()
  }

  stringr::str_squish(main_text)
}

document_title <- function(document) {
  title <- rvest::html_element(document, "h1") |>
    rvest::html_text2() |>
    stringr::str_squish()

  if (is.na(title) || !nzchar(title)) {
    title <- rvest::html_element(document, "title") |>
      rvest::html_text2() |>
      stringr::str_squish()
  }

  title
}

extract_date_text <- function(document, title) {
  paragraphs <- rvest::html_elements(document, "main p") |>
    rvest::html_text2() |>
    stringr::str_squish()

  if (length(paragraphs) == 0) {
    paragraphs <- rvest::html_elements(document, "p") |>
      rvest::html_text2() |>
      stringr::str_squish()
  }

  date_candidates <- paragraphs[
    stringr::str_detect(
      paragraphs,
      stringr::regex(
        "in effect|in place|between|will apply|\\bfrom\\b.+\\bto\\b",
        ignore_case = TRUE
      )
    )
  ]

  with_dates <- date_candidates[
    stringr::str_detect(date_candidates, date_token_rx)
  ]

  if (length(with_dates) > 0) {
    return(with_dates[[1]])
  }

  if (stringr::str_detect(title, date_token_rx)) {
    return(title)
  }

  NA_character_
}

parse_one_date <- function(date_text, year_hint = NA_integer_) {
  clean <- stringr::str_remove(
    date_text,
    paste0("(?i)^", weekday_rx, "\\s+")
  ) |>
    stringr::str_replace_all(",", "") |>
    stringr::str_squish()

  if (!stringr::str_detect(clean, "\\b\\d{4}\\b")) {
    if (is.na(year_hint)) {
      return(as.Date(NA))
    }
    clean <- paste(clean, year_hint)
  }

  suppressWarnings(as.Date(lubridate::dmy(clean, quiet = TRUE)))
}

parse_notice_dates <- function(date_text, title, source_url, current_notice,
                               scrape_date) {
  combined_date_text <- paste(date_text, title)
  compact_range_match <- stringr::str_match(
    combined_date_text,
    compact_date_range_rx
  )

  if (!is.na(compact_range_match[1, 1])) {
    start_date <- parse_one_date(paste(
      compact_range_match[1, 2],
      compact_range_match[1, 4],
      compact_range_match[1, 5]
    ))
    end_date <- parse_one_date(paste(
      compact_range_match[1, 3],
      compact_range_match[1, 4],
      compact_range_match[1, 5]
    ))

    return(tibble::tibble(
      start_date = start_date,
      end_date = end_date,
      end_date_basis = "notice_date_range_compact",
      date_parse_status = dplyr::if_else(
        is.na(start_date) || is.na(end_date) || end_date < start_date,
        "needs_review_compact_range",
        "parsed"
      )
    ))
  }

  shared_month_match <- stringr::str_match(
    combined_date_text,
    shared_month_range_rx
  )

  if (!is.na(shared_month_match[1, 1])) {
    start_date <- parse_one_date(paste(
      shared_month_match[1, 2],
      shared_month_match[1, 4],
      shared_month_match[1, 5]
    ))
    end_date <- parse_one_date(paste(
      shared_month_match[1, 3],
      shared_month_match[1, 4],
      shared_month_match[1, 5]
    ))

    return(tibble::tibble(
      start_date = start_date,
      end_date = end_date,
      end_date_basis = "notice_date_range_shared_month",
      date_parse_status = dplyr::if_else(
        is.na(start_date) || is.na(end_date) || end_date < start_date,
        "needs_review_shared_month_range",
        "parsed"
      )
    ))
  }

  source_text <- paste(date_text, title, source_url)
  year_values <- stringr::str_extract_all(
    source_text,
    "\\b(?:19|20)\\d{2}\\b"
  )[[1]]
  year_hint <- if (length(year_values) > 0) {
    as.integer(year_values[[1]])
  } else {
    NA_integer_
  }

  tokens <- if (!is.na(date_text)) {
    stringr::str_extract_all(date_text, date_token_rx)[[1]]
  } else {
    character()
  }

  if (length(tokens) == 0) {
    tokens <- stringr::str_extract_all(title, date_token_rx)[[1]]
  }

  parsed <- purrr::map(tokens, parse_one_date, year_hint = year_hint)
  parsed <- as.Date(unlist(parsed), origin = "1970-01-01")
  parsed <- parsed[!is.na(parsed)]

  start_date <- if (length(parsed) >= 1) parsed[[1]] else as.Date(NA)
  end_date <- if (length(parsed) >= 2) parsed[[2]] else as.Date(NA)
  end_date_basis <- if (length(parsed) >= 2) {
    "notice_date_range"
  } else if (length(parsed) == 1) {
    end_date <- start_date
    "single_day_notice"
  } else if (isTRUE(current_notice)) {
    end_date <- scrape_date
    "scrape_date_current_notice_fallback"
  } else {
    "not_parsed"
  }

  first_token_has_year <- length(tokens) >= 1 &&
    stringr::str_detect(tokens[[1]], "\\b\\d{4}\\b")

  if (
    length(parsed) >= 2 &&
      !first_token_has_year &&
      end_date < start_date
  ) {
    start_date <- lubridate::make_date(
      lubridate::year(end_date) - 1,
      lubridate::month(start_date),
      lubridate::day(start_date)
    )
  }

  tibble::tibble(
    start_date = as.Date(start_date),
    end_date = as.Date(end_date),
    end_date_basis = end_date_basis,
    date_parse_status = dplyr::case_when(
      is.na(start_date) ~ "needs_review_no_start_date",
      is.na(end_date) ~ "needs_review_no_end_date",
      end_date < start_date ~ "needs_review_end_before_start",
      TRUE ~ "parsed"
    )
  )
}

extract_location_description <- function(document) {
  headings <- rvest::html_elements(document, "h2, h3")
  heading_text <- headings |>
    rvest::html_text2() |>
    stringr::str_squish() |>
    stringr::str_to_lower()

  exact_index <- which(heading_text == "designated area")
  if (length(exact_index) > 0) {
    paragraph <- xml2::xml_find_first(
      headings[[exact_index[[1]]]],
      "following-sibling::p[1]"
    )
    text <- rvest::html_text2(paragraph) |>
      stringr::str_squish()
    if (!is.na(text) && nzchar(text)) {
      return(text)
    }
  }

  paragraphs <- rvest::html_elements(document, "main p") |>
    rvest::html_text2() |>
    stringr::str_squish()

  location_candidates <- paragraphs[
    stringr::str_detect(
      paragraphs,
      stringr::regex(
        "area (?:that is )?declared|designated area containing",
        ignore_case = TRUE
      )
    )
  ]

  if (length(location_candidates) > 0) {
    return(location_candidates[[1]])
  }

  NA_character_
}

derive_location_from_title <- function(title) {
  clean <- title |>
    stringr::str_remove(stringr::regex("^public notice:\\s*", TRUE)) |>
    stringr::str_remove("\\s*\\|.*$") |>
    stringr::str_remove(
      stringr::regex("\\s*[-–:]?\\s*designated area\\s*$", TRUE)
    ) |>
    stringr::str_squish()

  first_date <- stringr::str_locate(clean, date_token_rx)[1, ]
  shared_range <- stringr::str_locate(clean, shared_month_range_rx)[1, ]
  compact_range <- stringr::str_locate(clean, compact_date_range_rx)[1, ]

  location_range <- if (!is.na(shared_range[[1]])) {
    shared_range
  } else if (!is.na(compact_range[[1]])) {
    compact_range
  } else {
    first_date
  }

  if (is.na(location_range[[1]])) {
    return(empty_to_na(clean))
  }

  starts_with_date_cue <- stringr::str_detect(
    clean,
    stringr::regex(
      paste0("^(?:", weekday_rx, "\\s+)?\\d{1,2}\\b"),
      ignore_case = TRUE
    )
  )

  if (starts_with_date_cue && stringr::str_detect(clean, ":")) {
    candidate <- stringr::str_replace(clean, "^.*?:\\s*", "")
  } else if (location_range[[1]] <= 3) {
    candidate <- clean |>
      stringr::str_remove_all(shared_month_range_rx) |>
      stringr::str_remove_all(compact_date_range_rx) |>
      stringr::str_remove_all(date_token_rx) |>
      stringr::str_remove("(?i)^(?:\\s|to\\b|and\\b|on\\b|NYE\\b|[-–:])+")
  } else {
    candidate <- stringr::str_sub(clean, 1, location_range[[1]] - 1) |>
      stringr::str_remove("(?i)(?:\\bon\\b|[-–:]|\\s)+$")
  }

  candidate |>
    stringr::str_remove(stringr::regex("\\s*[-–:]?\\s*amended\\s*$", TRUE)) |>
    stringr::str_squish() |>
    empty_to_na()
}

derive_suburb <- function(location) {
  if (is.na(location) || !nzchar(location)) {
    return(tibble::tibble(suburb = NA_character_, suburb_source = NA_character_))
  }

  comma_parts <- stringr::str_split(location, "\\s*,\\s*")[[1]]
  if (length(comma_parts) > 1) {
    candidate <- empty_to_na(tail(comma_parts, 1))
    if (
      !is.na(candidate) &&
        stringr::str_count(candidate, "\\S+") <= 5 &&
        !stringr::str_detect(
          candidate,
          stringr::regex(
            "centre|station|precinct|district|festival|foreshore|surrounds|CBD",
            TRUE
          )
        )
    ) {
      return(tibble::tibble(
        suburb = candidate,
        suburb_source = "title_after_comma"
      ))
    }
  }

  keyword_match <- stringr::str_match(
    location,
    stringr::regex(
      paste0(
        "^(.+?)\\s+(?:Business District|Central Business District|CBD|",
        "Railway Station|Shopping Centre|Shopping Precinct|Town Centre)\\b"
      ),
      ignore_case = TRUE
    )
  )[, 2]

  if (!is.na(keyword_match)) {
    candidate <- keyword_match |>
      stringr::str_remove(
        stringr::regex("^(?:Stockland|Westfield|Pacific)\\s+", TRUE)
      ) |>
      empty_to_na()
    return(tibble::tibble(
      suburb = candidate,
      suburb_source = "location_keyword"
    ))
  }

  if (
    stringr::str_count(location, "\\S+") <= 4 &&
      !stringr::str_detect(
        location,
        stringr::regex("^(?:City|Shire|Rural City|Borough) of ", TRUE)
      ) &&
      !stringr::str_detect(
        location,
        stringr::regex(
          "festival|event|grand prix|shopping|station|precinct|district|CBD",
          TRUE
        )
      )
  ) {
    return(tibble::tibble(
      suburb = location,
      suburb_source = "simple_location_title"
    ))
  }

  tibble::tibble(suburb = NA_character_, suburb_source = NA_character_)
}

derive_explicit_lga <- function(location, location_description) {
  text <- paste(location, location_description)
  lga <- stringr::str_extract(
    text,
    stringr::regex(
      "(?:City|Shire|Rural City|Borough) of [A-Z][A-Za-z' -]{1,40}",
      ignore_case = FALSE
    )
  )

  if (!is.na(lga)) {
    lga <- stringr::str_remove(
      lga,
      stringr::regex(
        "\\s+(?:has|is|and|including|inclusive|being|acting)\\b.*$",
        TRUE
      )
    )
  }

  empty_to_na(lga)
}

extract_updated_date <- function(main_text) {
  updated_text <- stringr::str_extract(
    main_text,
    stringr::regex(
      paste0("Updated\\s+", date_token_rx),
      ignore_case = TRUE
    )
  )

  if (is.na(updated_text)) {
    return(as.Date(NA))
  }

  parse_one_date(stringr::str_remove(updated_text, "(?i)^Updated\\s+"))
}

parse_notice_document <- function(document, link_row, raw_html_file,
                                  scraped_at, scrape_date) {
  title <- document_title(document)
  main_text <- document_main_text(document)
  is_designated_area <- stringr::str_detect(
    main_text,
    stringr::regex("Control of Weapons Act", ignore_case = TRUE)
  ) && stringr::str_detect(
    main_text,
    stringr::regex("designated area", ignore_case = TRUE)
  )

  location <- derive_location_from_title(title)
  location_description <- extract_location_description(document)
  suburb_fields <- derive_suburb(location)
  date_text <- extract_date_text(document, title)
  date_fields <- parse_notice_dates(
    date_text = date_text,
    title = title,
    source_url = link_row$source_url,
    current_notice = link_row$current_notice,
    scrape_date = scrape_date
  )

  tibble::tibble(
    notice_id = safe_file_stem(link_row$source_url),
    title = title,
    location = location,
    suburb = suburb_fields$suburb,
    suburb_source = suburb_fields$suburb_source,
    lga = derive_explicit_lga(location, location_description),
    lga_source = dplyr::if_else(
      is.na(derive_explicit_lga(location, location_description)),
      NA_character_,
      "explicit_notice_text"
    ),
    start_date = date_fields$start_date,
    end_date = date_fields$end_date,
    end_date_basis = date_fields$end_date_basis,
    date_parse_status = date_fields$date_parse_status,
    current_notice = link_row$current_notice,
    index_section = link_row$index_section,
    index_title = link_row$index_title,
    location_description = location_description,
    date_text = date_text,
    page_updated_date = extract_updated_date(main_text),
    source_url = link_row$source_url,
    source_page_url = "https://www.police.vic.gov.au/public-notices",
    raw_html_file = raw_html_file,
    scraped_at = as.POSIXct(scraped_at),
    scrape_date = as.Date(scrape_date),
    is_designated_area = is_designated_area
  )
}

apply_location_overrides <- function(data, overrides_path) {
  if (!file.exists(overrides_path)) {
    return(data)
  }

  overrides <- readr::read_csv(
    overrides_path,
    col_types = readr::cols(.default = readr::col_character()),
    show_col_types = FALSE
  )

  if (nrow(overrides) == 0) {
    return(data)
  }

  required <- c(
    "source_url", "location_override", "suburb_override", "lga_override"
  )
  missing <- setdiff(required, names(overrides))
  if (length(missing) > 0) {
    stop(
      paste("Override file is missing:", paste(missing, collapse = ", ")),
      call. = FALSE
    )
  }

  data |>
    dplyr::left_join(overrides, by = "source_url") |>
    dplyr::mutate(
      location = dplyr::coalesce(
        empty_to_na(.data$location_override), .data$location
      ),
      suburb = dplyr::coalesce(
        empty_to_na(.data$suburb_override), .data$suburb
      ),
      lga = dplyr::coalesce(empty_to_na(.data$lga_override), .data$lga),
      suburb_source = dplyr::if_else(
        !is.na(empty_to_na(.data$suburb_override)),
        "manual_override",
        .data$suburb_source
      ),
      lga_source = dplyr::if_else(
        !is.na(empty_to_na(.data$lga_override)),
        "manual_override",
        .data$lga_source
      )
    ) |>
    dplyr::select(
      -dplyr::any_of(c(
        "location_override", "suburb_override", "lga_override"
      ))
    )
}

scrape_vicpol_designated_areas <- function(
    index_url = "https://www.police.vic.gov.au/public-notices",
    output_dir = "outputs",
    raw_html_root = "data-raw/vicpol-public-notices",
    overrides_path = "config/location_overrides.csv",
    delay_seconds = 2) {
  if (delay_seconds < 2) {
    stop(
      "delay_seconds must be at least 2 to respect the site's published crawl delay.",
      call. = FALSE
    )
  }

  scraped_at <- lubridate::with_tz(Sys.time(), "Australia/Melbourne")
  scrape_date <- as.Date(scraped_at)
  run_id <- format(scraped_at, "%Y%m%d-%H%M%S")
  raw_html_dir <- file.path(raw_html_root, run_id)

  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(raw_html_dir, recursive = TRUE, showWarnings = FALSE)

  index_result <- fetch_html_document(index_url, raw_html_dir)
  links <- extract_notice_links(index_result$document, index_url)

  records <- list()
  errors <- list()

  for (i in seq_len(nrow(links))) {
    link_row <- links[i, ]

    if (i > 1 && delay_seconds > 0) {
      Sys.sleep(delay_seconds)
    }

    message("[", i, "/", nrow(links), "] ", link_row$source_url)

    tryCatch({
      fetched <- fetch_html_document(link_row$source_url, raw_html_dir)
      parsed <- parse_notice_document(
        document = fetched$document,
        link_row = link_row,
        raw_html_file = fetched$raw_html_file,
        scraped_at = scraped_at,
        scrape_date = scrape_date
      )
      records[[length(records) + 1]] <- parsed
    }, error = function(error) {
      errors[[length(errors) + 1]] <<- tibble::tibble(
        source_url = link_row$source_url,
        error_message = conditionMessage(error),
        scraped_at = as.POSIXct(scraped_at)
      )
    })
  }

  if (length(records) == 0) {
    stop(
      paste0(
        "No notice pages were successfully parsed. Check the errors and the ",
        "saved index HTML before rerunning."
      ),
      call. = FALSE
    )
  }

  data <- dplyr::bind_rows(records) |>
    dplyr::filter(.data$is_designated_area) |>
    apply_location_overrides(overrides_path) |>
    dplyr::arrange(.data$start_date, .data$location, .data$source_url)

  error_data <- if (length(errors) > 0) {
    dplyr::bind_rows(errors)
  } else {
    tibble::tibble(
      source_url = character(),
      error_message = character(),
      scraped_at = as.POSIXct(character())
    )
  }

  csv_path <- file.path(
    output_dir,
    paste0("vicpol_designated_areas_", run_id, ".csv")
  )
  rds_path <- file.path(
    output_dir,
    paste0("vicpol_designated_areas_", run_id, ".rds")
  )
  links_path <- file.path(
    output_dir,
    paste0("vicpol_public_notice_links_", run_id, ".csv")
  )
  errors_path <- file.path(
    output_dir,
    paste0("vicpol_scrape_errors_", run_id, ".csv")
  )
  session_path <- file.path(
    output_dir,
    paste0("vicpol_scrape_session_", run_id, ".txt")
  )

  readr::write_csv(data, csv_path, na = "")
  saveRDS(data, rds_path)
  readr::write_csv(links, links_path, na = "")
  readr::write_csv(error_data, errors_path, na = "")
  writeLines(capture.output(utils::sessionInfo()), session_path)

  list(
    data = data,
    errors = error_data,
    links = links,
    files = list(
      csv = csv_path,
      rds = rds_path,
      links = links_path,
      errors = errors_path,
      session = session_path,
      raw_html_dir = raw_html_dir
    )
  )
}
