source(file.path("R", "scraper_functions.R"))

scrape_date <- as.Date("2026-09-25")

single_day <- parse_notice_dates(
  date_text = paste(
    "The declared designated area will be in effect from 11am on",
    "Thursday 24 September 2026 to 9pm on Thursday 24 September 2026 inclusive."
  ),
  title = "Public notice: Stockland Wendouree Shopping Centre, Wendouree - Thursday 24 September 2026",
  source_url = "https://www.police.vic.gov.au/example-2026",
  current_notice = TRUE,
  scrape_date = scrape_date
)
stopifnot(single_day$start_date == as.Date("2026-09-24"))
stopifnot(single_day$end_date == as.Date("2026-09-24"))

same_year_range <- parse_notice_dates(
  date_text = "The declaration is in place from Monday 4 March to Wednesday 6 March 2024.",
  title = "Example",
  source_url = "https://www.police.vic.gov.au/example-2024",
  current_notice = FALSE,
  scrape_date = scrape_date
)
stopifnot(same_year_range$start_date == as.Date("2024-03-04"))
stopifnot(same_year_range$end_date == as.Date("2024-03-06"))

cross_year_range <- parse_notice_dates(
  date_text = "The declaration is in place from Friday 31 December to Saturday 1 January 2022.",
  title = "Example",
  source_url = "https://www.police.vic.gov.au/example-2022",
  current_notice = FALSE,
  scrape_date = scrape_date
)
stopifnot(cross_year_range$start_date == as.Date("2021-12-31"))
stopifnot(cross_year_range$end_date == as.Date("2022-01-01"))

shared_month_range <- parse_notice_dates(
  date_text = "The declaration applies on Thursday 10 and Friday 11 July 2025.",
  title = "Thursday 10 and Friday 11 July 2025: Westfield Airport West",
  source_url = "https://www.police.vic.gov.au/example-2025",
  current_notice = FALSE,
  scrape_date = scrape_date
)
stopifnot(shared_month_range$start_date == as.Date("2025-07-10"))
stopifnot(shared_month_range$end_date == as.Date("2025-07-11"))

compact_range <- parse_notice_dates(
  date_text = "This declaration will be in place for the festival from:",
  title = "Public notice: Moomba Festival 6-8 March 2020",
  source_url = "https://www.police.vic.gov.au/public-notice-moomba-2020",
  current_notice = FALSE,
  scrape_date = scrape_date
)
stopifnot(compact_range$start_date == as.Date("2020-03-06"))
stopifnot(compact_range$end_date == as.Date("2020-03-08"))

stopifnot(
  derive_location_from_title(
    "Public notice: Footscray Business District - Friday 19 June 2026 | Designated area"
  ) == "Footscray Business District"
)
stopifnot(
  derive_location_from_title(
    "Wednesday 31 December 2025 NYE- St Kilda"
  ) == "St Kilda"
)
stopifnot(
  derive_location_from_title(
    "Thursday 10 and Friday 11 July 2025: Westfield Airport West"
  ) == "Westfield Airport West"
)
stopifnot(
  derive_location_from_title(
    "Public notice: Westfield Airport West - Thursday 10 and Friday 11 July 2025"
  ) == "Westfield Airport West"
)
stopifnot(
  derive_location_from_title(
    "Public notice: Moomba Festival 6-8 March 2020"
  ) == "Moomba Festival"
)

suburb <- derive_suburb(
  "Stockland Wendouree Shopping Centre, Wendouree"
)
stopifnot(suburb$suburb == "Wendouree")

message("Parser tests passed.")
