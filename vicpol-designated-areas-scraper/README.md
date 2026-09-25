# Victoria Police designated-area public notice scraper

This reproducible R scraper collects designated-area public notices from:

<https://www.police.vic.gov.au/public-notices>

It follows every linked public-notice page, retains notices made under the **Control of Weapons Act 1990**, and writes a tidy table with:

- `location`
- `suburb`, where it can be extracted from the notice title
- `lga`, where the notice explicitly names one
- `start_date`
- `end_date`
- `end_date_basis`
- current/previous notice status
- source URL, page update date and scrape timestamp
- the source date and location text used by the parser
- parser status and raw HTML snapshot path

## Why raw HTML is saved

Every run saves the index page and each notice page under a timestamped `data-raw` folder. This provides an auditable snapshot if Victoria Police later edits or removes a notice. The output also retains parser evidence fields rather than silently discarding uncertain text. A session-information file records the R and package versions used for the run.

## Required packages

```r
install.packages(c(
  "dplyr", "httr2", "lubridate", "purrr", "readr",
  "rvest", "stringr", "tibble", "tidyr", "xml2"
))
```

## Run the scraper

From the `VictoriaPoliceRacialSearch` project root, run:

```r
source("vicpol-designated-areas-scraper/run_scraper.R")
```

Alternatively, open R in the `vicpol-designated-areas-scraper` folder and run:

```r
source("run_scraper.R")
```

Or from a terminal:

```text
Rscript run_scraper.R
```

The scraper waits two seconds between detail pages, following the crawl delay published in Victoria Police's `robots.txt`, and retries temporary HTTP failures. You can increase `delay_seconds` in `run_scraper.R`, but do not reduce it below two seconds.

## Outputs

Each run creates timestamped files:

```text
outputs/
├── vicpol_designated_areas_YYYYMMDD-HHMMSS.csv
├── vicpol_designated_areas_YYYYMMDD-HHMMSS.rds
├── vicpol_public_notice_links_YYYYMMDD-HHMMSS.csv
├── vicpol_scrape_errors_YYYYMMDD-HHMMSS.csv
└── vicpol_scrape_session_YYYYMMDD-HHMMSS.txt

data-raw/vicpol-public-notices/YYYYMMDD-HHMMSS/
├── public-notices.html
└── one HTML file for each notice page
```

## Date rules

The parser first uses the sentence stating when the declaration is “in effect”, “in place” or applies “between” particular times.

- A one-day notice has the same start and end date.
- A stated range uses its first and second dates.
- A range such as `4 March to 6 March 2024` inherits 2024 for the first date.
- A range such as `31 December to 1 January 2022` assigns the start to 2021.
- If a notice is listed as current but no end date can be parsed, `end_date` uses the scrape date and `end_date_basis` records that fallback.
- Failed or questionable parses are retained and flagged in `date_parse_status`.

## Suburb and LGA review

Location text is not standardised across the archive. The scraper uses conservative title-based rules:

- a locality after a comma, such as `Wendouree`
- a locality attached to a recognised place description, such as `Footscray Business District`
- a short location title that appears to be a locality

An LGA is populated only when the notice explicitly uses wording such as `City of Yarra` or `Shire of ...`. It is not guessed from a suburb.

Review blank or questionable values and add corrections to:

```text
config/location_overrides.csv
```

Use the notice URL as the key. For example:

```csv
source_url,location_override,suburb_override,lga_override,notes
https://www.police.vic.gov.au/example,,Footscray,Maribyrnong,Checked against notice map
```

Overrides are applied after parsing and labelled as `manual_override` in the corresponding source field.

## Run parser tests

The tests cover current single-day wording, same-year ranges, ranges crossing New Year, and title-based location parsing:

```r
source("tests/test_parsers.R")
```

## Important limitation

The Victoria Police archive contains inconsistent titles and notice wording. The scraper preserves raw HTML and evidence fields so unusual records can be reviewed rather than hidden. If the website changes its structure, inspect `vicpol_public_notice_links_*.csv`, `vicpol_scrape_errors_*.csv` and the saved HTML before changing the parser.
