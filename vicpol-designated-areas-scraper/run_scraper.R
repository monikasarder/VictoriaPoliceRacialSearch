detect_project_dir <- function() {
  file_argument <- grep("^--file=", commandArgs(), value = TRUE)
  command_line_file <- if (length(file_argument) > 0) {
    sub("^--file=", "", file_argument[[1]])
  } else {
    NA_character_
  }

  sourced_files <- vapply(
    sys.frames(),
    function(frame) {
      if (is.null(frame$ofile)) NA_character_ else as.character(frame$ofile)[1]
    },
    character(1)
  )

  file_candidates <- c(command_line_file, sourced_files)
  file_candidates <- file_candidates[!is.na(file_candidates)]

  candidate_directories <- c(
    dirname(file_candidates),
    getwd(),
    file.path(getwd(), "vicpol-designated-areas-scraper"),
    dirname(getwd()),
    file.path(dirname(getwd()), "vicpol-designated-areas-scraper")
  )
  candidate_directories <- unique(normalizePath(
    candidate_directories,
    winslash = "/",
    mustWork = FALSE
  ))

  matches <- candidate_directories[
    file.exists(file.path(
      candidate_directories,
      "R",
      "scraper_functions.R"
    ))
  ]

  if (length(matches) == 0) {
    stop(
      paste0(
        "Could not locate the vicpol-designated-areas-scraper folder. ",
        "Keep run_scraper.R and the R folder together, or run ",
        "source('vicpol-designated-areas-scraper/run_scraper.R') from ",
        "the VictoriaPoliceRacialSearch project root."
      ),
      call. = FALSE
    )
  }

  matches[[1]]
}

main <- function() {
  project_dir <- detect_project_dir()
  previous_working_directory <- setwd(project_dir)
  on.exit(setwd(previous_working_directory), add = TRUE)

  source(file.path(project_dir, "R", "scraper_functions.R"))

  result <- scrape_vicpol_designated_areas(
    index_url = "https://www.police.vic.gov.au/public-notices",
    output_dir = "outputs",
    raw_html_root = "data-raw/vicpol-public-notices",
    overrides_path = "config/location_overrides.csv",
    delay_seconds = 2
  )

  message("Designated-area records: ", nrow(result$data))
  message("Fetch or parse errors: ", nrow(result$errors))
  message("CSV: ", result$files$csv)
  message("RDS: ", result$files$rds)
  message("Session information: ", result$files$session)
  message("Raw HTML: ", result$files$raw_html_dir)

  invisible(result)
}

main()
getwd()
