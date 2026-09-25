find_project_file <- function(relative_path, start_dir = getwd(), max_levels = 5) {
  current_dir <- normalizePath(start_dir, winslash = "/", mustWork = TRUE)

  for (level in 0:max_levels) {
    candidate <- file.path(current_dir, relative_path)
    if (file.exists(candidate)) {
      return(normalizePath(candidate, winslash = "/", mustWork = TRUE))
    }

    parent_dir <- dirname(current_dir)
    if (identical(parent_dir, current_dir)) {
      break
    }
    current_dir <- parent_dir
  }

  NULL
}

load_csa_sub_data <- function() {
  rds_relative <- file.path(
    "R-code-cleaning", "Processed", "CSA sub raw.RDS"
  )
  excel_relative <- file.path(
    "Primary datasets - VicPol Search",
    "Data_Tables_LGA_Criminal_Incidents_Year_Ending_June_2026.xlsx"
  )

  rds_path <- find_project_file(rds_relative)
  if (!is.null(rds_path)) {
    return(readRDS(rds_path))
  }

  excel_path <- find_project_file(excel_relative)
  if (is.null(excel_path)) {
    excel_path <- find_project_file(
      "Data_Tables_LGA_Criminal_Incidents_Year_Ending_June_2026.xlsx"
    )
  }

  if (is.null(excel_path)) {
    stop(
      paste0(
        "Could not find either '", rds_relative, "' or the source Excel ",
        "workbook. See README.md for the expected folder layout."
      ),
      call. = FALSE
    )
  }

  readxl::read_xlsx(
    excel_path,
    sheet = "Table 03",
    .name_repair = "universal"
  )
}

load_csa_lga_data <- function() {
  rds_relative <- file.path(
    "R-code-cleaning", "Processed", "CSA lga raw.RDS"
  )
  excel_relative <- file.path(
    "Primary datasets - VicPol Search",
    "Data_Tables_LGA_Criminal_Incidents_Year_Ending_June_2026.xlsx"
  )

  rds_path <- find_project_file(rds_relative)
  if (!is.null(rds_path)) {
    return(readRDS(rds_path))
  }

  excel_path <- find_project_file(excel_relative)
  if (is.null(excel_path)) {
    excel_path <- find_project_file(
      "Data_Tables_LGA_Criminal_Incidents_Year_Ending_June_2026.xlsx"
    )
  }

  if (is.null(excel_path)) {
    stop(
      paste0(
        "Could not find either '", rds_relative, "' or the source Excel ",
        "workbook. See README.md for the expected folder layout."
      ),
      call. = FALSE
    )
  }

  readxl::read_xlsx(
    excel_path,
    sheet = "Table 02",
    .name_repair = "universal"
  )
}

canonical_name <- function(x) {
  tolower(gsub("[^a-zA-Z0-9]", "", x))
}

resolve_source_columns <- function(data, wanted) {
  available <- stats::setNames(names(data), canonical_name(names(data)))
  missing_columns <- wanted[!wanted %in% names(available)]

  if (length(missing_columns) > 0) {
    stop(
      paste(
        "Missing required columns:",
        paste(names(missing_columns), collapse = ", ")
      ),
      call. = FALSE
    )
  }

  source_names <- unname(available[wanted])
  names(source_names) <- names(wanted)
  source_names
}

standardise_csa_sub_data <- function(data) {
  wanted <- c(
    Year = "year",
    Area_Type = "areatype",
    LGA = "localgovernmentarea",
    Postcode = "postcode",
    Suburb = "suburbtownname",
    Division = "offencedivision",
    Subdivision = "offencesubdivision",
    Subgroup = "offencesubgroup",
    Incidents = "incidentsrecorded"
  )

  source_names <- resolve_source_columns(data, wanted)

  data |>
    dplyr::transmute(
      Year = as.integer(.data[[source_names[["Year"]]]]),
      Area_Type = as.character(.data[[source_names[["Area_Type"]]]]),
      LGA = as.character(.data[[source_names[["LGA"]]]]),
      Postcode = as.character(.data[[source_names[["Postcode"]]]]),
      Suburb = as.character(.data[[source_names[["Suburb"]]]]),
      Division = as.character(.data[[source_names[["Division"]]]]),
      Subdivision = as.character(.data[[source_names[["Subdivision"]]]]),
      Subgroup = as.character(.data[[source_names[["Subgroup"]]]]),
      Incidents = as.numeric(.data[[source_names[["Incidents"]]]])
    ) |>
    dplyr::filter(
      !is.na(.data$Year),
      !is.na(.data$Area_Type),
      !is.na(.data$LGA),
      !is.na(.data$Suburb),
      !is.na(.data$Division),
      !is.na(.data$Subdivision),
      !is.na(.data$Subgroup)
    )
}

standardise_csa_lga_data <- function(data) {
  wanted <- c(
    Year = "year",
    Area_Type = "areatype",
    LGA = "localgovernmentarea",
    Division = "offencedivision",
    Subdivision = "offencesubdivision",
    Subgroup = "offencesubgroup",
    Incidents = "incidentsrecorded",
    LGA_Rate = "lgarateper100000population"
  )

  source_names <- resolve_source_columns(data, wanted)

  data |>
    dplyr::transmute(
      Year = as.integer(.data[[source_names[["Year"]]]]),
      Area_Type = as.character(.data[[source_names[["Area_Type"]]]]),
      LGA = as.character(.data[[source_names[["LGA"]]]]),
      Division = as.character(.data[[source_names[["Division"]]]]),
      Subdivision = as.character(.data[[source_names[["Subdivision"]]]]),
      Subgroup = as.character(.data[[source_names[["Subgroup"]]]]),
      Incidents = as.numeric(.data[[source_names[["Incidents"]]]]),
      LGA_Rate = as.numeric(.data[[source_names[["LGA_Rate"]]]])
    ) |>
    dplyr::filter(
      !is.na(.data$Year),
      !is.na(.data$Area_Type),
      !is.na(.data$LGA),
      !is.na(.data$Division),
      !is.na(.data$Subdivision),
      !is.na(.data$Subgroup)
    )
}

filter_selected <- function(data, column, selected) {
  if (is.null(selected) || length(selected) == 0) {
    return(data)
  }

  dplyr::filter(data, .data[[column]] %in% selected)
}

keep_valid_selections <- function(selected, choices) {
  if (is.null(selected)) {
    selected <- character()
  }
  intersect(selected, choices)
}
