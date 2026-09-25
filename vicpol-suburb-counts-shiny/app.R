library(shiny)
library(tidyverse)
library(readxl)
library(plotly)

source(file.path("R", "data_helpers.R"), local = TRUE)

csa_sub <- load_csa_sub_data() |>
  standardise_csa_sub_data() |>
  filter(Year %in% 2018:2026)

csa_lga <- load_csa_lga_data() |>
  standardise_csa_lga_data() |>
  filter(Year %in% 2018:2026)

sort_choices <- function(x) sort(unique(x[!is.na(x) & nzchar(x)]))

select_options <- list(
  plugins = list("remove_button"),
  placeholder = "Leave blank for all"
)

suburb_options <- list(
  plugins = list("remove_button"),
  placeholder = "Leave blank to sum to the selected parent geography"
)

ui <- navbarPage(
  title = "Victorian criminal incidents",
  id = "main_tab",
  header = tags$head(
    tags$style(HTML("
      body { background: #f5f6f8; }
      .navbar { margin-bottom: 0; }
      .page-wrap { padding-top: 22px; }
      .app-title { margin: 0 0 2px 0; font-weight: 650; }
      .app-subtitle { color: #5f6368; margin-bottom: 18px; }
      .sidebar-panel {
        background: #ffffff;
        border: 1px solid #dfe3e8;
        border-radius: 8px;
        padding: 18px;
        position: sticky;
        top: 12px;
      }
      .chart-panel, .table-panel {
        background: #ffffff;
        border: 1px solid #dfe3e8;
        border-radius: 8px;
        padding: 18px;
        margin-bottom: 16px;
      }
      .hierarchy-label {
        color: #5f6368;
        font-size: 12px;
        margin: -5px 0 12px 0;
      }
      .help-note {
        color: #5f6368;
        font-size: 12px;
        line-height: 1.45;
      }
      .btn-row { display: flex; gap: 8px; flex-wrap: wrap; }
      .shiny-output-error-validation {
        color: #5f6368;
        background: #fff8e1;
        border: 1px solid #f0d98c;
        border-radius: 6px;
        padding: 12px;
      }
    "))
  ),

  tabPanel(
    "Suburb counts",
    div(
      class = "container-fluid page-wrap",
      h2("Recorded criminal incident counts by suburb", class = "app-title"),
      div(
        "Table 03 · year ending June · 2018–2026",
        class = "app-subtitle"
      ),
      fluidRow(
        column(
          width = 3,
          div(
            class = "sidebar-panel",
            h4("Filters"),
            p("Geography", class = "hierarchy-label"),
            selectizeInput(
              "sub_area_type", "Area Type",
              choices = sort_choices(csa_sub$Area_Type),
              selected = character(),
              multiple = TRUE,
              options = select_options
            ),
            selectizeInput(
              "sub_lga", "Local Government Area",
              choices = sort_choices(csa_sub$LGA),
              selected = character(),
              multiple = TRUE,
              options = select_options
            ),
            selectizeInput(
              "sub_suburb", "Suburb/Town Name",
              choices = sort_choices(csa_sub$Suburb),
              selected = character(),
              multiple = TRUE,
              options = suburb_options
            ),

            tags$hr(),
            p("Offence hierarchy", class = "hierarchy-label"),
            selectizeInput(
              "sub_division", "Offence Division",
              choices = sort_choices(csa_sub$Division),
              multiple = TRUE,
              options = select_options
            ),
            selectizeInput(
              "sub_subdivision", "Offence Subdivision",
              choices = sort_choices(csa_sub$Subdivision),
              multiple = TRUE,
              options = select_options
            ),
            selectizeInput(
              "sub_subgroup", "Offence Subgroup",
              choices = sort_choices(csa_sub$Subgroup),
              multiple = TRUE,
              options = select_options
            ),

            div(
              class = "btn-row",
              actionButton("sub_reset", "Reset filters"),
              downloadButton("sub_download", "Download summary")
            ),
            tags$hr(),
            p(
              "Leave Area Type, LGA and Suburb/Town Name blank for one All Victoria line. With no LGA selected, selected area types are summed separately. With an LGA selected but no suburb, all suburbs are summed into one line per LGA.",
              class = "help-note"
            ),
            p(
              "Blank offence filters include all categories beneath that level. Division is the parent of Subdivision, which is the parent of Subgroup.",
              class = "help-note"
            )
          )
        ),

        column(
          width = 9,
          div(
            class = "chart-panel",
            uiOutput("sub_selection_summary"),
            plotlyOutput("sub_trend_plot", height = "560px")
          ),
          div(
            class = "table-panel",
            h4("Annual summary"),
            p(
              "Counts are summed across the selected, mutually exclusive offence subgroups and the lowest selected geography level.",
              class = "help-note"
            ),
            tableOutput("sub_annual_table")
          )
        )
      )
    )
  ),

  tabPanel(
    "LGA rates",
    div(
      class = "container-fluid page-wrap",
      h2("Recorded criminal incident rate by LGA", class = "app-title"),
      div(
        "Table 02 · incidents per 100,000 population · highest to lowest",
        class = "app-subtitle"
      ),
      fluidRow(
        column(
          width = 3,
          div(
            class = "sidebar-panel",
            h4("Filters"),
            p("Geography", class = "hierarchy-label"),
            selectizeInput(
              "rate_area_type", "Area Type",
              choices = sort_choices(csa_lga$Area_Type),
              selected = character(),
              multiple = TRUE,
              options = select_options
            ),
            selectizeInput(
              "rate_year", "Year ending June",
              choices = 2018:2026,
              selected = 2026,
              multiple = TRUE,
              options = select_options
            ),

            tags$hr(),
            p("Offence hierarchy", class = "hierarchy-label"),
            selectizeInput(
              "rate_division", "Offence Division",
              choices = sort_choices(csa_lga$Division),
              multiple = TRUE,
              options = select_options
            ),
            selectizeInput(
              "rate_subdivision", "Offence Subdivision",
              choices = sort_choices(csa_lga$Subdivision),
              multiple = TRUE,
              options = select_options
            ),
            selectizeInput(
              "rate_subgroup", "Offence Subgroup",
              choices = sort_choices(csa_lga$Subgroup),
              multiple = TRUE,
              options = select_options
            ),

            div(
              class = "btn-row",
              actionButton("rate_reset", "Reset filters"),
              downloadButton("rate_download", "Download ranking")
            ),
            tags$hr(),
            p(
              "The default year is 2026. Leave Year blank to use all years. When more than one year is included, the chart shows the average annual LGA rate; annual rates are not added together.",
              class = "help-note"
            ),
            p(
              "Leave any offence filter blank to include and sum all mutually exclusive child categories beneath it.",
              class = "help-note"
            )
          )
        ),

        column(
          width = 9,
          div(
            class = "chart-panel",
            uiOutput("rate_selection_summary"),
            plotlyOutput("rate_bar_plot", height = "1700px")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  sub_geography_rows <- reactive({
    csa_sub |>
      filter_selected("Area_Type", input$sub_area_type) |>
      filter_selected("LGA", input$sub_lga) |>
      filter_selected("Suburb", input$sub_suburb)
  })

  observeEvent(input$sub_area_type, {
    available <- csa_sub |>
      filter_selected("Area_Type", input$sub_area_type) |>
      pull(LGA) |>
      sort_choices()

    updateSelectizeInput(
      session, "sub_lga",
      choices = available,
      selected = keep_valid_selections(input$sub_lga, available),
      server = TRUE
    )
  }, ignoreInit = TRUE)

  observeEvent(list(input$sub_area_type, input$sub_lga), {
    available_rows <- csa_sub |>
      filter_selected("Area_Type", input$sub_area_type) |>
      filter_selected("LGA", input$sub_lga)

    available <- available_rows |>
      pull(Suburb) |>
      sort_choices()

    updateSelectizeInput(
      session, "sub_suburb",
      choices = available,
      selected = keep_valid_selections(input$sub_suburb, available),
      server = TRUE
    )
  }, ignoreInit = TRUE)

  observeEvent(input$sub_division, {
    available <- csa_sub |>
      filter_selected("Division", input$sub_division) |>
      pull(Subdivision) |>
      sort_choices()

    updateSelectizeInput(
      session, "sub_subdivision",
      choices = available,
      selected = keep_valid_selections(input$sub_subdivision, available),
      server = TRUE
    )
  }, ignoreInit = TRUE)

  observeEvent(list(input$sub_division, input$sub_subdivision), {
    available <- csa_sub |>
      filter_selected("Division", input$sub_division) |>
      filter_selected("Subdivision", input$sub_subdivision) |>
      pull(Subgroup) |>
      sort_choices()

    updateSelectizeInput(
      session, "sub_subgroup",
      choices = available,
      selected = keep_valid_selections(input$sub_subgroup, available),
      server = TRUE
    )
  }, ignoreInit = TRUE)

  observeEvent(input$sub_reset, {
    updateSelectizeInput(session, "sub_area_type", selected = character())
    updateSelectizeInput(session, "sub_lga", selected = character())
    updateSelectizeInput(session, "sub_suburb", selected = character())
    updateSelectizeInput(session, "sub_division", selected = character())
    updateSelectizeInput(session, "sub_subdivision", selected = character())
    updateSelectizeInput(session, "sub_subgroup", selected = character())
  })

  sub_filtered_rows <- reactive({
    sub_geography_rows() |>
      filter_selected("Division", input$sub_division) |>
      filter_selected("Subdivision", input$sub_subdivision) |>
      filter_selected("Subgroup", input$sub_subgroup)
  })

  sub_selected_areas <- reactive({
    if (length(input$sub_suburb) > 0) {
      sub_geography_rows() |>
        group_by(Area_Type, LGA, Suburb) |>
        summarise(
          Postcodes = paste(
            sort(unique(Postcode[!is.na(Postcode) & nzchar(Postcode)])),
            collapse = ", "
          ),
          .groups = "drop"
        ) |>
        mutate(
          Geography = Suburb,
          Series = paste0(Suburb, " (", LGA, ")")
        ) |>
        select(-Suburb) |>
        arrange(Area_Type, LGA, Geography)
    } else if (length(input$sub_lga) > 0) {
      sub_geography_rows() |>
        distinct(Area_Type, LGA) |>
        mutate(
          Geography = "All suburbs/towns (LGA total)",
          Postcodes = "All postcodes in LGA",
          Series = LGA
        ) |>
        arrange(Area_Type, LGA)
    } else if (length(input$sub_area_type) > 0) {
      sub_geography_rows() |>
        distinct(Area_Type) |>
        mutate(
          LGA = "All LGAs",
          Geography = paste0("All LGAs and suburbs (", Area_Type, " total)"),
          Postcodes = "All postcodes in area type",
          Series = Area_Type
        ) |>
        arrange(Area_Type)
    } else {
      tibble(
        Area_Type = "All",
        LGA = "All LGAs",
        Geography = "All LGAs and suburbs (Victoria total)",
        Postcodes = "All Victorian postcodes",
        Series = "All Victoria"
      )
    }
  })

  sub_annual_data <- reactive({
    areas <- sub_selected_areas() |>
      mutate(.area_id = row_number())

    if (length(input$sub_suburb) > 0) {
      summarised <- sub_filtered_rows() |>
        group_by(Year, Area_Type, LGA, Suburb) |>
        summarise(Incidents = sum(Incidents, na.rm = TRUE), .groups = "drop") |>
        mutate(Series = paste0(Suburb, " (", LGA, ")")) |>
        select(Year, Series, Incidents)
    } else if (length(input$sub_lga) > 0) {
      summarised <- sub_filtered_rows() |>
        group_by(Year, Area_Type, LGA) |>
        summarise(Incidents = sum(Incidents, na.rm = TRUE), .groups = "drop") |>
        mutate(Series = LGA) |>
        select(Year, Series, Incidents)
    } else if (length(input$sub_area_type) > 0) {
      summarised <- sub_filtered_rows() |>
        group_by(Year, Area_Type) |>
        summarise(Incidents = sum(Incidents, na.rm = TRUE), .groups = "drop") |>
        mutate(Series = Area_Type) |>
        select(Year, Series, Incidents)
    } else {
      summarised <- sub_filtered_rows() |>
        group_by(Year) |>
        summarise(Incidents = sum(Incidents, na.rm = TRUE), .groups = "drop") |>
        mutate(Series = "All Victoria") |>
        select(Year, Series, Incidents)
    }

    tidyr::expand_grid(
      Year = 2018:2026,
      .area_id = areas$.area_id
    ) |>
      left_join(areas, by = ".area_id") |>
      select(-.area_id) |>
      left_join(summarised, by = c("Year", "Series")) |>
      mutate(Incidents = replace_na(Incidents, 0)) |>
      arrange(Series, Year) |>
      group_by(Series) |>
      mutate(
        YoY = if_else(
          lag(Incidents) > 0,
          ((Incidents / lag(Incidents)) - 1) * 100,
          NA_real_
        )
      ) |>
      ungroup()
  })

  output$sub_selection_summary <- renderUI({
    n_series <- nrow(sub_selected_areas())
    geography_level <- case_when(
      length(input$sub_suburb) > 0 ~ "suburb/town",
      length(input$sub_lga) > 0 ~ "LGA total",
      length(input$sub_area_type) > 0 ~ "area-type total",
      TRUE ~ "Victoria total"
    )
    offence_level <- case_when(
      length(input$sub_subgroup) > 0 ~ paste(length(input$sub_subgroup), "subgroup(s) selected"),
      length(input$sub_subdivision) > 0 ~ paste(length(input$sub_subdivision), "subdivision(s) selected"),
      length(input$sub_division) > 0 ~ paste(length(input$sub_division), "division(s) selected"),
      TRUE ~ "all offence divisions"
    )

    tagList(
      h4("Trend in recorded criminal incidents"),
      p(
        paste(n_series, geography_level, "series;", offence_level),
        class = "help-note"
      )
    )
  })

  output$sub_trend_plot <- renderPlotly({
    plot_data <- sub_annual_data() |>
      mutate(
        Hover = paste0(
          "<b>", Geography, "</b>",
          "<br>Area type: ", Area_Type,
          "<br>LGA: ", LGA,
          "<br>Postcode(s): ", Postcodes,
          "<br>Year ending June: ", Year,
          "<br>Recorded incidents: ",
          formatC(Incidents, format = "f", digits = 0, big.mark = ","),
          "<br>YoY change: ",
          if_else(
            is.na(YoY),
            "Not available",
            paste0(formatC(YoY, format = "f", digits = 1), "%")
          )
        )
      )

    validate(
      need(nrow(plot_data) > 0, "No data match these filters."),
      need(
        n_distinct(plot_data$Series) <= 20,
        "This selection produces more than 20 lines. Narrow the LGA or suburb/town filters."
      )
    )

    segments <- plot_data |>
      group_by(Series) |>
      arrange(Year, .by_group = TRUE) |>
      mutate(
        Year_end = lead(Year),
        Incidents_end = lead(Incidents),
        Period = if_else(
          Year >= 2019 & Year_end <= 2021,
          "COVID-affected years (2019–2021)",
          "Other years"
        )
      ) |>
      filter(!is.na(Year_end)) |>
      ungroup()

    chart <- ggplot() +
      geom_segment(
        data = segments,
        aes(
          x = Year, y = Incidents,
          xend = Year_end, yend = Incidents_end,
          colour = Series, linetype = Period
        ),
        linewidth = 1.05,
        lineend = "round"
      ) +
      geom_point(
        data = plot_data,
        aes(x = Year, y = Incidents, colour = Series, text = Hover),
        size = 2.2
      ) +
      scale_x_continuous(breaks = 2018:2026) +
      scale_y_continuous(
        labels = scales::label_number(accuracy = 1, big.mark = ","),
        expand = expansion(mult = c(0.02, 0.08))
      ) +
      scale_linetype_manual(
        values = c(
          "Other years" = "solid",
          "COVID-affected years (2019–2021)" = "dotted"
        )
      ) +
      labs(
        x = "Year ending June",
        y = "Recorded incidents",
        colour = "Geography",
        linetype = NULL,
        caption = "The section joining 2019–2021 is dotted to distinguish the COVID-affected period."
      ) +
      theme_minimal(base_size = 13) +
      theme(
        panel.grid.minor = element_blank(),
        legend.position = "bottom",
        legend.box = "vertical",
        plot.caption = element_text(colour = "#5f6368", hjust = 0)
      )

    ggplotly(chart, tooltip = "text") |>
      layout(hovermode = "closest", hoverlabel = list(align = "left")) |>
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("select2d", "lasso2d")
      )
  })

  output$sub_annual_table <- renderTable({
    sub_annual_data() |>
      mutate(
        Incidents = scales::comma(Incidents, accuracy = 1),
        `YoY change` = if_else(
          is.na(YoY),
          "—",
          paste0(formatC(YoY, format = "f", digits = 1), "%")
        )
      ) |>
      select(
        Year,
        `Area Type` = Area_Type,
        `Local Government Area` = LGA,
        Geography,
        `Postcode(s)` = Postcodes,
        `Recorded incidents` = Incidents,
        `YoY change`
      )
  }, striped = TRUE, bordered = FALSE, spacing = "s")

  output$sub_download <- downloadHandler(
    filename = function() {
      paste0("vicpol-suburb-lga-annual-counts-", Sys.Date(), ".csv")
    },
    content = function(file) {
      sub_annual_data() |>
        rename(YoY_Percent = YoY) |>
        readr::write_csv(file)
    }
  )

  observeEvent(input$rate_division, {
    available <- csa_lga |>
      filter_selected("Division", input$rate_division) |>
      pull(Subdivision) |>
      sort_choices()

    updateSelectizeInput(
      session, "rate_subdivision",
      choices = available,
      selected = keep_valid_selections(input$rate_subdivision, available),
      server = TRUE
    )
  }, ignoreInit = TRUE)

  observeEvent(list(input$rate_division, input$rate_subdivision), {
    available <- csa_lga |>
      filter_selected("Division", input$rate_division) |>
      filter_selected("Subdivision", input$rate_subdivision) |>
      pull(Subgroup) |>
      sort_choices()

    updateSelectizeInput(
      session, "rate_subgroup",
      choices = available,
      selected = keep_valid_selections(input$rate_subgroup, available),
      server = TRUE
    )
  }, ignoreInit = TRUE)

  observeEvent(input$rate_reset, {
    updateSelectizeInput(session, "rate_area_type", selected = character())
    updateSelectizeInput(session, "rate_year", selected = 2026)
    updateSelectizeInput(session, "rate_division", selected = character())
    updateSelectizeInput(session, "rate_subdivision", selected = character())
    updateSelectizeInput(session, "rate_subgroup", selected = character())
  })

  rate_selected_years <- reactive({
    if (is.null(input$rate_year) || length(input$rate_year) == 0) {
      2018:2026
    } else {
      sort(as.integer(input$rate_year))
    }
  })

  rate_filtered_rows <- reactive({
    csa_lga |>
      filter(Year %in% rate_selected_years()) |>
      filter_selected("Area_Type", input$rate_area_type) |>
      filter_selected("Division", input$rate_division) |>
      filter_selected("Subdivision", input$rate_subdivision) |>
      filter_selected("Subgroup", input$rate_subgroup)
  })

  rate_annual_data <- reactive({
    summarised <- rate_filtered_rows() |>
      group_by(Year, Area_Type, LGA) |>
      summarise(
        Rate = sum(LGA_Rate, na.rm = TRUE),
        Incidents = sum(Incidents, na.rm = TRUE),
        .groups = "drop"
      )

    lga_lookup <- csa_lga |>
      filter_selected("Area_Type", input$rate_area_type) |>
      distinct(Area_Type, LGA)

    lga_lookup |>
      tidyr::crossing(Year = rate_selected_years()) |>
      select(Year, Area_Type, LGA) |>
      left_join(summarised, by = c("Year", "Area_Type", "LGA")) |>
      mutate(
        Rate = replace_na(Rate, 0),
        Incidents = replace_na(Incidents, 0)
      )
  })

  rate_ranking <- reactive({
    years <- rate_selected_years()

    rate_annual_data() |>
      group_by(Area_Type, LGA) |>
      summarise(
        Rate = mean(Rate, na.rm = TRUE),
        Incidents = sum(Incidents, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(desc(Rate), LGA) |>
      mutate(
        Rank = row_number(),
        Years = paste(years, collapse = ", "),
        Year_Count = length(years)
      )
  })

  output$rate_selection_summary <- renderUI({
    years <- rate_selected_years()
    rate_label <- if (length(years) == 1) {
      paste("Year ending June", years)
    } else {
      paste0(
        "Average annual rate across ", min(years), "–", max(years),
        " (", length(years), " years)"
      )
    }

    offence_level <- case_when(
      length(input$rate_subgroup) > 0 ~ paste(length(input$rate_subgroup), "subgroup(s) selected"),
      length(input$rate_subdivision) > 0 ~ paste(length(input$rate_subdivision), "subdivision(s) selected"),
      length(input$rate_division) > 0 ~ paste(length(input$rate_division), "division(s) selected"),
      TRUE ~ "all offence divisions"
    )

    area_label <- if (length(input$rate_area_type) == 0) {
      "all area types"
    } else {
      paste(input$rate_area_type, collapse = " and ")
    }

    tagList(
      h4("LGA ranking"),
      p(
        paste0(rate_label, "; ", area_label, "; ", offence_level),
        class = "help-note"
      )
    )
  })

  output$rate_bar_plot <- renderPlotly({
    plot_data <- rate_ranking() |>
      arrange(Rate) |>
      mutate(
        LGA = factor(LGA, levels = LGA),
        Hover = paste0(
          "<b>", LGA, "</b>",
          "<br>Area type: ", Area_Type,
          "<br>Rank: ", Rank,
          "<br>",
          if_else(
            Year_Count == 1,
            "LGA rate per 100,000: ",
            "Average annual LGA rate per 100,000: "
          ),
          formatC(Rate, format = "f", digits = 1, big.mark = ","),
          "<br>Years: ", Years,
          "<br>Total recorded incidents across selected years: ",
          formatC(Incidents, format = "f", digits = 0, big.mark = ",")
        )
      )

    validate(need(nrow(plot_data) > 0, "No data match these filters."))

    x_label <- if (length(rate_selected_years()) == 1) {
      "LGA rate per 100,000 population"
    } else {
      "Average annual LGA rate per 100,000 population"
    }

    chart <- ggplot(
      plot_data,
      aes(x = Rate, y = LGA, fill = Area_Type, text = Hover)
    ) +
      geom_col(width = 0.72) +
      scale_fill_manual(
        values = c("Metro" = "#2F6F9F", "Regional" = "#D9862C"),
        drop = FALSE
      ) +
      scale_x_continuous(
        labels = scales::label_number(accuracy = 1, big.mark = ","),
        expand = expansion(mult = c(0, 0.08))
      ) +
      labs(x = x_label, y = NULL, fill = "Area type") +
      theme_minimal(base_size = 12) +
      theme(
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank(),
        legend.position = "top"
      )

    ggplotly(chart, tooltip = "text") |>
      layout(
        hovermode = "closest",
        hoverlabel = list(align = "left"),
        margin = list(l = 135)
      ) |>
      config(
        displaylogo = FALSE,
        modeBarButtonsToRemove = c("select2d", "lasso2d")
      )
  })

  output$rate_download <- downloadHandler(
    filename = function() {
      paste0("vicpol-lga-rate-ranking-", Sys.Date(), ".csv")
    },
    content = function(file) {
      rate_ranking() |>
        select(Rank, Area_Type, LGA, Rate_per_100000 = Rate, Years, Incidents) |>
        readr::write_csv(file)
    }
  )
}

shinyApp(ui, server)
