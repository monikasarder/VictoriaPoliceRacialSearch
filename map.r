
```{r choropleth, echo=FALSE, warning=FALSE, message = FALSE}



melbs <- read_absmap("lga2022")

melbs <- melbs %>%
  filter(state_name_2021 == "Victoria")  %>%
  mutate(LGA = lga_name_2022)

melbs2 <- melbs %>%
  select(LGA, geometry, cent_long, cent_lat)%>%
  mutate(LGA = case_when(
    LGA %in% c("Moreland") ~ "Merri-bek",
    LGA == "Kingston (Vic.)" ~ "Kingston",
    LGA == "Bayside (Vic.)"~ "Bayside",
    TRUE ~ LGA
  ))

melbs2 <- melbs2 %>%
  filter(LGA %in% c(unique(loc.racial.data$LGA)))
```

```{r join sets, echo=FALSE, warning=FALSE, message = FALSE}
datm <- melbs2 %>%
  left_join(loc.racial.data, by = "LGA") %>%
  mutate(
    Relative.likelihood = case_when(
      is.na(`Relative likelihood: African`) ~ "Insufficient data",
      `Relative likelihood: African` <= -50 ~ "Over 50% less likely",
      `Relative likelihood: African` <= -20 ~ "Over 20% less likely",
      `Relative likelihood: African` <  20  ~ "Within ±20%",
      TRUE ~ "Over 20% more likely"
    ),
    Colour = case_when(
      Relative.likelihood == "Insufficient data"       ~ "#D3D3D3",  # Light grey
      Relative.likelihood == "Over 50% less likely"    ~ "#d73027",  # Red
      Relative.likelihood == "Over 20% less likely"    ~ "#fb8c00",  # Orange
      Relative.likelihood == "Within ±20%"             ~ "#fff9c4",  # Pale yellow
      Relative.likelihood == "Over 20% more likely"    ~ "#673ab7"   # Purple
    )
  )



```

```{r final heat map, echo=FALSE, warning=FALSE, message = FALSE}
# 1. Set your geo layout
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'mercator')
)

# 2. Generate the base plot (choropleth)
p <- plot_ly() %>%
  add_sf(
    data = datm,
    split = ~LGA,
    color = ~I(Colour),
    text = ~paste0(
      "LGA: ", LGA, "<br>",
      "Relative likelihood: ", `Relative likelihood: African`, "<br>",
      "Searches: ", `Total drug searches`
    ),
    hoveron = "fills",
    hoverinfo = "text",
    showlegend = FALSE
  )

# 3. Custom legend entries – new categories
legend_levels <- c(
  "Over 50% less likely",
  "Over 20% less likely",
  "Within ±20%",
  "Over 20% more likely",
  "Insufficient data"
)

legend_items <- datm %>%
  distinct(Relative.likelihood, Colour) %>%
  filter(Relative.likelihood %in% legend_levels) %>%
  mutate(Relative.likelihood = factor(Relative.likelihood, levels = legend_levels)) %>%
  arrange(Relative.likelihood)

# 4. Add manual legend items as off-map scatter traces
for (i in seq_len(nrow(legend_items))) {
  p <- p %>%
    add_trace(
      type = "scatter",
      mode = "markers",
      x = list(NA),
      y = list(NA),
      marker = list(color = legend_items$Colour[i], size = 10),
      name = as.character(legend_items$Relative.likelihood[i]),
      hoverinfo = "none",
      showlegend = TRUE,
      inherit = FALSE
    )
}

# 5. Final layout with geo and legend
p <- p %>%
  layout(
    geo = g,
    legend = list(
      title = list(text = "Relative likelihood"),
      x = 1.02,
      y = 0.95
    )
  )

p

```

