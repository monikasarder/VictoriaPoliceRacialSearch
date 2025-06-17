library(tidyverse)
library(forcats)
library(writexl)
library(purrr)

# Read in cleaned data
dat <- readRDS("./Output.data/Clean.search.data.RDS")

# Filter: uniform AND person searches AND drug searches AND Melbourne
loc.racial <- dat %>%
  filter(
    !is.na(Station.uniform),
    Contact.Type == "P",
    !is.na(`Search.type - Drugs`),
    Area.type == "Metro"
  ) %>%
  group_by(Division, Station.uniform) %>%
  mutate(
    `Searches: All drug`     = n(),
    `Finds: All drug`        = sum(Found == "Yes", na.rm = TRUE),
    `Hit rate (%): All drug` = round(`Finds: All drug` / `Searches: All drug` * 100, digits = 0)
  ) %>%
  group_by(
    Division, Station.uniform, 
    `Searches: All drug`, `Finds: All drug`, `Hit rate (%): All drug`, 
    Racial.appearance.transformed
  ) %>%
  summarise(
    Finds    = sum(Found == "Yes", na.rm = TRUE),
    Searches = n(),
    .groups  = "drop"
  ) %>%
  mutate(
    `Hit rate (%)` = round(Finds / Searches * 100, digits = 1),
    `Hit rate (%)` = ifelse(Searches <= 20, NA, `Hit rate (%)`),
    `Hit rate (%)` = ifelse(
      Racial.appearance.transformed == "Mediterranean and Middle Eastern - DO NOT USE",
      NA,
      `Hit rate (%)`
    )
  )
 
# Get racial groups in alphabetical order
racial_order <- sort(unique(loc.racial$Racial.appearance.transformed))

# Construct ordered list of pivoted column names
col_order <- map(racial_order, function(race) {
  c(
    paste0("Searches: ", race),
    paste0("Finds: ", race),
    paste0("Hit rate (%): ", race)
  )
}) %>% unlist()

# Identify non-pivoted columns (i.e. not in values_from or names_from)
non_pivot_cols <- setdiff(names(loc.racial), c("Racial.appearance.transformed", "Searches", "Finds", "Hit rate (%)"))

# Pivot and reorder columns: first original columns, then pivoted columns
loc.racial.wide <- loc.racial %>%
  pivot_wider(
    names_from = Racial.appearance.transformed,
    values_from = c(Searches, Finds, `Hit rate (%)`),
    names_glue = "{.value}: {Racial.appearance.transformed}"
  ) %>%
  select(all_of(non_pivot_cols), all_of(col_order))%>%
  mutate(`Relative likelihood: African` = round(((`Hit rate (%): White`-`Hit rate (%): African`)/`Hit rate (%): African`)*100, 
                                                digits = 1 ))


######################
#LOCAL GOVERNMENT AREA
######################

# Filter: uniform AND person searches AND drug searches AND Melbourne
lga.racial <- dat %>%
  filter(
    !is.na(Station.uniform),
    Contact.Type == "P",
    !is.na(`Search.type - Drugs`),
    Area.type == "Metro"
  ) %>%
  group_by(Local.Government.Area) %>%
  mutate(
    `Searches: All drug`     = n(),
    `Finds: All drug`        = sum(Found == "Yes", na.rm = TRUE),
    `Hit rate (%): All drug` = round(`Finds: All drug` / `Searches: All drug` * 100, digits = 0)
  ) %>%
  group_by(
    Local.Government.Area,
    `Searches: All drug`, `Finds: All drug`, `Hit rate (%): All drug`, 
    Racial.appearance.transformed
  ) %>%
  summarise(
    Finds    = sum(Found == "Yes", na.rm = TRUE),
    Searches = n(),
    .groups  = "drop"
  ) %>%
  mutate(
    `Hit rate (%)` = round(Finds / Searches * 100, digits = 1),
    `Hit rate (%)` = ifelse(Searches <= 20, NA, `Hit rate (%)`),
    `Hit rate (%)` = ifelse(
      Racial.appearance.transformed == "Mediterranean and Middle Eastern - DO NOT USE",
      NA,
      `Hit rate (%)`
    )
  )


non_pivot_cols <- setdiff(names(lga.racial), c("Racial.appearance.transformed", "Searches", "Finds", "Hit rate (%)"))

# Pivot and reorder columns: first original columns, then pivoted columns
lga.racial.wide <- lga.racial %>%
  pivot_wider(
    names_from = Racial.appearance.transformed,
    values_from = c(Searches, Finds, `Hit rate (%)`),
    names_glue = "{.value}: {Racial.appearance.transformed}"
  ) %>%
  select(all_of(non_pivot_cols), all_of(col_order)) %>%
  mutate(`Relative likelihood: African` = round(((`Hit rate (%): White`-`Hit rate (%): African`)/`Hit rate (%): African`)*100, 
         digits = 1 ))
         

loc.racial.data <- loc.racial%>%
 select(
   LGA = Local.Government.Area,
   `Total drug searches` = Total.searches,
    `Finds all (%)`,  
    
    `Searches: White` = `Searches_White`, 
    `Finds: White (%)` = `Hit rate_White`,
    
    `Searches: African` = `Searches_African`,
    `Finds: African (%)` = `Hit rate_African`,
   
    `Searches: Asian` = `Searches_Asian`, 
    `Finds: Asian (%)` = `Hit rate_Asian`, 
   
    `Searches: South Asian` = `Searches_South Asian`,
    `Finds: South Asian (%)` = `Hit rate_South Asian`,
   
    `Searches: Aboriginal` = Searches_Aboriginal, 
    `Finds: Aboriginal (%)` = `Hit rate_Aboriginal`, 
   
    `Searches: Middle Eastern` = `Searches_Middle Eastern`, 
    `Finds: Middle Eastern (%)`= `Hit rate_Middle Eastern` , 
   
    `Searches: Mediterranean/Mid` = `Searches_Mediterarranean/Mid`,
    `Finds: Middle Eastern/Mid (%)`=`Hit rate_Mediterarranean/Mid`,
   
    `Searches: Pacific Islander` = `Searches_Pacific Islander`,
    `Finds: Pacific Islander (%)` = `Hit rate_Pacific Islander`, 
   
    `Searches: Other racialised` = `Searches_Other racialised`,
    `Finds: Other racialised (%)` = `Hit rate_Other racialised`,
    `Searches: Missing` = Searches_Missing,
    `Finds: Missing (%)` = `Hit rate_Missing`,
    `Relative likelihood: African`)%>%
  arrange(desc(`Relative likelihood: African`), is.na(`Relative likelihood: African`))
```


```{r choropleth, echo=FALSE, warning=FALSE, message = FALSE}

melbs <- melbs %>%
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
  


datm <- melbs2 %>%
  left_join(loc.racial.data, by = "LGA")%>%
  mutate(Relative.likelihood = case_when(
    is.na(`Relative likelihood: African`) ~ "Insufficient data",
    `Relative likelihood: African` >300 ~ "Over 300%",
    `Relative likelihood: African` >100 ~ "Over 100%",
    `Relative likelihood: African` >50 ~ "Over 50%",
    `Relative likelihood: African` >25 ~ "Over 25%",
    `Relative likelihood: African` >10 ~ "Over 10%",
    `Relative likelihood: African` >-10 ~ "Within 10%",
    `Relative likelihood: African` >-100 ~ "Half as likely"))

datm <- datm %>%
   mutate(Colour = case_when(
    is.na(`Relative likelihood: African`) ~ "#D3D3D3",
    `Relative likelihood: African` >300 ~ "#7f0000",
    `Relative likelihood: African` >100 ~ "#b30000",
    `Relative likelihood: African` >50 ~ "#d7301f",
    `Relative likelihood: African` >25 ~ "#ef6548",
    `Relative likelihood: African` >10 ~ "#fdbb84",
    `Relative likelihood: African` >-10 ~ "#ffffbf",
    `Relative likelihood: African` >- 100~ "#4292c6"))#
    
    


# 1. Set your geo layout (no invalid fields!)
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(type = 'mercator')  # Or just use default
)

# 2. Generate your base plot (choropleth)
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
    showlegend = FALSE  # hide map legend, we build it manually
  )

# 3. Custom legend entries — use type = "scatter" NOT "scattergeo"
legend_items <- datm %>%
  distinct(Relative.likelihood, Colour) %>%
  arrange(match(Relative.likelihood, c(
    "Over 300%", "Over 100%", "Over 50%", "Over 25%", "Over 10%",
    "Within 10%", "Half as likely%", "Insufficent data"
  )))

# Add fake points off-map that don’t mess with projection
for (i in seq_len(nrow(legend_items))) {
  p <- p %>%
    add_trace(
      type = "scatter",
      mode = "markers",
      x = list(NA),  # No actual data
      y = list(NA),
      marker = list(color = legend_items$Colour[i], size = 10),
      name = legend_items$Relative.likelihood[i],
      hoverinfo = "none",
      showlegend = TRUE,
      inherit = FALSE
    )
}

# 4. Apply layout (correct geo and legend positioning)
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



```{r display 2, echo=FALSE, warning=FALSE, message = FALSE}




# Apply the custom cell background color and bold text to the 'Ratio: African to White appearance finds' column

#display data 
cols_to_colour <- names(loc.racial.data)[c(3, 5, 7, 9, 11, 13, 15, 17, 19, 21, 23)]

# Build the base table
dt <- datatable(
  loc.racial.data,
  rownames = FALSE,
  extensions = 'Buttons',
  class = 'cell-border',  # no 'stripe' class
  options = list(
    dom = 'Blfrtip',
    buttons = c('copy', 'excel', 'print'),
    pageLength = 20,
    autoWidth = TRUE
  )
)

# Apply a consistent background color (you can alternate or vary this if desired)
for (col in cols_to_colour) {
  dt <- dt %>% formatStyle(col, backgroundColor = '#f2f2f2')
}

dt


```

# Appendix - ethnic appearance mapping  

Ethnic appearance categories for different years were transformed to enable comparison across the four years. A table of transformations undertaken, along with the number of searches in each category, appears below,


```{r Map 1, echo=FALSE, warning=FALSE, message = FALSE}

ethnic.mapping1 <-dat %>%
  filter(Year %in% c(2022, 2023))%>%
  count(Racial.Appearance.original, Racial.appearance.abridged)

ethnic.mapping1 %>%
  kable()
```

```{r Map 2, echo=FALSE, warning=FALSE, message = FALSE}

ethnic.mapping2 <- dat %>%
  filter(Year %in% c(2018, 2019))%>%
  count(Racial.Appearance.original, Racial.appearance.abridged) 


ethnic.mapping2 %>%
  kable()
```