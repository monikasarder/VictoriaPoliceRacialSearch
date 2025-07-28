**Figure 1 Changes in hit rates by racial appearance over time and compared to the average**
  ```{r Finds race year, echo=FALSE, warning=FALSE, message = FALSE}
trend <- dat %>%
  filter(!Racial.appearance.transformed %in% c("Missing", "Mediterranean AND Middle Eastern - DO NOT USE", "Other"))%>%
  mutate(Racial.appearance = fct_infreq(Racial.appearance.transformed))%>%
  group_by(Year, `Racial.appearance`)%>%
  summarise(Finds = sum(Found == "Yes", na.rm = T), `Total searches`= n())%>%
  mutate(`Hit rate` = round(Finds/`Total searches`*100, digits = 1)) %>%
  ungroup()%>%
  ggplot(aes(x = Year, y = `Hit rate`)) +geom_col(fill = "darkblue")+
  facet_wrap(~`Racial.appearance`)+ 
  geom_hline(yintercept = 17.2,linetype="dotted", color = "red") +theme(legend.position = "none")

ggplotly(trend)
```

