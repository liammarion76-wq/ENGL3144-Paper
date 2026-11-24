library(tidyverse)

result = FMC %>%
  mutate(
    iccat = case_when(IC == 1 ~ "Present", IC == 0.5 ~ "Unsure", IC == 0 ~ "Not Present"),
    aaeccat = case_when(AAEC == 1 ~ "Present", AAEC == 0.5 ~ "Unsure", AAEC == 0 ~ "Not Present"),
    overlapcat = case_when(AAEC_IC == 1 ~ "Present", AAEC_IC == 0.5 ~ "Unsure", AAEC_IC == 0 ~ "Not Present")
  )

# tokens per brand
result = result %>%
  mutate(combo = case_when(
    iccat == "Present" & aaeccat == "Present" ~ "Internet + AAE",
    iccat == "Present" & aaeccat == "Unsure"  ~ "Internet + Unsure AAE",
    iccat == "Present" ~ "Internet only",
    aaeccat == "Present" ~ "AAE only",
    aaeccat == "Unsure" ~ "Unsure AAE only",
    TRUE ~ "Other"
  ))

figdata = result %>%
  count(BRAND, combo)

figdata = figdata %>%
  mutate(combo = factor(combo, levels = c(
    "Internet + AAE",
    "Internet + Unsure AAE",
    "Internet only",
    "AAE only",
    "Unsure AAE only"
  )))

ggplot(figdata, aes(x = BRAND, y = n, fill = combo)) +
  geom_col() +
  labs(x = NULL, y = "Feature Tokens", fill = NULL) +
  theme_classic() +
  scale_fill_viridis_d(end = 0.7) +
  scale_y_continuous(
    breaks = seq(0, 10, 2),
    expand = expansion(mult = c(0, 0.05))
  )
  ggsave("rankedbar_feature_tokens.png", width = 7, height = 5, dpi = 300)

  # summary table
  summary_table = result %>%
    group_by(BRAND) %>%
    summarise(
      any_feature = n_distinct(POST_ID),
      internet    = n_distinct(POST_ID[iccat == "Present"]),
      aae         = n_distinct(POST_ID[aaeccat == "Present"]),
      overlap     = n_distinct(POST_ID[combo == "Internet + AAE"])
    )
  
  totals = tibble(
    BRAND = c("Denny's", "Duolingo", "Wendy's"),
    total = c(32, 53, 17)
  )
  summary_table = summary_table %>%
    left_join(totals, by = "BRAND")
  
  summary_table = summary_table %>%
    mutate(across(c(any_feature, internet, aae, overlap),
                  ~ round(.x / total * 100, 1),
                  .names = "{.col}_pct"))
  
  #appendix A
  tinyResult = result %>%
    select("DATE", "BRAND", "MAIN_ID", "POST_ID", "iccat", "IC_SOURCE", "aaeccat", "AAEC_SOURCE", "overlapcat", "combo", "QUOTE", "Feature", "Notes")
  
  write_csv(tinyResult, "appendixA.csv")
  
  
  