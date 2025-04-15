# NUS Food science graduates

## Source: NUS FST website
## Created on 10 April 2025


library(tidyverse)
library(janitor)
library(ggthemr)
library(here)

conflicted::conflict_prefer("filter", "dplyr")

ggthemr("fresh")

# IMPORT -----
df <- 
  tibble::tribble(
                                 ~Sector, ~Percentage,
             "F&B, Flavor & Ingredients",         48L,
  "Health, Pharm, Food related business",          9L,
                              "Teaching",          9L,
                     "Government bodies",         13L,
                         "Further study",          9L,
                                "Others",         12L
  ) %>% 
  clean_names()

glimpse(df)


df %>% 
  ggplot(aes(x = fct_rev(fct_reorder(sector, percentage)), y = percentage)) +
  geom_col(alpha = 0.8) +
  geom_text(aes(label = percentage), vjust = 2, size = 4) +
  labs(
    title = "Average Employment Distribution of NUS FST Graduates",
    subtitle = "Majority of the graduates are in the F&B/Flavor/Ingredient sector.",
    x = "",
    y = "%",
    caption = "Source: https://www.fst.nus.edu.sg/education/undergraduate-programme/frequently-asked-questions/"
  ) +
  scale_y_continuous(expand = c(0,0), limits = c(0, 50)) +
  theme(plot.caption = element_text(face = "italic"))


path_out <- here("visualizations", "fst-emp-dist.jpg")
path_out

ggsave(path_out, dpi = 300)
