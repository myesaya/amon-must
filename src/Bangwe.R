library (tidyverse)
library(tidyverse)
library(janitor)
library(tidyquant)
library(patchwork)
library(survival)
library(survminer)
library(gtsummary)
library (tidyverse)
library(gtsummary)
library(lfactors)
library(flextable)
library(dplyr)
library(gt)
library(broom)


# import data -------------------------------------------------------------


microbiology<- read_csv(here::here("data/raw/sorted.csv")) |> 
  clean_names() |> 
  mutate_if(is.character, as_factor) |> 
  select(id:microbe)




#I will start by assessing statistical significnce on the presence of different microbes

Bangwe<-microbiology |> 
  filter(farm == "Bangwe") 
#I want to do anova 
# Two way ANOVA
model <- aov(inhibition ~ antibiotic * item, data = Bangwe)
#tidy anova results
anova_results <- tidy(model)

format_p_value <- function(p) {
  if (is.na(p)) {
    return("NA")  # Handle NA values gracefully
  } else if (p < 0.001) {
    return("< 0.001 ***")
  } else if (p < 0.01) {
    return(paste0(round(p, 3), " **"))
  } else if (p < 0.05) {
    return(paste0(round(p, 3), " *"))
  } else {
    return(round(p, 3))
  }
}

anova_table <- anova_results %>%
  mutate(p.value = sapply(p.value, format_p_value)) %>%  # Format p-values
  gt() %>%
  tab_header(
    title = "Test Results"
  ) %>%
  cols_label(
    term = "Source of Variation",
    df = "Degrees of Freedom",
    statistic = "F-Statistic",
    p.value = "P-Value"
  ) %>%
  tab_footnote(
    footnote = "Significance codes: *** p < 0.001; ** p < 0.01; * p < 0.05",
    locations = cells_column_labels(columns = p.value)  # Specify where to place the footnote
  )

# Display the table
print(anova_table)


anova_table |> gtsave("anova.docx")


# Step 2: Run Tukey's HSD Test
tukey_result <- TukeyHSD(model)


result_table <- as_tibble(tukey_result$`antibiotic:item`, rownames = "Comparison")

# Step 2: Rename columns for clarity
result_table <- result_table %>%
  rename(
    Difference = diff,
    Lower_CI = lwr,
    Upper_CI = upr,
    Adjusted_p_value = `p adj`
  )

final<-result_table |> 
  filter(Adjusted_p_value<0.05) 

final %>%
  mutate(Adjusted_p_value = sapply(Adjusted_p_value, format_p_value)) |> 
  gt() %>%
  tab_header(
    title = "Significant Differences"
  ) |> 
  tab_footnote(
    footnote = "Significance codes: *** p < 0.001; ** p < 0.01; * p < 0.05", 
    locations = cells_column_labels(columns = Adjusted_p_value)  # Specify where to place the footnote
  )



