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

Chileka1<-microbiology |> 
  filter(farm == "Chileka1") 

# Step 1: Calculate Mean Inhibition and Standard Error
summary_data <- Chileka1 |> 
  group_by(item) |>  # Group by sample type (manure, soil, vegetables)
  summarise(
    mean_inhibition = mean(inhibition, na.rm = TRUE), # Calculate mean inhibition
    se_inhibition = sd(inhibition, na.rm = TRUE) / sqrt(n()), # Calculate standard error
    .groups = 'drop' # Drop grouping after summarization
  )

# View the summary data
print(summary_data)

# Step 4: Create Line Graph with ggplot2
line_plot <- ggplot(summary_data, aes(x = item, y = mean_inhibition, group = 1)) +
  geom_line() +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_inhibition - se_inhibition, ymax = mean_inhibition + se_inhibition), width = 0.2) +
  geom_text(aes(label = round(mean_inhibition, 2)), vjust = -1, size = 5) + # Add mean values as text labels
  labs(title = "Microbial Inhibition Across Sample Types (Farm 4)",
       x = "Sample Type (Manure, Soil, Vegetables)",
       y = "Mean Inhibition") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 14), # Increase font size for x-axis title
    axis.title.y = element_text(size = 14), # Increase font size for y-axis title
    axis.text.x = element_text(size = 12, angle = 10, hjust = 1),   # Increase font size for x-axis categories (tick labels)
    axis.text.y = element_text(size = 12)     # Increase font size for y-axis categories (tick labels)
  )+
  my_theme

# Display the plot
print(line_plot)


#I want to do anova 
# Two way ANOVA
model <- aov(inhibition ~ antibiotic * item, data = Chileka1)
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


anova_table |> gtsave("chieka1-anova.docx")


# Step 2: Run Tukey's HSD Test
tukey_result <- TukeyHSD(model)


result_table <- as_tibble(tukey_result$`antibiotic`, rownames = "Comparison")

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

chileka1_t<-final %>%
  mutate(Adjusted_p_value = sapply(Adjusted_p_value, format_p_value)) |> 
  gt() %>%
  tab_header(
    title = "Significant Differences"
  ) |> 
  tab_footnote(
    footnote = "Significance codes: *** p < 0.001; ** p < 0.01; * p < 0.05", 
    locations = cells_column_labels(columns = Adjusted_p_value)  # Specify where to place the footnote
  )

chileka1_t |> gtsave('chileka1_t.docx')



