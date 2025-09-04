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
microbiology<-microbiology |> 
  mutate(
    farm=fct_recode(farm,
                    "farm 1"= "Bangwe",
                    "farm 2"="Chigumula",
                    "farm 3" = "Mpemba",
                    "farm 4" = "Chileka1",
                    "farm 5" = "Chileka2"
    )
  ) |> 
  mutate(farm=factor(farm, 
                     levels = c("farm 1",
                                "farm 2",
                                "farm 3",
                                "farm 4")))


# Filter the microbiology dataset for specific microbes
filtered_data <- microbiology %>%
  filter(microbe %in% c("E. coli", "Klebsiella pneumoniae"))


grouped_counts <- filtered_data %>%
  group_by(farm, item, microbe) %>%
  summarize(count = n(), .groups = "drop")


# Step 1: Calculate Mean Inhibition and Standard Error
summary_data <- filtered_data |> 
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
  labs(title = "Mean Microbial Inhibition Across All Sample Types",
       x = "Sample Type (Manure, Soil, Vegetables)",
       y = "Mean Inhibition") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 16), # Increase font size for x-axis title
    axis.title.y = element_text(size = 16), # Increase font size for y-axis title
    axis.text.x = element_text(size = 14, angle = 10, hjust = 1,color = "black"),   # Increase font size for x-axis categories (tick labels)
    axis.text.y = element_text(size = 14,color = "black"),     # Increase font size for y-axis categories (tick labels)
    plot.title = element_text(face = "bold", size = 16)  )


# Display the plot
print(line_plot)


#I want to do anova 
# Two way ANOVA
model <- aov(count ~ farm, data = grouped_counts)
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


anova_table |> gtsave("nrrowcombinedanova3.docx")


# Step 2: Run Tukey's HSD Test
tukey_result <- TukeyHSD(model)


result_table <- as_tibble(tukey_result$`farm:item`, rownames = "Comparison")

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

combined_t<-final %>%
  mutate(Adjusted_p_value = sapply(Adjusted_p_value, format_p_value)) |> 
  gt() %>%
  tab_header(
    title = "Significant Differences"
  ) |> 
  tab_footnote(
    footnote = "Significance codes: *** p < 0.001; ** p < 0.01; * p < 0.05", 
    locations = cells_column_labels(columns = Adjusted_p_value)  # Specify where to place the footnote
  )

print (combined_t)
combined_t |> gtsave("combined-tukey.docx")

# Vegetable readings across all farms -------------------------------------

#Sample vegetable
#strands locations
#utcome: inhibition

veg<- grouped_counts |> 
  filter(item=="Amaranthus") 

#I want to do anova 
# Two way ANOVA
model <- aov(count ~ farm, data = veg)
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

anova_table |> gtsave("amarathus accross farms.docx")


# Chicken Manure readings across all farms -------------------------------------

#Chicken manure
#strands locations
#outcome: inhibition

Chicken_manure<- grouped_counts |> 
  filter(item=="Chicken manure") 

#I want to do anova 
# Two way ANOVA
model <- aov(count ~ farm, data = Chicken_manure)
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

anova_table |> gtsave("chickenmanure.docx")

# Home Soil readings across all farms -------------------------------------

#Home Soil
#strands locations
#outcome: inhibition

levels(microbiology$item)

Home_soil<- grouped_counts |> 
  filter(item=="Pig manure") 

#I want to do anova 
# Two way ANOVA
model <- aov(count ~ farm, data = Home_soil)
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

home_soil <- anova_results %>%
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
print(home_soil)


home_soil |> gtsave ("pigmanure.docx")



# tURkey ------------------------------------------------------------------

# Step 2: Run Tukey's HSD Test
tukey_result <- TukeyHSD(model)


result_table <- as_tibble(tukey_result$`farm`, rownames = "Comparison")

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

combined_t<-final %>%
  mutate(Adjusted_p_value = sapply(Adjusted_p_value, format_p_value)) |> 
  gt() %>%
  tab_header(
    title = "Significant Differences"
  ) |> 
  tab_footnote(
    footnote = "Significance codes: *** p < 0.001; ** p < 0.01; * p < 0.05", 
    locations = cells_column_labels(columns = Adjusted_p_value)  # Specify where to place the footnote
  )

print (combined_t)
combined_t |> gtsave("pig-tukey.docx")
















































# Overall readings across all farms -------------------------------------

#Home Soil
#strands locations
#outcome: inhibition


#I want to do anova 
# Two way ANOVA
model <- aov(inhibition ~ item*farm, data = microbiology)
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



# Load libraries
library(dplyr)
library(ggplot2)

# Step 1: Create the data frame

microbiology |> filter(microbe %in% c("E. coli", "Klebsiella pneumoniae")) |> 
  na.omit() |>
  ggplot( aes(x = antibiotic, fill = sensitivity)) +
  geom_bar(position = "dodge") +
  facet_wrap(~ microbe) +
  labs(title = "Antibiotic Effectiveness Againtst Microbes (E. coli & Klebsiella)",
       x = "Antibiotic",
       y = "Count",
       fill = "Sensitivity")+
  theme_minimal() +
  facet_wrap(~ item) +
  theme(
    axis.title.x = element_text(size = 13), # Increase font size for x-axis title
    axis.title.y = element_text(size = 13), # Increase font size for y-axis title
    axis.text.x = element_text(size = 10, angle = 10, hjust = 1,color = "black"),   # Increase font size for x-axis categories (tick labels)
    axis.text.y = element_text(size = 13,color = "black"),     # Increase font size for y-axis categories (tick labels)
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(size = 12, color = "black"))+
  scale_fill_discrete(
    labels = c("Resisitant", "Intermediate", "Sensitive") )# Rename legend elements

#let me show the resistance pattern

# Load necessary libraries
library(dplyr)
library(ggplot2)
library(pheatmap)

# Filter the microbiology dataset for specific microbes
filtered_data <- microbiology %>%
  filter(microbe %in% c("E. coli", "Klebsiella pneumoniae", "Salmonella choleraesuis",
                        "Pseudomonas aeruginosa", "Acinetobacter spp.", "Stenotrophomonas maltophilia",
                        "Yersinia pestis", "Pasteurella spp.", "Raoultella ornitholytica"))

# Summarizing resistance patterns for each microbe and antibiotic
summary_data <- filtered_data %>%
  group_by(microbe, antibiotic, sensitivity) %>%
  summarise(count = n(), .groups = 'drop')

# View summarized data
print(summary_data)

# Bar plot: Resistance pattern per drug for each microbe (separated by microbe)
ggplot(summary_data, aes(x = antibiotic, y = count, fill = sensitivity)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(title = "Resistance Pattern for Drugs (Separated by Microbe)",
       x = "Drug", y = "Resistance Cases") +
  facet_wrap(~ microbe, scales = "free_y") +  # Faceting by microbe for better readability
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 13), # Increase font size for x-axis title
    axis.title.y = element_text(size = 13), # Increase font size for y-axis title
    axis.text.x = element_text(size = 10, angle = 10, hjust = 1,color = "black"),   # Increase font size for x-axis categories (tick labels)
    axis.text.y = element_text(size = 13,color = "black"),     # Increase font size for y-axis categories (tick labels)
    plot.title = element_text(face = "bold", size = 16),
    strip.text = element_text(size = 12, color = "black"))+
  scale_fill_discrete(
    labels = c("Resisitant", "Intermediate", "Sensitive") )# Rename legend elements

#let me show the resistance pattern

# Proportion plot: Resistance proportions per drug for each microbe
ggplot(filtered_data, aes(x = antibiotic, fill = sensitivity)) +
  geom_bar(position = "fill") +
  labs(title = "Proportion of Resistance to Drugs by Microbe",
       x = "Drug", y = "Proportion") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  facet_wrap(~ microbe, scales = "free_y")  # Faceting by microbe

# Heatmap: Resistance patterns per microbe and antibiotic
heatmap_data <- filtered_data %>%
  group_by(microbe, antibiotic, sensitivity) %>%
  summarise(count = n(), .groups = 'drop') %>%
  spread(key = sensitivity, value = count, fill = 0)

# Convert the data to a matrix for the heatmap
heatmap_matrix <- as.matrix(heatmap_data[,-c(1, 2)])  # Remove microbe and antibiotic columns
rownames(heatmap_matrix) <- heatmap_data$microbe
colnames(heatmap_matrix) <- colnames(heatmap_data)[-c(1, 2)]

# Plot the heatmap
pheatmap(heatmap_matrix, main = "Resistance Heatmap (Separated by Microbe)",
         cluster_rows = TRUE, cluster_cols = TRUE)












# Load necessary libraries
library(dplyr)
library(gt)


# Step 1: Filter the dataset to keep only Resistant cases
resistant_data <- filtered_data %>%
  filter(sensitivity == "R") |> 
  select(microbe, antibiotic, sensitivity)

resistant_data<-resistant_data |> 
mutate(
  sensitivity=fct_recode(sensitivity,
                  "Resistant"= "R",
                 
  )
)

# Step 2: Display the filtered data in a GT table
resistant_data<-resistant_data %>%
  gt() %>%
  tab_header(
    title = "Resistance Patterns of Microbes to Drugs"
  ) %>%
  cols_label(
    microbe = "Microbe",
    antibiotic = "Drug",
    sensitivity = "Resistance Status"
  ) %>%
  tab_spanner(
    label = "Resistance Information",
    columns = vars(microbe, antibiotic, sensitivity)
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold", color = "white"),
      cell_fill(color = "red")
    ),
    locations = cells_body(columns = vars(sensitivity), rows = sensitivity == "Resistant")
  ) %>%
  tab_style(
    style = list(cell_fill(color = "lightgray")),
    locations = cells_body(columns = vars(sensitivity), rows = sensitivity != "Resistant")
  )

resistant_data 
gtsave("resistant_data.docx")



# Load necessary libraries
library(dplyr)
library(gt)


table<-microbiology |> 
  select(microbe, item) |> 
  tbl_summary(by = item) |> 
  as_flex_table() |>
  save_as_docx(path = "~/4418304/msc-extent-open-burning/summer-study/summer-data/mmmmm.docx")


#i want to save as gtsummary table


