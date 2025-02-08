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
  mutate_if(is.character, as_factor) 

#re code factors to new names
  
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
                                  "farm 4",
                                  "farm 5")))  

# Assuming your data is stored in a data frame called `microbiology`
 microbiology %>%
  group_by(farm, item, microbe) %>% 
  summarize(count = n(), .groups = "drop") %>% 
  mutate(label = paste(microbe, "(", count, ")", sep = "")) %>% 
  group_by(farm, item) %>% 
  summarize(microbes = paste(label, collapse = ", "), .groups = "drop") %>% 
  pivot_wider(
    names_from = farm, 
    values_from = microbes, 
    values_fill = "" 
  ) %>%
  gt(rowname_col = "item") %>% 
  tab_header(
    title = "Microbes by Farm and Sample"
  ) %>%
  cols_label(
    item = md("**Item**") # Bold the column header "Item"
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"), # Bold the row names under the "Item" column
    locations = cells_stub(rows = everything()) 
  ) 
farm_sample |> gtsave("Microbes by Farm and Sample.docx")





library(dplyr)

microbiology %>%
  group_by(item, sensitivity) %>%
  summarise(
    microbe_count = n(), .groups = 'drop')


  


#change order of levels


#test for differences

# Create the table using gt
gt_table <- summary_data %>%
  gt() %>%
  cols_label(
    item = "Item",
    n_sd = "N (SD)",
    antibiotic = "Antibiotic",
    Resistant = "Resistant",
    Intermediate = "Intermediate",
    Sensitive = "Sensitive"
  ) %>%
  tab_spanner(
    label = "Sensitivity",
    columns = c("Resistant", "Intermediate", "Sensitive")
  ) %>%
  tab_header(
    title = "Antibiotic Sensitivity Summary"
  )%>%
  cols_align(
    align = "left",  # Left-align the column
    columns = c(item)  # Specify the first column
  )

# Display the table
gt_table

















# Load necessary library
library(dplyr)
library(stringr)

# Grouping and summarizing data by item
summary_data <- microbiology %>%
  separate_rows(antibiotic, sep = ",") %>%
  group_by(item) %>%
  summarise(
    antibiotics = str_c(
      unique(antibiotic), collapse = ", "),  # Combine antibiotics into a single string
    n = n(),
    SD = sd(as.numeric(sensitivity == "Resistant")), # For example: 1 for Resistant, 0 for others
    Resistant = sum(sensitivity == "Resistant"),
    Intermediate = sum(sensitivity == "Intermediate"),
    Sensitive = sum(sensitivity == "Sensitive"),
    .groups = 'drop'
  ) %>%
  mutate(
    n_sd = paste(n, "(", round(SD, 2), ")", sep = "")
  )

# View the summarized data
print(summary_data)

# Create the table using gt
gt_table <- summary_data %>%
  gt() %>%
  cols_label(
    item = "Item",
    antibiotics = "Antibiotics",  # Label for the new combined antibiotics column
    Resistant = "R",
    Intermediate = "I",
    Sensitive = "S"
  ) %>%
  tab_spanner(
    label = "Sensitivity",
    columns = c("R", "I", "S")
  ) %>%
  tab_header(
    title = "Antibiotic Sensitivity Summary"
  )

# Load necessary library
library(dplyr)
library(stringr)

# Grouping and summarizing data by item
summary_data <- microbiology %>%
  separate_rows(antibiotic, sep = ",") %>%
  group_by(item) %>%
  summarise(
    antibiotics = str_c(unique(antibiotic), collapse = ", "),  # Combine antibiotics into a single string
    n = n(),
    SD = sd(as.numeric(sensitivity == "R")), # For example: 1 for Resistant, 0 for others
    Resistant = sum(sensitivity == "R"),
    Intermediate = sum(sensitivity == "I"),
    Sensitive = sum(sensitivity == "S"),
    .groups = 'drop'
  ) %>%
  mutate(
    SD = paste(n, "(", round(SD, 2), ")", sep = "")
  )

# View the summarized data
print(summary_data)

# Create the table using gt
gt_table <- summary_data %>%
  gt() %>%
  cols_label(
    item = "Item",
    antibiotics = "Antibiotics",  # Label for the new combined antibiotics column
    Resistant = "Resistant",
    Intermediate = "Intermediate",
    Sensitive = "Sensitive"
  ) %>%
  tab_spanner(
    label = "Sensitivity",
    columns = c("Resistant", "Intermediate", "Sensitive")
  ) %>%
  tab_header(
    title = "Antibiotic Sensitivity Summary"
  )%>%
  cols_align(
    align = "left",  # Left-align the column
    columns = c(item)  # Specify the first column
  )

# Display the table
gt_table

