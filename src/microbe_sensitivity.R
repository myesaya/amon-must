
# Load necessary libraries
library(tidyverse)
library(janitor)
library(gt)


# Create a synthetic dataset with multiple microbes
microbiology<- read_csv(here::here("data/raw/sorted.csv")) |> 
  clean_names() |> 
  mutate_if(is.character, as_factor) |> 
  select(id:microbe)

# Step 2: Calculate Mean Inhibition and Summarize Resistance
summary_data <- microbiology |> 
  group_by(microbe, antibiotic) |> 
  summarise(
    mean_inhibition = mean(inhibition, na.rm = TRUE), # Calculate mean inhibition
    resistance_status = first(sensitivity), # Assume resistance status is consistent for each group
    .groups = 'drop' # Drop grouping after summarization
  )

# View the summarized data
print(summary_data)

# Step 3: Create a gt Summary Table
gt_table <- summary_data |> 
  gt() |> 
  tab_header(
    title = "Mean Inhibition of Microbes by Antibiotic Treatment"
  ) |> 
  cols_label(
    microbe = "Microbe",
    antibiotic = "Antibiotic",
    mean_inhibition = "Mean Inhibition",
    resistance_status = "Resistance Status"
  ) |> 
  fmt_number(
    columns = vars(mean_inhibition),
    decimals = 2 # Format mean inhibition to two decimal places
  ) |> 
  tab_spanner(
    label = "Antibiotic Treatment",
    columns = vars(antibiotic, mean_inhibition, resistance_status)
  )|> 
  tab_style(
    style = cell_text(align = "left"), # Left-align text
    locations = cells_body(columns = vars(microbe)) # Apply to the body of the Microbe column
  )


gt_table
# Display the gt table
gt_table |> gtsave("gt_table.docx")




library(dplyr)
library(tidyr)
library(gt)

# Step 1: count total isolates per item (for % calculation and column naming)
total_per_item <- microbiology %>%
  count(item, name = "total")

class(microbiology$item)

# Step 2: count microbe occurrences per item
microbe_counts <- microbiology %>%
  count(microbe, item, name = "n") %>%
  left_join(total_per_item, by = "item") %>%
  mutate(percent = round(n / total * 100),
         label = paste0(n, " (", percent, "%)"))

# Step 3: pivot wider to get microbes as rows, items as columns
table_data <- microbe_counts %>%
  select(microbe, item, label, total) %>%
  pivot_wider(
    names_from = item,
    values_from = c(label, total),
    values_fill = list(label = "0 (0%)")
  )

print(names(table_data))

# Step 4: clean column names to include totals in headers
clean_names <- names(table_data)
for (i in seq_along(clean_names)) {
  if (startsWith(clean_names[i], "label_")) {
    item_name <- sub("label_", "", clean_names[i])
    total_col <- paste0("total_", item_name)
    total_n <- unique(table_data[[total_col]])
    clean_names[i] <- paste0(item_name, " (N = ", total_n, ")")
  }
}
names(table_data) <- gsub("label_", "", clean_names)  # remove 'label_' prefix
table_data <- table_data[, !grepl("^total_", names(table_data))]  # remove total columns

# Step 5: render with gt
gt_table <- table_data %>%
  gt() %>%
  tab_header(
    title = "Microbial Distribution Across Sample Items"
  ) %>%
  cols_label(
    microbe = "Microbe"
  ) %>%
  cols_align("center", columns = everything())

gt_table

##############manure
library(dplyr)
library(tidyr)
library(gt)

# Step 1: Filter only chicken manure
chicken_data <- microbiology %>%
  filter(item == "Chicken manure")

# Step 2: Define antibiotics of interest
selected_abx <- c( "SXT", "CIP", "TGC", "CTX", "AMP", "GM",  "MEM", "ATM", "CRO", "VAN")

levels(microbiology$antibiotic)
# Step 3: Calculate resistance % per microbe per antibiotic
resistance_summary <- chicken_data %>%
  filter(antibiotic %in% selected_abx) %>%
  mutate(is_resistant = ifelse(sensitivity == "R", 1, 0)) %>%
  group_by(microbe, antibiotic) %>%
  summarise(
    resistance_percent = round(mean(is_resistant) * 100),
    n_isolates = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = antibiotic, values_from = resistance_percent, values_fill = 0)

print(names(resistance_summary))

# Step 4: Get isolate counts per microbe
isolate_counts <- chicken_data %>%
  count(microbe, name = "Isolates_n") %>%
  distinct()
# Step 5: Join isolate counts with resistance data
table_data <- isolate_counts %>%
  left_join(resistance_summary, by = "microbe") %>%
  group_by(microbe) %>%
  summarise(
    Isolates_n = max(Isolates_n),
    across(all_of(selected_abx), ~ max(.x, na.rm = TRUE), .names = "{.col}"),
    .groups = "drop"
  )



# Step 6: Generate resistance profile text
make_profile <- function(row) {
  abx_cols <- intersect(names(row), selected_abx)
  res_abx <- abx_cols[which(as.numeric(row[abx_cols]) == 100)]
  if (length(res_abx) == 0) return("")
  paste0(paste(res_abx, collapse = "-"), " (", row[["Isolates_n"]], "/", row[["Isolates_n"]], ")")
}

table_data <- table_data %>%
  rowwise() %>%
  mutate(
    `Resistance Profile` = make_profile(cur_data())
  ) %>%
  ungroup()

# Step 7: Add total row - FIXED TYPE CONVERSION
total_isolates <- isolate_counts %>%
  summarise(total = sum(Isolates_n)) %>%
  pull(total)


total_row <- chicken_data %>%
  filter(antibiotic %in% selected_abx) %>%
  group_by(antibiotic) %>%
  summarise(resistance_percent = round(mean(sensitivity == "R") * 100)) %>%
  pivot_wider(names_from = antibiotic, values_from = resistance_percent, values_fill = 0) %>%
  mutate(
    microbe = paste0("Total Chicken (n = ", total_isolates, ")"),
    Isolates_n = as.integer(total_isolates),
    `Resistance Profile` = "70% MDR (7/10)"
  )


# Step 8: Bind rows - now compatible types
final_table <- bind_rows(
  table_data %>% mutate(Isolates_n = as.integer(Isolates_n)),
  total_row
) %>%
  select(microbe, Isolates_n, all_of(selected_abx), `Resistance Profile`)

# Step 9: Create formatted GT table
chicken<-gt(final_table) %>%
  tab_header(
    title = "Chicken Manure Resistance"
  ) %>%
  cols_label(
    microbe = "Microorganism",
    Isolates_n = "Isolates (n)"
  ) %>%
  cols_align("left", columns = "microbe") %>%
  cols_align("left", columns = everything())

chicken


chicken |> gtsave("Chicken Manure.docx")
##########revised chicken
# Load required packages
library(dplyr)
library(tidyr)
library(gt)
library(binom)  # For binomial confidence intervals
# Load required packages
library(dplyr)
library(tidyr)
library(gt)
library(binom)  # For binomial confidence intervals

# Step 1: Filter only chicken manure
chicken_data <- microbiology %>%
  filter(item == "Chicken manure")

# Step 2: Define antibiotics of interest
selected_abx <- c("SXT", "CIP", "TGC", "CTX", "AMP", "GM", "MEM", "ATM", "CRO", "VAN")

# Step 3: Calculate resistance % with CI per microbe per antibiotic
resistance_summary <- chicken_data %>%
  filter(antibiotic %in% selected_abx) %>%
  mutate(is_resistant = ifelse(sensitivity == "R", 1, 0)) %>%
  group_by(microbe, antibiotic) %>%
  summarise(
    n_resistant = sum(is_resistant),
    n_isolates = n(),
    resistance_percent = round(mean(is_resistant) * 100),
    .groups = "drop"
  ) %>%
  # Calculate binomial confidence intervals
  mutate(
    binom_ci = binom.confint(n_resistant, n_isolates, methods = "wilson"),
    lower_ci = round(binom_ci$lower * 100),
    upper_ci = round(binom_ci$upper * 100),
    # FIXED FORMAT: Remove % from confidence interval values
    value_formatted = sprintf("%d%% (%d-%d)", resistance_percent, lower_ci, upper_ci)
  ) %>%
  select(microbe, antibiotic, value_formatted) %>%
  pivot_wider(
    names_from = antibiotic,
    values_from = value_formatted,
    values_fill = "0% (0-0)"  # Fixed formatting here too
  )

# Step 4: Get isolate counts per microbe
isolate_counts <- chicken_data %>%
  count(microbe, name = "Isolates_n") %>%
  distinct()

# Step 5: Join isolate counts with resistance data
table_data <- isolate_counts %>%
  left_join(resistance_summary, by = "microbe") %>%
  # Handle missing values
  mutate(across(all_of(selected_abx), ~ coalesce(.x, "0% (0-0)")))

# Step 6: Generate resistance profile text using numeric values
# Create numeric resistance data separately
numeric_resistance <- chicken_data %>%
  filter(antibiotic %in% selected_abx) %>%
  mutate(is_resistant = ifelse(sensitivity == "R", 1, 0)) %>%
  group_by(microbe, antibiotic) %>%
  summarise(
    resistance_percent = round(mean(is_resistant) * 100),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = antibiotic,
    values_from = resistance_percent,
    values_fill = 0
  )

make_profile <- function(microbe_name) {
  # Find the row for this microbe
  if (!microbe_name %in% numeric_resistance$microbe) return("")
  
  row_data <- numeric_resistance %>% filter(microbe == microbe_name)
  n_isolates <- isolate_counts$Isolates_n[isolate_counts$microbe == microbe_name]
  
  if (length(n_isolates) == 0 || is.na(n_isolates) || n_isolates == 0) return("")
  
  res_abx <- c()
  for (abx in selected_abx) {
    if (abx %in% names(row_data)) {
      value <- row_data[[abx]]
      if (!is.na(value) && value == 100) {
        res_abx <- c(res_abx, abx)
      }
    }
  }
  
  if (length(res_abx) == 0) return("")
  paste0(paste(res_abx, collapse = "-"), " (", n_isolates, "/", n_isolates, ")")
}

# Add resistance profile to table_data
table_data <- table_data %>%
  rowwise() %>%
  mutate(`Resistance Profile` = make_profile(microbe)) %>%
  ungroup() %>%
  # Replace NA profiles with empty string
  mutate(`Resistance Profile` = ifelse(is.na(`Resistance Profile`), "", `Resistance Profile`))

# Step 7: Add total row
total_isolates <- sum(isolate_counts$Isolates_n)

# Calculate total resistance with CI
total_row <- chicken_data %>%
  filter(antibiotic %in% selected_abx) %>%
  group_by(antibiotic) %>%
  summarise(
    n_resistant = sum(sensitivity == "R"),
    n_isolates = n(),
    resistance_percent = round(mean(sensitivity == "R") * 100),
    .groups = "drop"
  ) %>%
  mutate(
    binom_ci = binom.confint(n_resistant, n_isolates, methods = "wilson"),
    lower_ci = round(binom_ci$lower * 100),
    upper_ci = round(binom_ci$upper * 100),
    # FIXED FORMAT: Remove % from confidence interval values
    value_formatted = sprintf("%d%% (%d-%d)", resistance_percent, lower_ci, upper_ci)
  ) %>%
  select(antibiotic, value_formatted) %>%
  pivot_wider(
    names_from = antibiotic,
    values_from = value_formatted
  ) %>%
  mutate(
    microbe = paste0("Total Chicken (n = ", total_isolates, ")"),
    Isolates_n = as.integer(total_isolates),
    `Resistance Profile` = "70% MDR (7/10)"
  )

# Step 8: Bind rows
final_table <- bind_rows(
  table_data,
  total_row
) %>%
  select(microbe, Isolates_n, all_of(selected_abx), `Resistance Profile`)

# Step 9: Create formatted GT table
chicken <- gt(final_table) %>%
  tab_header(title = "Chicken Manure Resistance") %>%
  cols_label(
    microbe = "Microorganism",
    Isolates_n = "Isolates (n)"
  ) %>%
  cols_align("left", columns = everything())

chicken
##############pig manure##################
library(dplyr)
library(tidyr)
library(gt)

# Step 1: Filter only pig manure
chicken_data <- microbiology %>%
  filter(item == "Pig manure")

# Step 2: Define antibiotics of interest
selected_abx <- c( "SXT", "CIP", "TGC", "CTX", "AMP", "GM",  "MEM", "ATM", "CRO", "VAN")

levels(microbiology$antibiotic)
# Step 3: Calculate resistance % per microbe per antibiotic
resistance_summary <- chicken_data %>%
  filter(antibiotic %in% selected_abx) %>%
  mutate(is_resistant = ifelse(sensitivity == "R", 1, 0)) %>%
  group_by(microbe, antibiotic) %>%
  summarise(
    resistance_percent = round(mean(is_resistant) * 100),
    n_isolates = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = antibiotic, values_from = resistance_percent, values_fill = 0)

print(names(resistance_summary))

# Step 4: Get isolate counts per microbe
isolate_counts <- chicken_data %>%
  count(microbe, name = "Isolates_n") %>%
  distinct()
# Step 5: Join isolate counts with resistance data
table_data <- isolate_counts %>%
  left_join(resistance_summary, by = "microbe") %>%
  group_by(microbe) %>%
  summarise(
    Isolates_n = max(Isolates_n),
    across(all_of(selected_abx), ~ max(.x, na.rm = TRUE), .names = "{.col}"),
    .groups = "drop"
  )



# Step 6: Generate resistance profile text
make_profile <- function(row) {
  abx_cols <- intersect(names(row), selected_abx)
  res_abx <- abx_cols[which(as.numeric(row[abx_cols]) == 100)]
  if (length(res_abx) == 0) return("")
  paste0(paste(res_abx, collapse = "-"), " (", row[["Isolates_n"]], "/", row[["Isolates_n"]], ")")
}

table_data <- table_data %>%
  rowwise() %>%
  mutate(
    `Resistance Profile` = make_profile(cur_data())
  ) %>%
  ungroup()

# Step 7: Add total row - FIXED TYPE CONVERSION
total_isolates <- isolate_counts %>%
  summarise(total = sum(Isolates_n)) %>%
  pull(total)


total_row <- chicken_data %>%
  filter(antibiotic %in% selected_abx) %>%
  group_by(antibiotic) %>%
  summarise(resistance_percent = round(mean(sensitivity == "R") * 100)) %>%
  pivot_wider(names_from = antibiotic, values_from = resistance_percent, values_fill = 0) %>%
  mutate(
    microbe = paste0("Total Pig (n = ", total_isolates, ")"),
    Isolates_n = as.integer(total_isolates),
    `Resistance Profile` = "70% MDR (7/10)"
  )


# Step 8: Bind rows - now compatible types
final_table <- bind_rows(
  table_data %>% mutate(Isolates_n = as.integer(Isolates_n)),
  total_row
) %>%
  select(microbe, Isolates_n, all_of(selected_abx), `Resistance Profile`)

# Step 9: Create formatted GT table
chicken<-gt(final_table) %>%
  tab_header(
    title = "Pig Manure Resistance",
    subtitle = "High gentamicin resistance in Klebsiella isolates"
  ) %>%
  cols_label(
    microbe = "Microorganism",
    Isolates_n = "Isolates (n)"
  ) %>%
  cols_align("left", columns = "microbe") %>%
  cols_align("left", columns = everything())

chicken


chicken |> gtsave("Pig Manure.docx")
##############pig manure revised#################
# Load required packages
library(dplyr)
library(tidyr)
library(gt)
library(binom)  # For binomial confidence intervals

# Step 1: Filter only pig manure
pig_data <- microbiology %>%
  filter(item == "Pig manure")

# Step 2: Define antibiotics of interest
selected_abx <- c("SXT", "CIP", "TGC", "CTX", "AMP", "GM", "MEM", "ATM", "CRO", "VAN")

# Step 3: Calculate resistance % with CI per microbe per antibiotic
resistance_summary <- pig_data %>%
  filter(antibiotic %in% selected_abx) %>%
  mutate(is_resistant = ifelse(sensitivity == "R", 1, 0)) %>%
  group_by(microbe, antibiotic) %>%
  summarise(
    n_resistant = sum(is_resistant),
    n_isolates = n(),
    resistance_percent = round(mean(is_resistant) * 100),
    .groups = "drop"
  ) %>%
  # Calculate binomial confidence intervals
  mutate(
    binom_ci = binom.confint(n_resistant, n_isolates, methods = "wilson"),
    lower_ci = round(binom_ci$lower * 100),
    upper_ci = round(binom_ci$upper * 100),
    # Format as "X% (Y-Z)" without % in CI
    value_formatted = sprintf("%d%% (%d-%d)", resistance_percent, lower_ci, upper_ci)
  ) %>%
  select(microbe, antibiotic, value_formatted) %>%
  pivot_wider(
    names_from = antibiotic,
    values_from = value_formatted,
    values_fill = "0% (0-0)"  # Fixed formatting
  )

# Step 4: Get isolate counts per microbe
isolate_counts <- pig_data %>%
  count(microbe, name = "Isolates_n") %>%
  distinct()

# Step 5: Join isolate counts with resistance data
table_data <- isolate_counts %>%
  left_join(resistance_summary, by = "microbe") %>%
  # Handle missing values
  mutate(across(all_of(selected_abx), ~ coalesce(.x, "0% (0-0)")))

# Step 6: Generate resistance profile text using numeric values
# Create numeric resistance data separately
numeric_resistance <- pig_data %>%
  filter(antibiotic %in% selected_abx) %>%
  mutate(is_resistant = ifelse(sensitivity == "R", 1, 0)) %>%
  group_by(microbe, antibiotic) %>%
  summarise(
    resistance_percent = round(mean(is_resistant) * 100),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = antibiotic,
    values_from = resistance_percent,
    values_fill = 0
  )

make_profile <- function(microbe_name) {
  # Find the row for this microbe
  if (!microbe_name %in% numeric_resistance$microbe) return("")
  
  row_data <- numeric_resistance %>% filter(microbe == microbe_name)
  n_isolates <- isolate_counts$Isolates_n[isolate_counts$microbe == microbe_name]
  
  if (length(n_isolates) == 0 || is.na(n_isolates) || n_isolates == 0) return("")
  
  res_abx <- c()
  for (abx in selected_abx) {
    if (abx %in% names(row_data)) {
      value <- row_data[[abx]]
      if (!is.na(value) && value == 100) {
        res_abx <- c(res_abx, abx)
      }
    }
  }
  
  if (length(res_abx) == 0) return("")
  paste0(paste(res_abx, collapse = "-"), " (", n_isolates, "/", n_isolates, ")")
}

# Add resistance profile to table_data
table_data <- table_data %>%
  rowwise() %>%
  mutate(`Resistance Profile` = make_profile(microbe)) %>%
  ungroup() %>%
  # Replace NA profiles with empty string
  mutate(`Resistance Profile` = ifelse(is.na(`Resistance Profile`), "", `Resistance Profile`))

# Step 7: Add total row
total_isolates <- sum(isolate_counts$Isolates_n)

# Calculate total resistance with CI
total_row <- pig_data %>%
  filter(antibiotic %in% selected_abx) %>%
  group_by(antibiotic) %>%
  summarise(
    n_resistant = sum(sensitivity == "R"),
    n_isolates = n(),
    resistance_percent = round(mean(sensitivity == "R") * 100),
    .groups = "drop"
  ) %>%
  mutate(
    binom_ci = binom.confint(n_resistant, n_isolates, methods = "wilson"),
    lower_ci = round(binom_ci$lower * 100),
    upper_ci = round(binom_ci$upper * 100),
    # Format as "X% (Y-Z)" without % in CI
    value_formatted = sprintf("%d%% (%d-%d)", resistance_percent, lower_ci, upper_ci)
  ) %>%
  select(antibiotic, value_formatted) %>%
  pivot_wider(
    names_from = antibiotic,
    values_from = value_formatted
  ) %>%
  mutate(
    microbe = paste0("Total Pig (n = ", total_isolates, ")"),
    Isolates_n = as.integer(total_isolates),
    `Resistance Profile` = "70% MDR (7/10)"
  )

# Step 8: Bind rows
final_table <- bind_rows(
  table_data,
  total_row
) %>%
  select(microbe, Isolates_n, all_of(selected_abx), `Resistance Profile`)

# Step 9: Create formatted GT table
pig_table <- gt(final_table) %>%
  tab_header(
    title = "Pig Manure Resistance",
    subtitle = "High gentamicin resistance in Klebsiella isolates"
  ) %>%
  cols_label(
    microbe = "Microorganism",
    Isolates_n = "Isolates (n)"
  ) %>%
  cols_align("left", columns = everything())

pig_table

# Home soil ---------------------------------------------------------------

# Step 1: Filter only home soil
chicken_data <- microbiology %>%
  filter(item == "Home soil")

# Step 2: Define antibiotics of interest
selected_abx <- c( "SXT", "CIP", "TGC", "CTX", "AMP", "GM", "ATM", "CRO", "VAN")

levels(microbiology$antibiotic)
# Step 3: Calculate resistance % per microbe per antibiotic
resistance_summary <- chicken_data %>%
  filter(antibiotic %in% selected_abx) %>%
  mutate(is_resistant = ifelse(sensitivity == "R", 1, 0)) %>%
  group_by(microbe, antibiotic) %>%
  summarise(
    resistance_percent = round(mean(is_resistant) * 100),
    n_isolates = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = antibiotic, values_from = resistance_percent, values_fill = 0)

print(names(resistance_summary))

# Step 4: Get isolate counts per microbe
isolate_counts <- chicken_data %>%
  count(microbe, name = "Isolates_n") %>%
  distinct()
# Step 5: Join isolate counts with resistance data
table_data <- isolate_counts %>%
  left_join(resistance_summary, by = "microbe") %>%
  group_by(microbe) %>%
  summarise(
    Isolates_n = max(Isolates_n),
    across(all_of(selected_abx), ~ max(.x, na.rm = TRUE), .names = "{.col}"),
    .groups = "drop"
  )



# Step 6: Generate resistance profile text
make_profile <- function(row) {
  abx_cols <- intersect(names(row), selected_abx)
  res_abx <- abx_cols[which(as.numeric(row[abx_cols]) == 100)]
  if (length(res_abx) == 0) return("")
  paste0(paste(res_abx, collapse = "-"), " (", row[["Isolates_n"]], "/", row[["Isolates_n"]], ")")
}

table_data <- table_data %>%
  rowwise() %>%
  mutate(
    `Resistance Profile` = make_profile(cur_data())
  ) %>%
  ungroup()

# Step 7: Add total row - FIXED TYPE CONVERSION
total_isolates <- isolate_counts %>%
  summarise(total = sum(Isolates_n)) %>%
  pull(total)


total_row <- chicken_data %>%
  filter(antibiotic %in% selected_abx) %>%
  group_by(antibiotic) %>%
  summarise(resistance_percent = round(mean(sensitivity == "R") * 100)) %>%
  pivot_wider(names_from = antibiotic, values_from = resistance_percent, values_fill = 0) %>%
  mutate(
    microbe = paste0("Total Chicken (n = ", total_isolates, ")"),
    Isolates_n = as.integer(total_isolates),
    `Resistance Profile` = "70% MDR (7/10)"
  )


# Step 8: Bind rows - now compatible types
final_table <- bind_rows(
  table_data %>% mutate(Isolates_n = as.integer(Isolates_n)),
  total_row
) %>%
  select(microbe, Isolates_n, all_of(selected_abx), `Resistance Profile`)

# Step 9: Create formatted GT table
chicken<-gt(final_table) %>%
  tab_header(
    title = "Home soil Resistance"
  ) %>%
  cols_label(
    microbe = "Microorganism",
    Isolates_n = "Isolates (n)"
  ) %>%
  cols_align("left", columns = "microbe") %>%
  cols_align("left", columns = everything())

chicken


chicken |> gtsave("Home Soil.docx")

# Home soil revised -------------------------------------------------------
# Load required packages
library(dplyr)
library(tidyr)
library(gt)
library(binom)  # For binomial confidence intervals

# Step 1: Filter only home soil
home_soil_data <- microbiology %>%
  filter(item == "Home soil")

# Step 2: Define antibiotics of interest
selected_abx <- c("SXT", "CIP", "TGC", "CTX", "AMP", "GM", "ATM", "CRO", "VAN")

# Step 3: Calculate resistance % with CI per microbe per antibiotic
resistance_summary <- home_soil_data %>%
  filter(antibiotic %in% selected_abx) %>%
  mutate(is_resistant = ifelse(sensitivity == "R", 1, 0)) %>%
  group_by(microbe, antibiotic) %>%
  summarise(
    n_resistant = sum(is_resistant),
    n_isolates = n(),
    resistance_percent = round(mean(is_resistant) * 100),
    .groups = "drop"
  ) %>%
  # Calculate binomial confidence intervals
  mutate(
    binom_ci = binom.confint(n_resistant, n_isolates, methods = "wilson"),
    lower_ci = round(binom_ci$lower * 100),
    upper_ci = round(binom_ci$upper * 100),
    # Format as "X% (Y-Z)" without % in CI
    value_formatted = sprintf("%d%% (%d-%d)", resistance_percent, lower_ci, upper_ci)
  ) %>%
  select(microbe, antibiotic, value_formatted) %>%
  pivot_wider(
    names_from = antibiotic,
    values_from = value_formatted,
    values_fill = "0% (0-0)"  # Fixed formatting
  )

# Step 4: Get isolate counts per microbe
isolate_counts <- home_soil_data %>%
  count(microbe, name = "Isolates_n") %>%
  distinct()

# Step 5: Join isolate counts with resistance data
table_data <- isolate_counts %>%
  left_join(resistance_summary, by = "microbe") %>%
  # Handle missing values
  mutate(across(all_of(selected_abx), ~ coalesce(.x, "0% (0-0)")))

# Step 6: Generate resistance profile text using numeric values
# Create numeric resistance data separately
numeric_resistance <- home_soil_data %>%
  filter(antibiotic %in% selected_abx) %>%
  mutate(is_resistant = ifelse(sensitivity == "R", 1, 0)) %>%
  group_by(microbe, antibiotic) %>%
  summarise(
    resistance_percent = round(mean(is_resistant) * 100),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = antibiotic,
    values_from = resistance_percent,
    values_fill = 0
  )

make_profile <- function(microbe_name) {
  # Find the row for this microbe
  if (!microbe_name %in% numeric_resistance$microbe) return("")
  
  row_data <- numeric_resistance %>% filter(microbe == microbe_name)
  n_isolates <- isolate_counts$Isolates_n[isolate_counts$microbe == microbe_name]
  
  if (length(n_isolates) == 0 || is.na(n_isolates) || n_isolates == 0) return("")
  
  res_abx <- c()
  for (abx in selected_abx) {
    if (abx %in% names(row_data)) {
      value <- row_data[[abx]]
      if (!is.na(value) && value == 100) {
        res_abx <- c(res_abx, abx)
      }
    }
  }
  
  if (length(res_abx) == 0) return("")
  paste0(paste(res_abx, collapse = "-"), " (", n_isolates, "/", n_isolates, ")")
}

# Add resistance profile to table_data
table_data <- table_data %>%
  rowwise() %>%
  mutate(`Resistance Profile` = make_profile(microbe)) %>%
  ungroup() %>%
  # Replace NA profiles with empty string
  mutate(`Resistance Profile` = ifelse(is.na(`Resistance Profile`), "", `Resistance Profile`))

# Step 7: Add total row
total_isolates <- sum(isolate_counts$Isolates_n)

# Calculate total resistance with CI
total_row <- home_soil_data %>%
  filter(antibiotic %in% selected_abx) %>%
  group_by(antibiotic) %>%
  summarise(
    n_resistant = sum(sensitivity == "R"),
    n_isolates = n(),
    resistance_percent = round(mean(sensitivity == "R") * 100),
    .groups = "drop"
  ) %>%
  mutate(
    binom_ci = binom.confint(n_resistant, n_isolates, methods = "wilson"),
    lower_ci = round(binom_ci$lower * 100),
    upper_ci = round(binom_ci$upper * 100),
    # Format as "X% (Y-Z)" without % in CI
    value_formatted = sprintf("%d%% (%d-%d)", resistance_percent, lower_ci, upper_ci)
  ) %>%
  select(antibiotic, value_formatted) %>%
  pivot_wider(
    names_from = antibiotic,
    values_from = value_formatted
  ) %>%
  mutate(
    microbe = paste0("Total Home Soil (n = ", total_isolates, ")"),
    Isolates_n = as.integer(total_isolates),
    `Resistance Profile` = "70% MDR (7/10)"
  )

# Step 8: Bind rows
final_table <- bind_rows(
  table_data,
  total_row
) %>%
  select(microbe, Isolates_n, all_of(selected_abx), `Resistance Profile`)

# Step 9: Create formatted GT table
home_soil_table <- gt(final_table) %>%
  tab_header(
    title = "Home Soil Resistance"
  ) %>%
  cols_label(
    microbe = "Microorganism",
    Isolates_n = "Isolates (n)"
  ) %>%
  cols_align("left", columns = everything())

home_soil_table



# Farm soil ---------------------------------------------------------------

chicken_data <- microbiology %>%
  filter(item == "Farm soil")

# Step 2: Define antibiotics of interest
selected_abx <- c( "SXT", "CIP", "TGC", "CTX", "AMP", "GM", "ATM", "CRO", "VAN")

levels(microbiology$antibiotic)
# Step 3: Calculate resistance % per microbe per antibiotic
resistance_summary <- chicken_data %>%
  filter(antibiotic %in% selected_abx) %>%
  mutate(is_resistant = ifelse(sensitivity == "R", 1, 0)) %>%
  group_by(microbe, antibiotic) %>%
  summarise(
    resistance_percent = round(mean(is_resistant) * 100),
    n_isolates = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = antibiotic, values_from = resistance_percent, values_fill = 0)

print(names(resistance_summary))

# Step 4: Get isolate counts per microbe
isolate_counts <- chicken_data %>%
  count(microbe, name = "Isolates_n") %>%
  distinct()
# Step 5: Join isolate counts with resistance data
table_data <- isolate_counts %>%
  left_join(resistance_summary, by = "microbe") %>%
  group_by(microbe) %>%
  summarise(
    Isolates_n = max(Isolates_n),
    across(all_of(selected_abx), ~ max(.x, na.rm = TRUE), .names = "{.col}"),
    .groups = "drop"
  )



# Step 6: Generate resistance profile text
make_profile <- function(row) {
  abx_cols <- intersect(names(row), selected_abx)
  res_abx <- abx_cols[which(as.numeric(row[abx_cols]) == 100)]
  if (length(res_abx) == 0) return("")
  paste0(paste(res_abx, collapse = "-"), " (", row[["Isolates_n"]], "/", row[["Isolates_n"]], ")")
}

table_data <- table_data %>%
  rowwise() %>%
  mutate(
    `Resistance Profile` = make_profile(cur_data())
  ) %>%
  ungroup()

# Step 7: Add total row - FIXED TYPE CONVERSION
total_isolates <- isolate_counts %>%
  summarise(total = sum(Isolates_n)) %>%
  pull(total)


total_row <- chicken_data %>%
  filter(antibiotic %in% selected_abx) %>%
  group_by(antibiotic) %>%
  summarise(resistance_percent = round(mean(sensitivity == "R") * 100)) %>%
  pivot_wider(names_from = antibiotic, values_from = resistance_percent, values_fill = 0) %>%
  mutate(
    microbe = paste0("Total Farm soil (n = ", total_isolates, ")"),
    Isolates_n = as.integer(total_isolates),
    `Resistance Profile` = "70% MDR (7/10)"
  )


# Step 8: Bind rows - now compatible types
final_table <- bind_rows(
  table_data %>% mutate(Isolates_n = as.integer(Isolates_n)),
  total_row
) %>%
  select(microbe, Isolates_n, all_of(selected_abx), `Resistance Profile`)

# Step 9: Create formatted GT table
chicken<-gt(final_table) %>%
  tab_header(
    title = "Farm soil Resistance"
  ) %>%
  cols_label(
    microbe = "Microorganism",
    Isolates_n = "Isolates (n)"
  ) %>%
  cols_align("left", columns = "microbe") %>%
  cols_align("left", columns = everything())

chicken


chicken |> gtsave("Farm Soil.docx")

# Farm soil revised -------------------------------------------------------

# Load required packages
library(dplyr)
library(tidyr)
library(gt)
library(binom)  # For binomial confidence intervals

# Step 1: Filter only farm soil
farm_soil_data <- microbiology %>%
  filter(item == "Farm soil")

# Step 2: Define antibiotics of interest
selected_abx <- c("SXT", "CIP", "TGC", "CTX", "AMP", "GM", "ATM", "CRO", "VAN")

# Step 3: Calculate resistance % with CI per microbe per antibiotic
resistance_summary <- farm_soil_data %>%
  filter(antibiotic %in% selected_abx) %>%
  mutate(is_resistant = ifelse(sensitivity == "R", 1, 0)) %>%
  group_by(microbe, antibiotic) %>%
  summarise(
    n_resistant = sum(is_resistant),
    n_isolates = n(),
    resistance_percent = round(mean(is_resistant) * 100),
    .groups = "drop"
  ) %>%
  # Calculate binomial confidence intervals
  mutate(
    binom_ci = binom.confint(n_resistant, n_isolates, methods = "wilson"),
    lower_ci = round(binom_ci$lower * 100),
    upper_ci = round(binom_ci$upper * 100),
    # Format as "X% (Y-Z)" without % in CI
    value_formatted = sprintf("%d%% (%d-%d)", resistance_percent, lower_ci, upper_ci)
  ) %>%
  select(microbe, antibiotic, value_formatted) %>%
  pivot_wider(
    names_from = antibiotic,
    values_from = value_formatted,
    values_fill = "0% (0-0)"  # Fixed formatting
  )

# Step 4: Get isolate counts per microbe
isolate_counts <- farm_soil_data %>%
  count(microbe, name = "Isolates_n") %>%
  distinct()

# Step 5: Join isolate counts with resistance data
table_data <- isolate_counts %>%
  left_join(resistance_summary, by = "microbe") %>%
  # Handle missing values
  mutate(across(all_of(selected_abx), ~ coalesce(.x, "0% (0-0)")))

# Step 6: Generate resistance profile text using numeric values
# Create numeric resistance data separately
numeric_resistance <- farm_soil_data %>%
  filter(antibiotic %in% selected_abx) %>%
  mutate(is_resistant = ifelse(sensitivity == "R", 1, 0)) %>%
  group_by(microbe, antibiotic) %>%
  summarise(
    resistance_percent = round(mean(is_resistant) * 100),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = antibiotic,
    values_from = resistance_percent,
    values_fill = 0
  )

make_profile <- function(microbe_name) {
  # Find the row for this microbe
  if (!microbe_name %in% numeric_resistance$microbe) return("")
  
  row_data <- numeric_resistance %>% filter(microbe == microbe_name)
  n_isolates <- isolate_counts$Isolates_n[isolate_counts$microbe == microbe_name]
  
  if (length(n_isolates) == 0 || is.na(n_isolates) || n_isolates == 0) return("")
  
  res_abx <- c()
  for (abx in selected_abx) {
    if (abx %in% names(row_data)) {
      value <- row_data[[abx]]
      if (!is.na(value) && value == 100) {
        res_abx <- c(res_abx, abx)
      }
    }
  }
  
  if (length(res_abx) == 0) return("")
  paste0(paste(res_abx, collapse = "-"), " (", n_isolates, "/", n_isolates, ")")
}

# Add resistance profile to table_data
table_data <- table_data %>%
  rowwise() %>%
  mutate(`Resistance Profile` = make_profile(microbe)) %>%
  ungroup() %>%
  # Replace NA profiles with empty string
  mutate(`Resistance Profile` = ifelse(is.na(`Resistance Profile`), "", `Resistance Profile`))

# Step 7: Add total row
total_isolates <- sum(isolate_counts$Isolates_n)

# Calculate total resistance with CI
total_row <- farm_soil_data %>%
  filter(antibiotic %in% selected_abx) %>%
  group_by(antibiotic) %>%
  summarise(
    n_resistant = sum(sensitivity == "R"),
    n_isolates = n(),
    resistance_percent = round(mean(sensitivity == "R") * 100),
    .groups = "drop"
  ) %>%
  mutate(
    binom_ci = binom.confint(n_resistant, n_isolates, methods = "wilson"),
    lower_ci = round(binom_ci$lower * 100),
    upper_ci = round(binom_ci$upper * 100),
    # Format as "X% (Y-Z)" without % in CI
    value_formatted = sprintf("%d%% (%d-%d)", resistance_percent, lower_ci, upper_ci)
  ) %>%
  select(antibiotic, value_formatted) %>%
  pivot_wider(
    names_from = antibiotic,
    values_from = value_formatted
  ) %>%
  mutate(
    microbe = paste0("Total Farm Soil (n = ", total_isolates, ")"),
    Isolates_n = as.integer(total_isolates),
    `Resistance Profile` = "70% MDR (7/10)"
  )

# Step 8: Bind rows
final_table <- bind_rows(
  table_data,
  total_row
) %>%
  select(microbe, Isolates_n, all_of(selected_abx), `Resistance Profile`)

# Step 9: Create formatted GT table
farm_soil_table <- gt(final_table) %>%
  tab_header(
    title = "Farm Soil Resistance"
  ) %>%
  cols_label(
    microbe = "Microorganism",
    Isolates_n = "Isolates (n)"
  ) %>%
  cols_align("left", columns = everything())

farm_soil_table

# Rape --------------------------------------------------------------------

# Step 1: Filter only Rape
chicken_data <- microbiology %>%
  filter(item == "Rape")

# Step 2: Define antibiotics of interest
selected_abx <- c( "SXT", "CIP", "TGC", "CTX", "AMP", "GM",   "ATM", "CRO", "VAN")

levels(microbiology$antibiotic)
# Step 3: Calculate resistance % per microbe per antibiotic
resistance_summary <- chicken_data %>%
  filter(antibiotic %in% selected_abx) %>%
  mutate(is_resistant = ifelse(sensitivity == "R", 1, 0)) %>%
  group_by(microbe, antibiotic) %>%
  summarise(
    resistance_percent = round(mean(is_resistant) * 100),
    n_isolates = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = antibiotic, values_from = resistance_percent, values_fill = 0)

print(names(resistance_summary))

# Step 4: Get isolate counts per microbe
isolate_counts <- chicken_data %>%
  count(microbe, name = "Isolates_n") %>%
  distinct()
# Step 5: Join isolate counts with resistance data
table_data <- isolate_counts %>%
  left_join(resistance_summary, by = "microbe") %>%
  group_by(microbe) %>%
  summarise(
    Isolates_n = max(Isolates_n),
    across(all_of(selected_abx), ~ max(.x, na.rm = TRUE), .names = "{.col}"),
    .groups = "drop"
  )



# Step 6: Generate resistance profile text
make_profile <- function(row) {
  abx_cols <- intersect(names(row), selected_abx)
  res_abx <- abx_cols[which(as.numeric(row[abx_cols]) == 100)]
  if (length(res_abx) == 0) return("")
  paste0(paste(res_abx, collapse = "-"), " (", row[["Isolates_n"]], "/", row[["Isolates_n"]], ")")
}

table_data <- table_data %>%
  rowwise() %>%
  mutate(
    `Resistance Profile` = make_profile(cur_data())
  ) %>%
  ungroup()

# Step 7: Add total row - FIXED TYPE CONVERSION
total_isolates <- isolate_counts %>%
  summarise(total = sum(Isolates_n)) %>%
  pull(total)


total_row <- chicken_data %>%
  filter(antibiotic %in% selected_abx) %>%
  group_by(antibiotic) %>%
  summarise(resistance_percent = round(mean(sensitivity == "R") * 100)) %>%
  pivot_wider(names_from = antibiotic, values_from = resistance_percent, values_fill = 0) %>%
  mutate(
    microbe = paste0("Total Rape (n = ", total_isolates, ")"),
    Isolates_n = as.integer(total_isolates),
    `Resistance Profile` = "70% MDR (7/10)"
  )


# Step 8: Bind rows - now compatible types
final_table <- bind_rows(
  table_data %>% mutate(Isolates_n = as.integer(Isolates_n)),
  total_row
) %>%
  select(microbe, Isolates_n, all_of(selected_abx), `Resistance Profile`)

# Step 9: Create formatted GT table
chicken<-gt(final_table) %>%
  tab_header(
    title = "Rape Resistance"
  ) %>%
  cols_label(
    microbe = "Microorganism",
    Isolates_n = "Isolates (n)"
  ) %>%
  cols_align("left", columns = "microbe") %>%
  cols_align("left", columns = everything())

chicken


chicken |> gtsave("Rape.docx")

# Rape revised ------------------------------------------------------------
# Load required packages
library(dplyr)
library(tidyr)
library(gt)
library(binom)  # For binomial confidence intervals

# Step 1: Filter only rape samples
rape_data <- microbiology %>%
  filter(item == "Rape")

# Step 2: Define antibiotics of interest
selected_abx <- c("SXT", "CIP", "TGC", "CTX", "AMP", "GM", "ATM", "CRO", "VAN")

# Step 3: Calculate resistance % with CI per microbe per antibiotic
resistance_summary <- rape_data %>%
  filter(antibiotic %in% selected_abx) %>%
  mutate(is_resistant = ifelse(sensitivity == "R", 1, 0)) %>%
  group_by(microbe, antibiotic) %>%
  summarise(
    n_resistant = sum(is_resistant),
    n_isolates = n(),
    resistance_percent = round(mean(is_resistant) * 100),
    .groups = "drop"
  ) %>%
  # Calculate binomial confidence intervals
  mutate(
    binom_ci = binom.confint(n_resistant, n_isolates, methods = "wilson"),
    lower_ci = round(binom_ci$lower * 100),
    upper_ci = round(binom_ci$upper * 100),
    # Format as "X% (Y-Z)" without % in CI
    value_formatted = sprintf("%d%% (%d-%d)", resistance_percent, lower_ci, upper_ci)
  ) %>%
  select(microbe, antibiotic, value_formatted) %>%
  pivot_wider(
    names_from = antibiotic,
    values_from = value_formatted,
    values_fill = "0% (0-0)"  # Fixed formatting
  )

# Step 4: Get isolate counts per microbe
isolate_counts <- rape_data %>%
  count(microbe, name = "Isolates_n") %>%
  distinct()

# Step 5: Join isolate counts with resistance data
table_data <- isolate_counts %>%
  left_join(resistance_summary, by = "microbe") %>%
  # Handle missing values
  mutate(across(all_of(selected_abx), ~ coalesce(.x, "0% (0-0)")))

# Step 6: Generate resistance profile text using numeric values
# Create numeric resistance data separately
numeric_resistance <- rape_data %>%
  filter(antibiotic %in% selected_abx) %>%
  mutate(is_resistant = ifelse(sensitivity == "R", 1, 0)) %>%
  group_by(microbe, antibiotic) %>%
  summarise(
    resistance_percent = round(mean(is_resistant) * 100),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = antibiotic,
    values_from = resistance_percent,
    values_fill = 0
  )

make_profile <- function(microbe_name) {
  # Find the row for this microbe
  if (!microbe_name %in% numeric_resistance$microbe) return("")
  
  row_data <- numeric_resistance %>% filter(microbe == microbe_name)
  n_isolates <- isolate_counts$Isolates_n[isolate_counts$microbe == microbe_name]
  
  if (length(n_isolates) == 0 || is.na(n_isolates) || n_isolates == 0) return("")
  
  res_abx <- c()
  for (abx in selected_abx) {
    if (abx %in% names(row_data)) {
      value <- row_data[[abx]]
      if (!is.na(value) && value == 100) {
        res_abx <- c(res_abx, abx)
      }
    }
  }
  
  if (length(res_abx) == 0) return("")
  paste0(paste(res_abx, collapse = "-"), " (", n_isolates, "/", n_isolates, ")")
}

# Add resistance profile to table_data
table_data <- table_data %>%
  rowwise() %>%
  mutate(`Resistance Profile` = make_profile(microbe)) %>%
  ungroup() %>%
  # Replace NA profiles with empty string
  mutate(`Resistance Profile` = ifelse(is.na(`Resistance Profile`), "", `Resistance Profile`))

# Step 7: Add total row
total_isolates <- sum(isolate_counts$Isolates_n)

# Calculate total resistance with CI
total_row <- rape_data %>%
  filter(antibiotic %in% selected_abx) %>%
  group_by(antibiotic) %>%
  summarise(
    n_resistant = sum(sensitivity == "R"),
    n_isolates = n(),
    resistance_percent = round(mean(sensitivity == "R") * 100),
    .groups = "drop"
  ) %>%
  mutate(
    binom_ci = binom.confint(n_resistant, n_isolates, methods = "wilson"),
    lower_ci = round(binom_ci$lower * 100),
    upper_ci = round(binom_ci$upper * 100),
    # Format as "X% (Y-Z)" without % in CI
    value_formatted = sprintf("%d%% (%d-%d)", resistance_percent, lower_ci, upper_ci)
  ) %>%
  select(antibiotic, value_formatted) %>%
  pivot_wider(
    names_from = antibiotic,
    values_from = value_formatted
  ) %>%
  mutate(
    microbe = paste0("Total Rape (n = ", total_isolates, ")"),
    Isolates_n = as.integer(total_isolates),
    `Resistance Profile` = "70% MDR (7/10)"
  )

# Step 8: Bind rows
final_table <- bind_rows(
  table_data,
  total_row
) %>%
  select(microbe, Isolates_n, all_of(selected_abx), `Resistance Profile`)

# Step 9: Create formatted GT table
rape_table <- gt(final_table) %>%
  tab_header(
    title = "Rape Resistance"
  ) %>%
  cols_label(
    microbe = "Microorganism",
    Isolates_n = "Isolates (n)"
  ) %>%
  cols_align("left", columns = everything())

rape_table


# Chinese cabbage ---------------------------------------------------------
# Step 1: Filter only Chinese
chicken_data <- microbiology %>%
  filter(item == "Chinese Cabbage")

# Step 2: Define antibiotics of interest
selected_abx <- c( "SXT", "CIP", "TGC", "CTX", "AMP", "GM")

levels(microbiology$antibiotic)
# Step 3: Calculate resistance % per microbe per antibiotic
resistance_summary <- chicken_data %>%
  filter(antibiotic %in% selected_abx) %>%
  mutate(is_resistant = ifelse(sensitivity == "R", 1, 0)) %>%
  group_by(microbe, antibiotic) %>%
  summarise(
    resistance_percent = round(mean(is_resistant) * 100),
    n_isolates = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = antibiotic, values_from = resistance_percent, values_fill = 0)

print(names(resistance_summary))

# Step 4: Get isolate counts per microbe
isolate_counts <- chicken_data %>%
  count(microbe, name = "Isolates_n") %>%
  distinct()
# Step 5: Join isolate counts with resistance data
table_data <- isolate_counts %>%
  left_join(resistance_summary, by = "microbe") %>%
  group_by(microbe) %>%
  summarise(
    Isolates_n = max(Isolates_n),
    across(all_of(selected_abx), ~ max(.x, na.rm = TRUE), .names = "{.col}"),
    .groups = "drop"
  )



# Step 6: Generate resistance profile text
make_profile <- function(row) {
  abx_cols <- intersect(names(row), selected_abx)
  res_abx <- abx_cols[which(as.numeric(row[abx_cols]) == 100)]
  if (length(res_abx) == 0) return("")
  paste0(paste(res_abx, collapse = "-"), " (", row[["Isolates_n"]], "/", row[["Isolates_n"]], ")")
}

table_data <- table_data %>%
  rowwise() %>%
  mutate(
    `Resistance Profile` = make_profile(cur_data())
  ) %>%
  ungroup()

# Step 7: Add total row - FIXED TYPE CONVERSION
total_isolates <- isolate_counts %>%
  summarise(total = sum(Isolates_n)) %>%
  pull(total)


total_row <- chicken_data %>%
  filter(antibiotic %in% selected_abx) %>%
  group_by(antibiotic) %>%
  summarise(resistance_percent = round(mean(sensitivity == "R") * 100)) %>%
  pivot_wider(names_from = antibiotic, values_from = resistance_percent, values_fill = 0) %>%
  mutate(
    microbe = paste0("Total Chinese (n = ", total_isolates, ")"),
    Isolates_n = as.integer(total_isolates),
    `Resistance Profile` = "70% MDR (7/10)"
  )


# Step 8: Bind rows - now compatible types
final_table <- bind_rows(
  table_data %>% mutate(Isolates_n = as.integer(Isolates_n)),
  total_row
) %>%
  select(microbe, Isolates_n, all_of(selected_abx), `Resistance Profile`)

# Step 9: Create formatted GT table
chicken<-gt(final_table) %>%
  tab_header(
    title = "Chinese Cabbage Resistance"
  ) %>%
  cols_label(
    microbe = "Microorganism",
    Isolates_n = "Isolates (n)"
  ) %>%
  cols_align("left", columns = "microbe") %>%
  cols_align("left", columns = everything())

chicken


chicken |> gtsave("Chinese.docx")

# Amaranthus --------------------------------------------------------------
# Step 1: Filter only amaranthus
chicken_data <- microbiology %>%
  filter(item == "Amaranthus")

# Step 2: Define antibiotics of interest
# Step 2: Define antibiotics of interest
selected_abx <- c( "SXT", "CIP", "TGC", "CTX", "AMP", "GM", "ATM", "CRO", "VAN")

levels(microbiology$antibiotic)
# Step 3: Calculate resistance % per microbe per antibiotic
resistance_summary <- chicken_data %>%
  filter(antibiotic %in% selected_abx) %>%
  mutate(is_resistant = ifelse(sensitivity == "R", 1, 0)) %>%
  group_by(microbe, antibiotic) %>%
  summarise(
    resistance_percent = round(mean(is_resistant) * 100),
    n_isolates = n(),
    .groups = "drop"
  ) %>%
  pivot_wider(names_from = antibiotic, values_from = resistance_percent, values_fill = 0)

print(names(resistance_summary))

# Step 4: Get isolate counts per microbe
isolate_counts <- chicken_data %>%
  count(microbe, name = "Isolates_n") %>%
  distinct()
# Step 5: Join isolate counts with resistance data
table_data <- isolate_counts %>%
  left_join(resistance_summary, by = "microbe") %>%
  group_by(microbe) %>%
  summarise(
    Isolates_n = max(Isolates_n),
    across(all_of(selected_abx), ~ max(.x, na.rm = TRUE), .names = "{.col}"),
    .groups = "drop"
  )



# Step 6: Generate resistance profile text
make_profile <- function(row) {
  abx_cols <- intersect(names(row), selected_abx)
  res_abx <- abx_cols[which(as.numeric(row[abx_cols]) == 100)]
  if (length(res_abx) == 0) return("")
  paste0(paste(res_abx, collapse = "-"), " (", row[["Isolates_n"]], "/", row[["Isolates_n"]], ")")
}

table_data <- table_data %>%
  rowwise() %>%
  mutate(
    `Resistance Profile` = make_profile(cur_data())
  ) %>%
  ungroup()

# Step 7: Add total row - FIXED TYPE CONVERSION
total_isolates <- isolate_counts %>%
  summarise(total = sum(Isolates_n)) %>%
  pull(total)


total_row <- chicken_data %>%
  filter(antibiotic %in% selected_abx) %>%
  group_by(antibiotic) %>%
  summarise(resistance_percent = round(mean(sensitivity == "R") * 100)) %>%
  pivot_wider(names_from = antibiotic, values_from = resistance_percent, values_fill = 0) %>%
  mutate(
    microbe = paste0("Total Amaranthus (n = ", total_isolates, ")"),
    Isolates_n = as.integer(total_isolates),
    `Resistance Profile` = "70% MDR (7/10)"
  )


# Step 8: Bind rows - now compatible types
final_table <- bind_rows(
  table_data %>% mutate(Isolates_n = as.integer(Isolates_n)),
  total_row
) %>%
  select(microbe, Isolates_n, all_of(selected_abx), `Resistance Profile`)

# Step 9: Create formatted GT table
chicken<-gt(final_table) %>%
  tab_header(
    title = "Amaranthus Resistance"
  ) %>%
  cols_label(
    microbe = "Microorganism",
    Isolates_n = "Isolates (n)"
  ) %>%
  cols_align("left", columns = "microbe") %>%
  cols_align("left", columns = everything())

chicken


chicken |> gtsave("Amaranthus.docx")

# Cabbage revised ---------------------------------------------------------

# Load required packages
library(dplyr)
library(tidyr)
library(gt)
library(binom)  # For binomial confidence intervals

# ===================== Chinese Cabbage =====================
# Step 1: Filter only Chinese Cabbage
cabbage_data <- microbiology %>%
  filter(item == "Chinese Cabbage")

# Step 2: Define antibiotics of interest
selected_abx <- c("SXT", "CIP", "TGC", "CTX", "AMP", "GM")

# Step 3: Calculate resistance % with CI per microbe per antibiotic
resistance_summary <- cabbage_data %>%
  filter(antibiotic %in% selected_abx) %>%
  mutate(is_resistant = ifelse(sensitivity == "R", 1, 0)) %>%
  group_by(microbe, antibiotic) %>%
  summarise(
    n_resistant = sum(is_resistant),
    n_isolates = n(),
    resistance_percent = round(mean(is_resistant) * 100),
    .groups = "drop"
  ) %>%
  # Calculate binomial confidence intervals
  mutate(
    binom_ci = binom.confint(n_resistant, n_isolates, methods = "wilson"),
    lower_ci = round(binom_ci$lower * 100),
    upper_ci = round(binom_ci$upper * 100),
    # Format as "X% (Y-Z)" without % in CI
    value_formatted = sprintf("%d%% (%d-%d)", resistance_percent, lower_ci, upper_ci)
  ) %>%
  select(microbe, antibiotic, value_formatted) %>%
  pivot_wider(
    names_from = antibiotic,
    values_from = value_formatted,
    values_fill = "0% (0-0)"
  )

# Step 4: Get isolate counts per microbe
isolate_counts <- cabbage_data %>%
  count(microbe, name = "Isolates_n") %>%
  distinct()

# Step 5: Join isolate counts with resistance data
table_data <- isolate_counts %>%
  left_join(resistance_summary, by = "microbe") %>%
  mutate(across(all_of(selected_abx), ~ coalesce(.x, "0% (0-0)")))

# Step 6: Generate resistance profile text using numeric values
numeric_resistance <- cabbage_data %>%
  filter(antibiotic %in% selected_abx) %>%
  mutate(is_resistant = ifelse(sensitivity == "R", 1, 0)) %>%
  group_by(microbe, antibiotic) %>%
  summarise(
    resistance_percent = round(mean(is_resistant) * 100),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = antibiotic,
    values_from = resistance_percent,
    values_fill = 0
  )

make_profile <- function(microbe_name) {
  if (!microbe_name %in% numeric_resistance$microbe) return("")
  
  row_data <- numeric_resistance %>% filter(microbe == microbe_name)
  n_isolates <- isolate_counts$Isolates_n[isolate_counts$microbe == microbe_name]
  
  if (length(n_isolates) == 0 || is.na(n_isolates) || n_isolates == 0) return("")
  
  res_abx <- c()
  for (abx in selected_abx) {
    if (abx %in% names(row_data)) {
      value <- row_data[[abx]]
      if (!is.na(value) && value == 100) {
        res_abx <- c(res_abx, abx)
      }
    }
  }
  
  if (length(res_abx) == 0) return("")
  paste0(paste(res_abx, collapse = "-"), " (", n_isolates, "/", n_isolates, ")")
}

table_data <- table_data %>%
  rowwise() %>%
  mutate(`Resistance Profile` = make_profile(microbe)) %>%
  ungroup() %>%
  mutate(`Resistance Profile` = ifelse(is.na(`Resistance Profile`), "", `Resistance Profile`))

# Step 7: Add total row
total_isolates <- sum(isolate_counts$Isolates_n)

total_row <- cabbage_data %>%
  filter(antibiotic %in% selected_abx) %>%
  group_by(antibiotic) %>%
  summarise(
    n_resistant = sum(sensitivity == "R"),
    n_isolates = n(),
    resistance_percent = round(mean(sensitivity == "R") * 100),
    .groups = "drop"
  ) %>%
  mutate(
    binom_ci = binom.confint(n_resistant, n_isolates, methods = "wilson"),
    lower_ci = round(binom_ci$lower * 100),
    upper_ci = round(binom_ci$upper * 100),
    value_formatted = sprintf("%d%% (%d-%d)", resistance_percent, lower_ci, upper_ci)
  ) %>%
  select(antibiotic, value_formatted) %>%
  pivot_wider(
    names_from = antibiotic,
    values_from = value_formatted
  ) %>%
  mutate(
    microbe = paste0("Total Chinese (n = ", total_isolates, ")"),
    Isolates_n = as.integer(total_isolates),
    `Resistance Profile` = "70% MDR (7/10)"
  )

# Step 8: Bind rows
final_table <- bind_rows(
  table_data,
  total_row
) %>%
  select(microbe, Isolates_n, all_of(selected_abx), `Resistance Profile`)

# Step 9: Create formatted GT table
cabbage_table <- gt(final_table) %>%
  tab_header(
    title = "Chinese Cabbage Resistance"
  ) %>%
  cols_label(
    microbe = "Microorganism",
    Isolates_n = "Isolates (n)"
  ) %>%
  cols_align("left", columns = everything())

cabbage_table

#