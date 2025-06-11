
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

#########chicken manure
library(dplyr)
library(tidyr)
library(gt)

# Step 1: Filter only chicken manure
chicken_data <- microbiology %>%
  filter(item == "Chicken Manure")

# Step 2: Define antibiotics of interest (optional: you can customize this)
selected_abx <- c("AMP", "CIP", "CTX", "GM", "SXT", "MEM")

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

# Step 4: Get isolate counts per microbe
isolate_counts <- chicken_data %>%
  count(microbe, name = "Isolates_n") %>%
  distinct()

class(microbiology$microbe)

# Step 5: Join isolate counts with resistance data
table_data <- isolate_counts %>%
  left_join(resistance_summary, by = "microbe")

# Step 6: Generate resistance profile text
make_profile <- function(row) {
  abx_cols <- intersect(names(row), selected_abx)
  res_abx <- abx_cols[which(row[abx_cols] == 100)]
  if (length(res_abx) == 0) return("")
  paste0(paste(res_abx, collapse = "-"), " (", row[["Isolates_n"]], "/", row[["Isolates_n"]], ")")
}

table_data <- table_data %>%
  rowwise() %>%
  mutate(
    `Resistance Profile` = make_profile(cur_data())
  ) %>%
  ungroup()

# Step 7: Add total row
total_row <- chicken_data %>%
  filter(antibiotic %in% selected_abx) %>%
  group_by(antibiotic) %>%
  summarise(resistance_percent = round(mean(sensitivity == "R") * 100)) %>%
  pivot_wider(names_from = antibiotic, values_from = resistance_percent, values_fill = 0) %>%
  mutate(
    microbe = "Total Chicken (n = 10)",
    Isolates_n = "",
    `Resistance Profile` = "70% MDR (7/10)"
  )

# Step 8: Bind total row
final_table <- bind_rows(table_data, total_row) %>%
  select(microbe, Isolates_n, all_of(selected_abx), `Resistance Profile`)


gt(final_table) %>%
  tab_header(
    title = "Table 3.1: Chicken Manure Resistance",
    subtitle = "High gentamicin resistance in Klebsiella isolates"
  ) %>%
  cols_label(
    microbe = "Microorganism",
    Isolates_n = "Isolates (n)"
  ) %>%
  cols_align("left", columns = "microbe") %>%
  cols_align("center", columns = everything())




