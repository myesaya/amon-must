
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




microbiology |> 
  filter(item == "Chicken manure", 
         str_detect(microbe, "E\\.? coli")) |>  
  summarise(n = n())                            # Count rows

levels(microbiology$microbe) # Check levels of the microbe factor)

microbiology |> 
  filter(item == "Chicken manure") |>  
  summarise(n = n())                   
