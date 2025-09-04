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


survey_df<- read_csv(here::here("data/raw/Antibiotic_Usage_Practices_among_Pig_and_Poultry_Farmers_-_all_versions.csv")) |> 
  clean_names() |> 
  mutate_if(is.character, as_factor) 

levels(microbiology$farm)

survey_df<- survey_df |> 
  mutate(
    years_farming_cat = case_when(
      years_farming >= 1 & years_farming <= 5 ~ "1-5",
      years_farming >= 6 & years_farming <= 10 ~ "6-10",
      years_farming >= 11 & years_farming <= 15 ~ "11-15",
      TRUE ~ "16+"
    )
  )

Demographics<-survey_df |> 
  select(gender,age,education,farm_type,years_farming_cat,years_farming)


Demographics |> 
  tbl_summary(statistic=list(all_continuous() ~ " ({mean} ± {sd})"),
              missing = "no")


Demographics |> 
  summarise(N = sum(!is.na(years_farming))) |> 
  pull(N)


# Extract the summary data into a data frame
summary_df <- summary_table$table_body

summary_df<-summary_df |> 
  rename(Characteristic = variable,
         Category = label) 


# Separate frequencies and percentages into different columns
summary_df <- summary_df %>%
  mutate(
    Frequency = gsub(" \\(.*\\)", "", stat_0), # Extract frequency
    Percentage = gsub(".*\\((.*)%\\)", "\\1", stat_0) # Extract percentage
  ) %>%
  select(Characteristic,Category,  Frequency, Percentage) # Select relevant columns
# Replace NAs with blank strings
summary_df <- summary_df %>%
  mutate(
    Frequency = na_if(Frequency, ""),
    Percentage = na_if(Percentage, "")
  ) |> 
  na.omit()

# Create a gt table with separate columns for frequency and percentage
gt_table <- summary_df |> 
  gt() |> 
  tab_header(
    title = "Survey Summary Table"
  ) |> 
  cols_label(
    Frequency = "Frequency (N)",
    Percentage = "Percentage (%)"
  ) |> 
  fmt_missing(
    columns = everything(), # Apply to all columns
    missing_text = ""      # Set missing text to blank
  )

# Display the gt table
print(gt_table)
gtsave(gt_table, filename = "amon demo.docx")



altitude<-survey |> 
  select(gender,age,education,farm_type,years_farming_cat,years_farming,starts_with("products")) 


a<-altitude |> 
  select(gender,age,education,farm_type,years_farming_cat,years_farming,products_antibiotics) |> 
  tbl_summary(by=products_antibiotics,
              missing = "no",
              statistic = list(
                all_continuous() ~ "{mean}±{sd}",
                all_categorical() ~ "{n} ({p}%)")
  ) |> 
  add_p(pvalue_fun = function(x) style_number(x, digits = 3)  # Format p-values to 3 decimal places
  ) |> 
  bold_p ()|> 
  add_significance_stars() |> 
  separate_p_footnotes() |> 
  modify_header(p.value ~ "**Antibiotics**")


b<-altitude |> 
  select(gender,age,education,farm_type,years_farming_cat,years_farming,products_erbal) |> 
  tbl_summary(by=products_erbal,
              missing = "no",
              statistic = list(
                all_continuous() ~ "{mean}±{sd}",
                all_categorical() ~ "{n} ({p}%)")
  ) |> 
  add_p(pvalue_fun = function(x) style_number(x, digits = 3)  # Format p-values to 3 decimal places
  ) |> 
  bold_p ()|> 
  add_significance_stars() |> 
  separate_p_footnotes() |> 
  modify_header(p.value ~ "**Herbals**")



c<-altitude |> 
  select(gender,age,education,farm_type,years_farming_cat,years_farming,products_vaccines) |> 
  tbl_summary(by=products_vaccines,
              missing = "no",
              statistic = list(
                all_continuous() ~ "{mean}±{sd}",
                all_categorical() ~ "{n} ({p}%)")
  ) |> 
  add_p(pvalue_fun = function(x) style_number(x, digits = 3)  # Format p-values to 3 decimal places
  ) |> 
  bold_p ()|> 
  add_significance_stars() |> 
  separate_p_footnotes() |> 
  modify_header(p.value ~ "**Vaccine**")




d<-altitude |> 
  select(gender,age,education,farm_type,years_farming_cat,years_farming,products_none) |> 
  tbl_summary(
    by = products_none,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean}±{sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) |>
  add_p(
    pvalue_fun = function(x) style_number(x, digits = 3)  # Format p-values to 3 decimal places
  ) |> 
  bold_p ()|> 
  add_significance_stars() |> 
  separate_p_footnotes() |> 
  modify_header(p.value ~ "**None**")


library(gtsummary)
library(tidyselect)

z<-tbl_merge(
  tbls = list(a, b, c, d)
) |> as_gt()


z |> 
  gtsave("Altitude.docx")






# Knowledge and altitudes -------------------------------------------------




data<-survey_df|> 
  select(gender,age,education,farm_type,years_farming_cat,years_farming,follow_prescription:withdraww)

dataf<-data |> 
  select(follow_prescription:withdraw)

a<-data |> 
  select(follow_prescription:withdraw,gender) |> 
  tbl_summary(
    by = gender,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean}±{sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) |>
  add_p(
    pvalue_fun = function(x) style_number(x, digits = 3)  # Format p-values to 3 decimal places
  ) |> 
  bold_p ()|> 
  add_significance_stars() |> 
  separate_p_footnotes() |>  
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Gender**") 


# Age ---------------------------------------------------------------------
b<-data |> 
  select(follow_prescription:withdraw,age) |> 
  tbl_summary(
    by = age,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean}±{sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) |>
  add_p(
    pvalue_fun = function(x) style_number(x, digits = 3)  # Format p-values to 3 decimal places
  ) |> 
  bold_p ()|> 
  add_significance_stars() |> 
  separate_p_footnotes() |>  
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Age**") 


# education ---------------------------------------------------------------

c<-data |> 
  select(follow_prescription:withdraw,education) |> 
  tbl_summary(
    by = education,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean}±{sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) |>
  add_p(
    pvalue_fun = function(x) style_number(x, digits = 3)  # Format p-values to 3 decimal places
  ) |> 
  bold_p ()|> 
  add_significance_stars() |> 
  separate_p_footnotes() |>  
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Education**") 



# farm_type ---------------------------------------------------------------


d<-data |> 
  select(follow_prescription:withdraw,farm_type) |> 
  tbl_summary(
    by = farm_type,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean}±{sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) |>
  add_p(
    pvalue_fun = function(x) style_number(x, digits = 3)  # Format p-values to 3 decimal places
  ) |> 
  bold_p ()|> 
  add_significance_stars() |> 
  separate_p_footnotes() |>  
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Farn Type**") 

# years of farming --------------------------------------------------------


e<-data |> 
  select(follow_prescription:withdraw,years_farming_cat) |> 
  tbl_summary(
    by = years_farming_cat,
    missing = "no",
    statistic = list(
      all_continuous() ~ "{mean}±{sd}",
      all_categorical() ~ "{n} ({p}%)"
    ),
    digits = all_continuous() ~ 1
  ) |>
  add_p(
    pvalue_fun = function(x) style_number(x, digits = 3)  # Format p-values to 3 decimal places
  ) |> 
  bold_p ()|> 
  add_significance_stars() |> 
  separate_p_footnotes() |>  
  modify_spanning_header(c("stat_1", "stat_2") ~ "**Years as Farmer**") 

z<-tbl_merge(
  tbls = list(a, b, c, d, e)
)



print(z)
z |> 
as_flex_table() |> 
  save_as_docx(path = "~/gitrepos/amon-must/assoiation3.docx")

dataf<-data|> 
  select(follow_prescription:withdraw)




library(dplyr)
library(tidyr)
library(gt)

# Replace NAs in relevant columns to avoid missing values affecting calculations
dataf <- dataf %>%
  mutate(across(everything(), ~replace_na(., "No")))  # Treat missing as "No"

library(dplyr)
library(tidyr)
library(gt)

# Replace NAs in relevant columns to avoid missing values affecting calculations
dataf <- dataf %>%
  mutate(across(everything(), ~replace_na(., "No")))  # Treat missing as "No"

# Summarize response frequencies
freq_table <- dataf %>%
  summarise(
    across(everything(), list(
      Yes = ~sum(. == "Yes"),
      No = ~sum(. == "No"),
      IDK = ~sum(. == "I don't know") # Include "I don't know"
    ))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Response"),
    names_pattern = "(.*)_(Yes|No|IDK)", 
    values_to = "Count"
  ) %>%
  group_by(Variable, Response) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  pivot_wider(
    names_from = "Response", 
    values_from = "Count", 
    values_fill = list(Count = 0)
  ) %>%
  mutate(
    Total = Yes + No + IDK,  # Adjust total to include IDK
    Yes_Percent = round((Yes / Total) * 100, 0),  # Whole numbers
    No_Percent = round((No / Total) * 100, 0),  
    IDK_Percent = round((IDK / Total) * 100, 0) # Whole numbers
  ) %>%
  select(Variable, Yes, Yes_Percent, No, No_Percent, IDK, IDK_Percent) # Keep all relevant columns

# Perform chi-square test and add p-values
freq_table <- freq_table %>%
  rowwise() %>%
  mutate(
    p_value = tryCatch(
      chisq.test(c(Yes, No, IDK))$p.value, 
      error = function(e) NA  # Handle errors if expected counts are too low
    )
  ) %>%
  ungroup()

# Generate table using gt
gt_table <- freq_table %>%
  gt() %>%
  fmt_number(
    columns = p_value,
    decimals = 3
  ) %>%
  cols_label(
    Variable = "Variable",
    Yes = "Yes (N)",
    Yes_Percent = "Yes (%)",
    No = "No (N)",
    No_Percent = "No (%)",
    IDK = "I Don't Know (N)",
    IDK_Percent = "I Don't Know (%)",
    p_value = "p-value"
  )

# Print the formatted table
gt_table

# Save table
gtsave(gt_table, filename = "guidelines_gt2.docx")




library(dplyr)
library(tidyr)
library(gt)

# Summarize response frequencies
freq_table <- dataf %>%
  summarise(
    across(everything(), list(
      Yes = ~sum(. == "Yes"),
      No = ~sum(. == "No"),
      IDK = ~sum(. == "I don't know") # Include "I don't know"
    ))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Response"),
    names_pattern = "(.*)_(Yes|No|IDK)", 
    values_to = "Count"
  ) %>%
  group_by(Variable, Response) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  pivot_wider(
    names_from = "Response", 
    values_from = "Count", 
    values_fill = list(Count = 0)
  ) %>%
  mutate(
    Total = Yes + No + IDK,  # Adjust total to include IDK
    Yes_Percent = round((Yes / Total) * 100, 1),
    No_Percent = round((No / Total) * 100, 1),
    IDK_Percent = round((IDK / Total) * 100, 1) # Calculate percentage for IDK
  ) %>%
  select(Variable, Yes, Yes_Percent, No, No_Percent, IDK, IDK_Percent) # Include IDK

# Perform chi-square test and add p-values
freq_table <- freq_table %>%
  rowwise() %>%
  mutate(
    p_value = tryCatch(
      chisq.test(c(Yes, No, IDK))$p.value, 
      error = function(e) NA  # Handle errors if expected counts are too low
    )
  ) %>%
  ungroup()

# Generate table using gt
gt_table <- freq_table %>%
  gt() %>%
  fmt_number(
    columns = p_value,
    decimals = 3
  ) %>%
  cols_label(
    Variable = "Variable",
    Yes = "Yes (N)",
    Yes_Percent = "Yes (%)",
    No = "No (N)",
    No_Percent = "No (%)",
    IDK = "I Don't Know (N)",
    IDK_Percent = "I Don't Know (%)",
    p_value = "p-value"
  )

# Print the formatted table
gt_table

# Save table
gtsave(gt_table, filename = "guidelines_gt2.docx")




library(dplyr)
library(tidyr)
library(gt)

# Summarize response frequencies
freq_table <- dataf %>%
  summarise(
    across(everything(), list(
      Yes = ~sum(. == "Yes"),
      No = ~sum(. == "No"),
      IDK = ~sum(. == "I don't know") # Include "I don't know"
    ))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Response"),
    names_pattern = "(.*)_(Yes|No|IDK)", 
    values_to = "Count"
  ) %>%
  group_by(Variable, Response) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  pivot_wider(
    names_from = "Response", 
    values_from = "Count", 
    values_fill = list(Count = 0)
  ) %>%
  mutate(
    Total = Yes + No + IDK,  # Adjust total to include IDK
    Yes_Percent = round((Yes / Total) * 100, 1),
    No_Percent = round((No / Total) * 100, 1),
    IDK_Percent = round((IDK / Total) * 100, 1) # Calculate percentage for IDK
  ) %>%
  select(Variable, Yes, Yes_Percent, No, No_Percent, IDK, IDK_Percent) # Include IDK

# Perform chi-square test and add p-values
freq_table <- freq_table %>%
  rowwise() %>%
  mutate(
    p_value = tryCatch(
      chisq.test(c(Yes, No, IDK))$p.value, 
      error = function(e) NA  # Handle errors if expected counts are too low
    )
  ) %>%
  ungroup()

# Generate table using gt
gt_table <- freq_table %>%
  gt() %>%
  fmt_number(
    columns = p_value,
    decimals = 3
  ) %>%
  cols_label(
    Variable = "Variable",
    Yes = "Yes (N)",
    Yes_Percent = "Yes (%)",
    No = "No (N)",
    No_Percent = "No (%)",
    IDK = "I Don't Know (N)",
    IDK_Percent = "I Don't Know (%)",
    p_value = "p-value"
  )

# Print the formatted table
gt_table

# Save table
gtsave(gt_table, filename = "guidelines_gt2.docx")












library(dplyr)
library(tidyr)
library(gt)

# Replace NAs in relevant columns to avoid missing values affecting calculations
dataf <- dataf %>%
  mutate(across(everything(), ~replace_na(., "No")))  # Treat missing as "No"

# Summarize response frequencies
freq_table <- dataf %>%
  summarise(
    across(everything(), list(
      Yes = ~sum(. == "Yes"),
      No = ~sum(. == "No"),
      IDK = ~sum(. == "I don't know") # Include "I don't know"
    ))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Response"),
    names_pattern = "(.*)_(Yes|No|IDK)", 
    values_to = "Count"
  ) %>%
  group_by(Variable, Response) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  pivot_wider(
    names_from = "Response", 
    values_from = "Count", 
    values_fill = list(Count = 0)
  ) %>%
  mutate(
    Total = Yes + No + IDK,  # Adjust total to include IDK
    Yes_Percent = round((Yes / Total) * 100, 0),  # Whole numbers
    No_Percent = round((No / Total) * 100, 0),  
    IDK_Percent = round((IDK / Total) * 100, 0) # Whole numbers
  ) %>%
  select(Variable, Yes, Yes_Percent, No, No_Percent, IDK, IDK_Percent) # Keep all relevant columns

# Perform chi-square test and add p-values
freq_table <- freq_table %>%
  rowwise() %>%
  mutate(
    p_value = tryCatch(
      chisq.test(c(Yes, No, IDK))$p.value, 
      error = function(e) NA  # Handle errors if expected counts are too low
    )
  ) %>%
  ungroup()

# Generate table using gt
gt_table <- freq_table %>%
  gt() %>%
  fmt_number(
    columns = p_value,
    decimals = 3
  ) %>%
  cols_label(
    Variable = "Variable",
    Yes = "Yes (N)",
    Yes_Percent = "Yes (%)",
    No = "No (N)",
    No_Percent = "No (%)",
    IDK = "I Don't Know (N)",
    IDK_Percent = "I Don't Know (%)",
    p_value = "p-value"
  )

# Print the formatted table
gt_table

# Save table
gtsave(gt_table, filename = "guidelines_gt3.docx")


#disposal methods (expired drugs)

disposal<-survey_df |> 
  select(dispose_return:disposal_other)
disposal <- disposal %>%
  mutate(across(everything(), ~ ifelse(. == 1, "Yes", "No")))



library(dplyr)
library(tidyr)
library(gt)

# Select disposal-related variables and convert 1 → "Yes", 0 → "No"
disposal <- survey_df %>%
  select(dispose_return:disposal_other) %>%
  mutate(across(everything(), ~ ifelse(. == 1, "Yes", "No")))

# Summarize response frequencies
freq_table <- disposal %>%
  summarise(
    across(everything(), list(
      Yes = ~sum(. == "Yes"),
      No = ~sum(. == "No")
    ))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Response"),
    names_pattern = "(.*)_(Yes|No)",  # Match only "Yes" and "No"
    values_to = "Count"
  ) %>%
  group_by(Variable, Response) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  pivot_wider(
    names_from = "Response", 
    values_from = "Count", 
    values_fill = list(Count = 0)
  ) %>%
  mutate(
    Total = Yes + No,  # No IDK category
    Yes_Percent = round((Yes / Total) * 100, 0),  # Whole numbers
    No_Percent = round((No / Total) * 100, 0)  
  ) %>%
  select(Variable, Yes, Yes_Percent, No, No_Percent) # Keep relevant columns

# Perform chi-square test and add p-values
freq_table <- freq_table %>%
  rowwise() %>%
  mutate(
    p_value = tryCatch(
      chisq.test(c(Yes, No))$p.value, 
      error = function(e) NA  # Handle errors if expected counts are too low
    )
  ) %>%
  ungroup()

# Generate table using gt
gt_table <- freq_table %>%
  gt() %>%
  fmt_number(
    columns = p_value,
    decimals = 3
  ) %>%
  cols_label(
    Variable = "Disposal Method",
    Yes = "Yes (N)",
    Yes_Percent = "Yes (%)",
    No = "No (N)",
    No_Percent = "No (%)",
    p_value = "p-value"
  )

# Print the formatted table
gt_table

# Save table
gtsave(gt_table, filename = "guidelines_gt3.docx")
#disposal methods (Packaging)

disposal_p<-survey_df |> 
  select(dispose_packaging_reuse:dispose_packaging_field)

disposal_p <- disposal_p %>%
  mutate(across(everything(), ~ ifelse(. == 1, "Yes", "No")))



library(dplyr)
library(tidyr)
library(gt)

# Select disposal-related variables and convert 1 → "Yes", 0 → "No"
#disposal <- survey_df %>%
 # select(dispose_return:disposal_other) %>%
  #mutate(across(everything(), ~ ifelse(. == 1, "Yes", "No")))

# Summarize response frequencies
freq_table <- disposal_p %>%
  summarise(
    across(everything(), list(
      Yes = ~sum(. == "Yes"),
      No = ~sum(. == "No")
    ))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Response"),
    names_pattern = "(.*)_(Yes|No)",  # Match only "Yes" and "No"
    values_to = "Count"
  ) %>%
  group_by(Variable, Response) %>%
  summarise(Count = sum(Count), .groups = "drop") %>%
  pivot_wider(
    names_from = "Response", 
    values_from = "Count", 
    values_fill = list(Count = 0)
  ) %>%
  mutate(
    Total = Yes + No,  # No IDK category
    Yes_Percent = round((Yes / Total) * 100, 0),  # Whole numbers
    No_Percent = round((No / Total) * 100, 0)  
  ) %>%
  select(Variable, Yes, Yes_Percent, No, No_Percent) # Keep relevant columns

# Perform chi-square test and add p-values
freq_table <- freq_table %>%
  rowwise() %>%
  mutate(
    p_value = tryCatch(
      chisq.test(c(Yes, No))$p.value, 
      error = function(e) NA  # Handle errors if expected counts are too low
    )
  ) %>%
  ungroup()

# Generate table using gt
gt_table <- freq_table %>%
  gt() %>%
  fmt_number(
    columns = p_value,
    decimals = 3
  ) %>%
  cols_label(
    Variable = "Disposal Method",
    Yes = "Yes (N)",
    Yes_Percent = "Yes (%)",
    No = "No (N)",
    No_Percent = "No (%)",
    p_value = "p-value"
  )

# Print the formatted table
gt_table

# Save table
gtsave(gt_table, filename = "guidelines_gt4.docx")

###################################combined##########################3
library(tidyverse)
library(gt)

# Convert 1 = "Yes" and 0 = "No"
purpose <- survey_df |> 
select(starts_with("purpose_")) |> 
  mutate(across(everything(), ~ ifelse(. == 1, "Yes", "No")))


# Reshape to long format
purpose_long <- purpose |> 
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Response") |> 
  mutate(
    Category = case_when(
      str_detect(Variable, "_exp$") ~ "Expired Drugs",
      str_detect(Variable, "_pkg$") ~ "Empty Packaging",
      TRUE ~ "Other"
    )
  )

# Summarize response frequencies
freq_table <- disposal_long %>%
  group_by(Category, Variable, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  pivot_wider(names_from = Response, values_from = Count, values_fill = list(Count = 0)) %>%
  mutate(
    Total = Yes + No, 
    Yes_Percent = round((Yes / Total) * 100, 0),  
    No_Percent = round((No / Total) * 100, 0)  
  )

# Pivot wider so Expired Drugs and Empty Packaging are side by side
final_table <- freq_table %>%
  pivot_wider(
    names_from = Category, 
    values_from = c(Yes, Yes_Percent, No, No_Percent),
    names_glue = "{Category}_{.value}"
  ) %>%
  rename(
    `Disposal Method` = Variable,
    
    `Expired Drugs (Yes N)` = `Expired Drugs_Yes`,
    `Expired Drugs (Yes %)` = `Expired Drugs_Yes_Percent`,
    `Expired Drugs (No N)` = `Expired Drugs_No`,
    `Expired Drugs (No %)` = `Expired Drugs_No_Percent`,
    
    `Empty Packaging (Yes N)` = `Empty Packaging_Yes`,
    `Empty Packaging (Yes %)` = `Empty Packaging_Yes_Percent`,
    `Empty Packaging (No N)` = `Empty Packaging_No`,
    `Empty Packaging (No %)` = `Empty Packaging_No_Percent`
  )

# Perform chi-square tests for each disposal method
final_table <- final_table %>%
  rowwise() %>%
  mutate(
    p_value = tryCatch(
      chisq.test(matrix(c(`Expired Drugs (Yes N)`, `Expired Drugs (No N)`, 
                          `Empty Packaging (Yes N)`, `Empty Packaging (No N)`), 
                        nrow = 2))$p.value, 
      error = function(e) NA  
    )
  ) %>%
  ungroup()

# Format table using gt
gt_table <- final_table %>%
  gt() %>%
  fmt_number(columns = p_value, decimals = 3) %>%
  cols_label(
    `Disposal Method` = "Disposal Method",
    
    `Expired Drugs (Yes N)` = "Yes (N)",
    `Expired Drugs (Yes %)` = "Yes (%)",
    `Expired Drugs (No N)` = "No (N)",
    `Expired Drugs (No %)` = "No (%)",
    
    `Empty Packaging (Yes N)` = "Yes (N)",
    `Empty Packaging (Yes %)` = "Yes (%)",
    `Empty Packaging (No N)` = "No (N)",
    `Empty Packaging (No %)` = "No (%)",
    
    p_value = "p-value"
  ) %>%
  tab_spanner(label = "Expired Drugs", columns = starts_with("Expired Drugs")) %>%
  tab_spanner(label = "Empty Packaging", columns = starts_with("Empty Packaging"))

# Save table
gtsave(gt_table, filename = "association_table.docx")

# Print table
gt_table




library(tidyverse)

library(tidyverse)

# Transform data
products <- survey_df |> 
  select(starts_with("product"), often,id) |> 
  mutate(across(everything(), ~ ifelse(. == 1, "Yes", "No")))

# Reshape to long format and rename product names using fct_recode()
library(tidyverse)
#install.packages("extrafont")
library(extrafont)
font_import()

loadfonts(device = "win")
# Reshape to long format and rename product names using fct_recode()
products_long <- products |> 
  pivot_longer(cols = everything(), names_to = "Product", values_to = "Response") |> 
  mutate(Product = fct_recode(Product,
                              "Antibiotics" = "often", 
                              "Vaccines" = "products_vaccines", 
                              "Herbals" = "products_erbal",
                              "None" = "products_none")) 

# If you have an id column, calculate total respondents based on unique ids:
# If not, just use total rows
if("id" %in% names(products_long)) {
  total_n <- n_distinct(products_long$id)
} else {
  total_n <- nrow(products_long) / length(unique(products_long$Product)) # assuming wide data reshaped to long
}

# Calculate summary for "Yes" responses per Product
summary_data <- products_long |>
  filter(Response == "Yes") |>
  group_by(Product) |>
  summarise(
    Count = n(),
    Percentage = (Count / total_n) * 100,
    SE = sqrt((Percentage / 100) * (1 - Percentage / 100) / total_n) * 100,
    .groups = 'drop'
  ) |> 
  filter(Percentage >= 1)  # remove categories with <1%

# Load required packages
library(ggplot2)
library(forcats)
library(extrafont)

# Load the fonts (you might need to run this once per session)
loadfonts(device = "win")

# Create the plot with Palatino Linotype font
mhm <- ggplot(summary_data, aes(x = fct_reorder(Product, Percentage, .desc = TRUE), 
                                y = Percentage, 
                                fill = Product)) + 
  geom_col(width = 0.3) +
  geom_errorbar(aes(ymin = Percentage - SE, ymax = Percentage + SE), 
                width = 0.2, 
                color = "black") +
geom_text(aes(y = Percentage + SE + 2, 
               label = sprintf("%.1f%%", Percentage)),  
          size = 5, 
         fontface = "bold",
        vjust = 0,
       family = "Palatino Linotype") +  # Specify font family for text
  scale_y_continuous(limits = c(0, 100), 
                     breaks = seq(0, 100, 20),
                     expand = expansion(mult = c(0, 0.1))) +
  labs(x = "Type of health products ",
       y = "Percentage of respondents(%)") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 10, face = "bold", 
                              color = "black", family = "Palatino Linotype"),  
    axis.title.x = element_text(hjust = 0.5, size = 14, color = "black", 
                                face = "bold", family = "Palatino Linotype"),  
    axis.title.y = element_text(hjust = 0.5, size = 14, color = "black", 
                                face = "bold", family = "Palatino Linotype"),  
    axis.text.x = element_text(size = 12, color = "black", 
                               family = "Palatino Linotype"),  
    axis.text.y = element_text(size = 12, color = "black", 
                               family = "Palatino Linotype")   
  ) +
  guides(fill = "none") 
mhm

# Save the plot with embedded fonts
ggsave("products_revised.pdf",
       plot = mhm,
       width = 9, 
       height = 6,
       units = "in",
       device = cairo_pdf)  # Use Cairo PDF device which handles fonts better
#******************************************************
library(dplyr)
library(forcats)
library(dplyr)
library(forcats)
library(ggplot2)

# Step 1: Transform data
disease <- survey_df %>%
  select(starts_with("disease")) %>%
  mutate(across(everything(), ~ ifelse(. == 1, "Yes", "No"))) %>%
  mutate(across(everything(), as.factor))  # Convert all columns to factors

# Step 2: Reshape to long format
disease_long <- disease %>%
  pivot_longer(cols = everything(), names_to = "Disease", values_to = "Response")
# Convert to factor
disease_long$Disease <- as.factor(disease_long$Disease)

# Check factor levels
levels(disease_long$Disease)


# Step 3: Recoding disease names into Chicken and Pig categories
disease_modified <- disease_long %>%
  mutate(Disease2 = fct_recode(Disease,
                               "Chicken" = "disease_chicken_newcastle",
                               "Chicken" = "disease_chicken_infectious_bursal",
                               "Chicken" = "disease_chicken_coccidiosis",
                               "Chicken" = "disease_chicken_coryza",
                               "Chicken" = "disease_chicken_cholera",
                               "Chicken" = "diseases_chicken_fowl_pox",
                               "Chicken" = "disease_chicken_worms",
                               "Chicken" = "disease_chicken_parasites",
                               "Pig" = "disease_pig_diarrhea",
                               "Pig" = "disease_pig_swine_erysipelas",
                               "Pig" = "disease_pig_pneumonia",
                               "Pig" = "disease_pig_swine_dysentery",
                               "Pig" = "disease_pig_malnutrition",
                               "Pig" = "disease_pig_brucellosis",
                               "Pig" = "disease_pig_anthrax",
                               "Pig" = "disease_pig_scouring",
                               "Pig" = "disease_pig_foot_mouth",
                               "Pig" = "disease_pig_mange",
                               "Pig" = "disease_pigs_african_swine")) 





###################experience in handling different waste types

#chicken

chichen_disease<-survey_df |>
  select(disease_chicken_newcastle:disease_chicken_worms) |> 
  mutate(id=1:nrow(survey_df)) 


chicken_disease_long <- chichen_disease |> 
  pivot_longer(cols = -id, names_to = "Variable", values_to = "Response") 

chicken_disease_long<-chicken_disease_long |> 
mutate(animal="chicken") 


###########plot######################

yes_counts_percent <- chicken_disease_long %>%
  filter(!is.na(Response)) %>%  # Exclude NA values
  group_by(Variable, Response) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  ) |> 
  group_by(Variable) %>%
  mutate(
    Percent = 100 * Count / sum(Count)  # Calculate percentage
  ) %>%
  filter(Response == "1") |>  # Filter for "Yes" responses only
  ungroup() |> 
  mutate(Variable = as.factor(Variable)) 

yes_counts_percent<-yes_counts_percent |> 
 mutate(Variable=fct_recode(Variable,
                            "New castle"="disease_chicken_newcastle",
                            "Infectious bursal"="disease_chicken_infectious_bursal",
                            "Coccidiosis"="disease_chicken_coccidiosis",
                            "Corza"="disease_chicken_coryza",
                            "Cholera"="disease_chicken_cholera",
                            "Fowl Pox"="diseases_chicken_fowl_pox",
                            "Worms"="disease_chicken_worms",
                            "Parasites"="disease_chicken_parasites"))

# Plot the data
ggplot(yes_counts_percent, aes(x = reorder(Variable, -Percent), y = Percent, fill = Variable)) +
  geom_col( width = 0.65) +
  geom_text(aes(label = sprintf("%.2f%%", Percent)), 
            vjust = -0.5,
            size = 5, 
            fontface = "bold") +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 100, 10)) +
  labs(
    title = "Percentage of Respondents Who Have Experienced Chicken Diseases",
    x = "Diseases",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") + # Remove legend
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=14, color = "black"),
        axis.title.x=element_text(size=16,face="bold", color = "black"),
        axis.text.y = element_text( hjust = 1, size=14, color = "black"),
        axis.title.y=element_text(size=16,face="bold", color = "black"),
        plot.title =  element_text(size=17,face="bold", color = "black")) # Rotate x-axis labels

       
#pigs
pig_disease<-survey_df |>
  select(disease_pig_diarrhea:disease_pig_foot_mouth) |> 
  mutate(id=1:nrow(survey_df)) 


pig_disease_long <- pig_disease |> 
  pivot_longer(cols = -id, names_to = "Variable", values_to = "Response") 


###########plot######################

yes_counts_percent <- pig_disease_long %>%
  filter(!is.na(Response)) %>%  # Exclude NA values
  group_by(Variable, Response) %>%
  summarise(
    Count = n(),
    .groups = "drop"
  ) |> 
  group_by(Variable) %>%
  mutate(
    Percent = 100 * Count / sum(Count)  # Calculate percentage
  ) %>%
  filter(Response == "1") |>  # Filter for "Yes" responses only
  ungroup() |> 
  mutate(Variable = as.factor(Variable)) 

yes_counts_percent<-yes_counts_percent |> 
  mutate(Variable=fct_recode(Variable,
                            "Diarrhea"="disease_pig_diarrhea",
                            "Swine Erysipelas"="disease_pig_swine_erysipelas",
                            "Pneumonia"="disease_pig_pneumonia",
                            "Swine Dysentery"="disease_pig_swine_dysentery",
                            "Malnutrition"="disease_pig_malnutrition",
                            "Brucellosis"="disease_pig_brucellosis",
                            "Anthrax"="disease_pig_anthrax",
                            "Scouring"="disease_pig_scouring",
                            "Foot and Mouth"="disease_pig_foot_mouth",
                            "Mange"="disease_pig_mange",
                            "African Swine"="disease_pigs_african_swine"))
# Plot the data
ggplot(yes_counts_percent, aes(x = reorder(Variable, -Percent), y = Percent, fill = Variable)) +
  geom_col( width = 0.65) +
  geom_text(aes(label = sprintf("%.2f%%", Percent)), 
            vjust = -0.5,
            size = 5, 
            fontface = "bold") +
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 100, 20)) +
  labs(
title = "Percentage of Respondents Who Have Experienced Pig Diseases",
    x = "Diseases",
    y = "Percentage (%)"
  ) +
  theme_minimal() +
  theme(legend.position = "none") + # Remove legend
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size=14, color = "black"),
        axis.title.x=element_text(size=16,face="bold", color = "black"),
        axis.text.y = element_text( hjust = 1, size=14, color = "black"),
        axis.title.y=element_text(size=16,face="bold", color = "black"),
        plot.title =  element_text(size=17,face="bold", color = "black")) # Rotate x-axis labels

# Ensure necessary libraries are loaded
library(ggplot2)
library(treemapify)
library(dplyr)  # Ensure dplyr is loaded for pipes

# Select the relevant column
consult <- survey_df |> 
  select(consult_health)

# Summarize the data
summary <- consult |> 
  group_by(consult_health) |>
  summarise(Count = n()) |>
  mutate(percentage = 100 * Count / sum(Count))

# Create the treemap
ggplot(summary, aes(area = percentage, fill = consult_health, label = consult_health)) +
  geom_treemap(show.legend = FALSE) +
  geom_treemap_text(
    aes(label = paste0(consult_health, "\n(", sprintf("%.2f", percentage), "%)")),  
    place = "centre",  # Centers the text
    grow = FALSE       # Prevents text from zooming in
  )+
  labs(
    title = "Consultation with a Health Professional",

  ) +
  theme(plot.title = element_text(size = 17, face = "bold", color = "black", hjust = 0.5))




#########################################################where

amr<-survey_df |>
  select("misuse_amr")
library(tidyverse)

# Select relevant columns
where <- survey_df |> select(starts_with("where"))

# Reshape to long format
where_long <- where |> 
  pivot_longer(cols = where_purchase_veterinarian:where_purchase_other,
               names_to = "Variable", values_to = "Response")

# Total number of respondents
total_n <- nrow(where)  # assuming one row per respondent

# Summarize percentages and calculate SE
summary_where <- where_long %>%
  filter(!is.na(Response)) %>%
  group_by(Variable, Response) %>%
  summarise(Count = n(), .groups = "drop") %>%
  group_by(Variable) %>%
  mutate(Percent = 100 * Count / sum(Count)) %>%
  filter(Response == "1") %>%
  ungroup() %>%
  mutate(
    
    SE = sqrt((Percent / 100) * (1 - Percent / 100) / total_n) * 100
  )


# Rename the Variable names
summary_where <- summary_where |> 
  mutate(Variable = fct_recode(Variable,
                               "Agrovet" = "where_purchase_agrovet",
                               "Other" = "where_purchase_other",
                               "Other" = "where_purchase_vendors",
                               "Veterinarian" = "where_purchase_veterinarian"))

# Plot with error bars
ggplot(summary_where, aes(x = reorder(Variable, -Percent), y = Percent, fill = Variable)) +
  geom_col(width = 0.4) +
  geom_errorbar(aes(ymin = Percent - SE, ymax = Percent + SE), width = 0.2, color = "black") +
  geom_text(aes(y = Percent + SE + 1, label = sprintf("%.2f%%", Percent)), 
            size = 5, fontface = "bold") +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 100, 20)) +
  labs(
    title = "Percentage of responses showing different Antibiotic sources",
    x = "Source of Antibiotics",
    y = "Percentages (%)"
  ) +
  theme_classic() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1, size = 14, color = "black"),
    axis.title.x = element_text(size = 16, face = "bold", color = "black"),
    axis.text.y = element_text(hjust = 1, size = 14, color = "black"),
    axis.title.y = element_text(size = 16, face = "bold", color = "black"),
    plot.title = element_text(size = 17, face = "bold", color = "black")
  )


# Ensure necessary libraries are loaded
library(dplyr)
library(gt)

# Select the relevant column (misuse_amr)
amr <- survey_df |> 
  select(misuse_amr)

# Assuming we are comparing misuse_amr with another categorical variable like 'consult_health'
data <- survey_df |> 
  select(misuse_amr, consult_health)  # Replace consult_health with your actual variable

# Create a contingency table
contingency_table <- table(data$misuse_amr)

# Perform the Chi-square test
chi_square_result <- chisq.test(contingency_table)

# Create a summary of the Chi-square test results
chi2_summary <- data.frame(
  "Chi-Square Statistic" = chi_square_result$statistic,
  "Degrees of Freedom" = chi_square_result$parameter,
  "p-value" = chi_square_result$p.value
)

# Format the results using gt
chi2_gt <- chi2_summary |> 
  gt() |> 
  tab_header(
    title = "Chi-Square Test Results: misuse_amr")
  

# Display the formatted gt table
chi2_gt













library(dplyr)
library(tidyr)
library(gt)

# Select relevant columns
dataf <- survey_df %>%
  select(misuse_amr,consult_veterinan)

# Summarize response frequencies
freq_table <- dataf %>%
  summarise(
    across(everything(), list(
      Yes = ~sum(. == "Yes", na.rm = TRUE),
      No = ~sum(. == "No", na.rm = TRUE)
    ))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Response"),
    names_pattern = "(.*)_(Yes|No)", 
    values_to = "Count"
  ) %>%
  pivot_wider(
    names_from = "Response", 
    values_from = "Count", 
    values_fill = list(Count = 0)
  ) %>%
  mutate(
    Total = Yes + No,  
    Yes_Percent = round((Yes / Total) * 100, 1),  
    No_Percent = round((No / Total) * 100, 1)
  ) %>%
  select(Variable, Yes, Yes_Percent, No, No_Percent, Total)  

# Perform chi-square test for each variable based on percentages
freq_table <- freq_table %>%
  rowwise() %>%
  mutate(
    p_value = tryCatch(
      chisq.test(matrix(c(Yes, No), nrow = 2))$p.value, 
      error = function(e) NA_real_  
    )
  ) %>%
  ungroup()

# Generate a well-formatted gt table
gt_table <- freq_table %>%
  gt() %>%
  fmt_number(columns = c(Yes_Percent, No_Percent), decimals = 1) %>%
  fmt_number(columns = p_value, decimals = 3) %>%
  cols_label(
    Variable = "Variable",
    Yes = "Yes (N)",
    Yes_Percent = "Yes (%)",
    No = "No (N)",
    No_Percent = "No (%)",
    Total = "Total (N)",
    p_value = "p-value"
  ) %>%
  tab_header(
    title = "Survey Response Summary with Chi-Square Test"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  )

# Print the formatted table
gt_table

# Save table
gtsave(gt_table, filename = "guidelines_gt2.docx")

library(dplyr)
library(tidyr)
library(gt)

# Select relevant columns
dataf <- survey_df %>%
  select(misuse_amr, consult_veterinan)

# Summarize response frequencies (including "IDK")
freq_table <- dataf %>%
  summarise(
    across(everything(), list(
      Yes = ~sum(. == "Yes", na.rm = TRUE),
      No = ~sum(. == "No", na.rm = TRUE),
      IDK = ~sum(. == "I Don't Know", na.rm = TRUE)
    ))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Response"),
    names_pattern = "(.*)_(Yes|No|IDK)", 
    values_to = "Count"
  ) %>%
  pivot_wider(
    names_from = "Response", 
    values_from = "Count", 
    values_fill = list(Count = 0)
  ) %>%
  mutate(
    Total = Yes + No + IDK,  
    Yes_Percent = round((Yes / Total) * 100, 1),  
    No_Percent = round((No / Total) * 100, 1),
    IDK_Percent = round((IDK / Total) * 100, 1)  
  ) %>%
  select(Variable, Yes, Yes_Percent, No, No_Percent, IDK, IDK_Percent, Total)  

# Perform chi-square test including "IDK"
freq_table <- freq_table %>%
  rowwise() %>%
  mutate(
    p_value = tryCatch(
      chisq.test(c(Yes, No, IDK))$p.value, 
      error = function(e) NA_real_  
    )
  ) %>%
  ungroup()

# Generate a well-formatted gt table
gt_table <- freq_table %>%
  gt() %>%
  fmt_number(columns = c(Yes_Percent, No_Percent, IDK_Percent), decimals = 1) %>%
  fmt_number(columns = p_value, decimals = 3) %>%
  cols_label(
    Variable = "Variable",
    Yes = "Yes (N)",
    Yes_Percent = "Yes (%)",
    No = "No (N)",
    No_Percent = "No (%)",
    IDK = "IDK (N)",
    IDK_Percent = "IDK (%)",
    Total = "Total (N)",
    p_value = "p-value"
  ) %>%
  tab_header(
    title = "Survey Response Summary with Chi-Square Test"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  )

# Print the formatted table
gt_table

# Save table
gtsave(gt_table, filename = "guidelines_gtidk.docx")


#######################
# Select relevant columns
dataf <- survey_df %>%
  select(misuse_amr,consult_veterinan)

# Summarize response frequencies
freq_table <- dataf %>%
  summarise(
    across(everything(), list(
      Yes = ~sum(. == "Yes", na.rm = TRUE),
      No = ~sum(. == "No", na.rm = TRUE)
    ))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Response"),
    names_pattern = "(.*)_(Yes|No)", 
    values_to = "Count"
  ) %>%
  pivot_wider(
    names_from = "Response", 
    values_from = "Count", 
    values_fill = list(Count = 0)
  ) %>%
  mutate(
    Total = Yes + No,  
    Yes_Percent = round((Yes / Total) * 100, 1),  
    No_Percent = round((No / Total) * 100, 1)
  ) %>%
  select(Variable, Yes, Yes_Percent, No, No_Percent, Total)  

# Perform chi-square test for each variable based on percentages
freq_table <- freq_table %>%
  rowwise() %>%
  mutate(
    p_value = tryCatch(
      chisq.test(matrix(c(Yes, No), nrow = 2))$p.value, 
      error = function(e) NA_real_  
    )
  ) %>%
  ungroup()

# Generate a well-formatted gt table
gt_table <- freq_table %>%
  gt() %>%
  fmt_number(columns = c(Yes_Percent, No_Percent), decimals = 1) %>%
  fmt_number(columns = p_value, decimals = 3) %>%
  cols_label(
    Variable = "Variable",
    Yes = "Yes (N)",
    Yes_Percent = "Yes (%)",
    No = "No (N)",
    No_Percent = "No (%)",
    Total = "Total (N)",
    p_value = "p-value"
  ) %>%
  tab_header(
    title = "Survey Response Summary with Chi-Square Test"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  )

# Print the formatted table
gt_table

# Save table
gtsave(gt_table, filename = "guidelines_gt2.docx")

library(dplyr)
library(tidyr)
library(gt)

# Select relevant columns
dataf <- survey_df %>%
  select(misuse_amr, consult_veterinan)

# Summarize response frequencies (including "IDK")
freq_table <- dataf %>%
  summarise(
    across(everything(), list(
      Yes = ~sum(. == "Yes", na.rm = TRUE),
      No = ~sum(. == "No", na.rm = TRUE),
      IDK = ~sum(. == "I Don't Know", na.rm = TRUE)
    ))
  ) %>%
  pivot_longer(
    cols = everything(),
    names_to = c("Variable", "Response"),
    names_pattern = "(.*)_(Yes|No|IDK)", 
    values_to = "Count"
  ) %>%
  pivot_wider(
    names_from = "Response", 
    values_from = "Count", 
    values_fill = list(Count = 0)
  ) %>%
  mutate(
    Total = Yes + No + IDK,  
    Yes_Percent = round((Yes / Total) * 100, 1),  
    No_Percent = round((No / Total) * 100, 1),
    IDK_Percent = round((IDK / Total) * 100, 1)  
  ) %>%
  select(Variable, Yes, Yes_Percent, No, No_Percent, IDK, IDK_Percent, Total)  

# Perform chi-square test including "IDK"
freq_table <- freq_table %>%
  rowwise() %>%
  mutate(
    p_value = tryCatch(
      chisq.test(c(Yes, No, IDK))$p.value, 
      error = function(e) NA_real_  
    )
  ) %>%
  ungroup()

# Generate a well-formatted gt table
gt_table <- freq_table %>%
  gt() %>%
  fmt_number(columns = c(Yes_Percent, No_Percent, IDK_Percent), decimals = 1) %>%
  fmt_number(columns = p_value, decimals = 3) %>%
  cols_label(
    Variable = "Variable",
    Yes = "Yes (N)",
    Yes_Percent = "Yes (%)",
    No = "No (N)",
    No_Percent = "No (%)",
    IDK = "IDK (N)",
    IDK_Percent = "IDK (%)",
    Total = "Total (N)",
    p_value = "p-value"
  ) %>%
  tab_header(
    title = "Survey Response Summary with Chi-Square Test"
  ) %>%
  cols_align(
    align = "center",
    columns = everything()
  )

# Print the formatted table
gt_table
####################

library(dplyr)
library(gtsummary)

# Sample dataset (replace this with your actual dataset)
df <- data.frame(
  Education = c("Primary", "Secondary", "Tertiary", "Primary", "Secondary"),
  yrs_farming = c(5, 10, 15, 20, 25),
  purpose1 = c(1, 0, 1, 1, 0),
  purpose2 = c(0, 1, 0, 1, 1)
)

#import data

survey_df<- read_csv(here::here("data/raw/Antibiotic_Usage_Practices_among_Pig_and_Poultry_Farmers_-_all_versions.csv")) |> 
  clean_names() |> 
  mutate_if(is.character, as_factor) 

survey_df<- survey_df |> 
  mutate(
    years_farming_cat = case_when(
      years_farming >= 1 & years_farming <= 5 ~ "1-5",
      years_farming >= 6 & years_farming <= 10 ~ "6-10",
      years_farming >= 11 & years_farming <= 15 ~ "11-15",
      TRUE ~ "16+"
    )
  )

# Convert binary responses (1/0) to "YES"/"NO"
products <- survey_df  |> 
  select(starts_with("where_purchase_"),education,years_farming_cat) |>
mutate(across(starts_with("where_purchase_"),~ ifelse(. == 1, "yes", "no")))

# Reshaping data for multiple response handling
products_long <- products %>%
 pivot_longer(cols = starts_with("where_purchase_"),
           names_to = "products", values_to = "Response")

products_yes <- products_long %>%
filter(Response == "yes") |> 
  select(-Response)

  
  
  
 
  # Keep only "yes" responses


# Generate summary table with conditional testing
p<-products_yes |>
  select(education, years_farming_cat, products) |>  # Keep necessary columns
  tbl_summary(
    by = products,  # Group by 'cons' (assuming categorical)
    missing = "no"
  ) |> 
  add_p(pvalue_fun = function(x) style_number(x, digits = 3)) |> 
  as_gt() 

as_flex_table()
  save_as_docx(path = "~/gitrepos/amon-must/products final.docx")

gt::gtsave(p, "where do you get anti.docx")


#############import data

survey_df <- read_csv(here::here("data/raw/info-source.csv")) |> 
  clean_names() |> 
  mutate_if(is.character, as_factor) |> 
  mutate(id = row_number())  # Assigns a unique ID to each row

#####create subset
info<-survey_df |> 
  select(starts_with("info_"),id) |> 
  mutate(across(starts_with("info_"), ~ ifelse(. == 1, "Yes", "No")))

####pivot and recode
info_long <- info |> 
  pivot_longer(
    cols = starts_with("info_"),
    names_to = "Source",
    values_to = "Response"
  )|> 
  mutate(Source = as.factor(Source)) |> 
  mutate(Source = fct_recode(Source,
                              "Exension officers" = "info_agricultural_extension_officers", 
                              "Agrovet staff" = "info_agrovet_shop_staff", 
                              "Family and friends" = "info_family_or_friends",
                              "Online resources" = "info_online_resources_websites_social_media",
                              "Farmers" = "info_other_farmers", 
                              "Other" = "info_other_please_specify", 
                              "TV/Radio" = "info_television_or_radio",
                              "Veterinarian" = "info_veterinarian")) 

#######plotting
 info_long |> 
  filter(Response == "Yes") |> 
  group_by(Source) |> 
  summarise(
    Count = n(),  
    Percentage = (Count / n_distinct(info_long$id)) * 100
  ) |> 
ggplot(aes(x = fct_reorder(Source, Percentage, .desc = TRUE), y = Percentage, fill = Source)) + 
  geom_col(width = 0.6) +
  geom_text(aes(label = sprintf("%.2f%%", Percentage)),  
            vjust = -0.5, size = 6, fontface = "bold") +  
  scale_y_continuous(limits = c(0, 70), breaks = seq(0, 100, 20)) +  # Ensure Y-axis goes up to 100%
  labs(title = "",
       x = "Sources",
       y = "Percentage (%)") +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),  # Black title
    axis.title.x = element_text(hjust = 0.5, size = 14, color = "black",face = "bold"),  # Black X-axis title
    axis.title.y = element_text(hjust = 0.5, size = 14, color = "black"),  # Black Y-axis title
    axis.text.x = element_text(size = 13, color = "black", angle = 45, hjust = 1),  # Black X-axis text
    axis.text.y = element_text(size = 13, color = "black")   # Black Y-axis text
  ) +
  guides(fill = "none") 


 ##################determine
 #####create subset
 how<-survey_df |> 
   select(starts_with("how_determine"),id) |> 
   mutate(across(starts_with("how_determine"), ~ ifelse(. == 1, "Yes", "No")))
 
 ####pivot and recode
 how_long <- how |> 
   pivot_longer(
     cols = starts_with("how_determine"),
     names_to = "how",
     values_to = "Response"
   )|> 
   mutate(how = as.factor(how)) |> 
   mutate(how = fct_recode(how,
                              "Agrovet staff advice" = "how_determine_agrovet_shop_staff_advice", 
                              "Don't know" = "how_determine_i_dont_know", 
                              "Manufacture's instructions" = "how_determine_manufacturers_instructions",
                              "Farmer's advice" = "how_determine_other_farmers_advice",
                              "Personal Experience" = "how_determine_personal_experience", 
                              "Vertenary Advice" = "how_determine_veterinary_advice" )) 
 
 #######plotting
 how_long |> 
   filter(Response == "Yes") |> 
   group_by(how) |> 
   summarise(
     Count = n(),  
     Percentage = (Count / n_distinct(info_long$id)) * 100
   ) |> 
   ggplot(aes(x = fct_reorder(how, Percentage, .desc = TRUE), y = Percentage, fill = how)) + 
   geom_col(width = 0.6) +
   geom_text(aes(label = sprintf("%.2f%%", Percentage)),  
             vjust = -0.5, size = 6, fontface = "bold") +  
   scale_y_continuous(limits = c(0, 70), breaks = seq(0, 100, 20)) +  # Ensure Y-axis goes up to 100%
   labs(title = "",
        x = "Source of information",
        y = "Percentage (%)") +
   theme_classic() +
   theme(
     plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),  # Black title
     axis.title.x = element_text(hjust = 0.5, size = 14, color = "black",face = "bold"),  # Black X-axis title
     axis.title.y = element_text(hjust = 0.5, size = 14, color = "black"),  # Black Y-axis title
     axis.text.x = element_text(size = 13, color = "black", angle = 45, hjust = 1),  # Black X-axis text
     axis.text.y = element_text(size = 13, color = "black")   # Black Y-axis text
   ) +
   guides(fill = "none") 
 
 ####################how antibiotic
 
 #####create subset
 anti<-survey_df |> 
   select(starts_with("how_antibiotic"),id) |> 
   mutate(across(starts_with("how_antibiotic"), ~ ifelse(. == 1, "Yes", "No")))
 
 ####pivot and recode
 anti_long <- anti |> 
   pivot_longer(
     cols = starts_with("how_antibiotic"),
     names_to = "how",
     values_to = "Response"
   )|> 
   mutate(how = as.factor(how)) |> 
   mutate(how = fct_recode(how,
                           "Veterinary shop attendant" = "how_antibiotic_veterinary_shop_attendant", 
                           "Don't know" = "how_antibiotic_i_do_not_know", 
                           "Farmer's recommendations" = "how_antibiotic_recommendations_from_other_farmers",
                           "Personal Experience" = "how_antibiotic_personal_experience", 
                           "Consultat veterinarian " = "how_antibiotic_consultation_with_a_veterinarian" )) 
 
 #######plotting
 anti_long |> 
   filter(Response == "Yes") |> 
   group_by(how) |> 
   summarise(
     Count = n(),  
     Percentage = (Count / n_distinct(info_long$id)) * 100
   ) |> 
   ggplot(aes(x = fct_reorder(how, Percentage, .desc = TRUE), y = Percentage, fill = how)) + 
   geom_col(width = 0.6) +
   geom_text(aes(label = sprintf("%.2f%%", Percentage)),  
             vjust = -0.5, size = 6, fontface = "bold") +  
   scale_y_continuous(limits = c(0, 80), breaks = seq(0, 100, 20)) +  # Ensure Y-axis goes up to 100%
   labs(title = "",
        x = "Source of information",
        y = "Percentage (%)") +
   theme_classic() +
   theme(
     plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),  # Black title
     axis.title.x = element_text(hjust = 0.5, size = 14, color = "black",face = "bold"),  # Black X-axis title
     axis.title.y = element_text(hjust = 0.5, size = 14, color = "black"),  # Black Y-axis title
     axis.text.x = element_text(size = 13, color = "black", angle = 45, hjust = 1),  # Black X-axis text
     axis.text.y = element_text(size = 13, color = "black")   # Black Y-axis text
   ) +
   guides(fill = "none") 
 
 
###############products used
 
 #####create subset
 pro<-survey_df |> 
   select(starts_with("product_use_"),id) |> 
   mutate(across(starts_with("product_use_"), ~ ifelse(. == 1, "Yes", "No")))
 
 ####pivot and recode
 anti_long <- pro |> 
   pivot_longer(
     cols = starts_with("product_use_"),
     names_to = "how",
     values_to = "Response"
   )|> 
   mutate(how = as.factor(how)) |> 
   mutate(how = fct_recode(how,
                           "Antibiotics" = "product_use_antibiotics", 
                           "Herbal remedies" = "product_use_herbal_remedies", 
                           "None" = "product_use_none",
                           "Vaccines " = "product_use_vaccines" )) 
 
 #######plotting
 anti_long |> 
   filter(Response == "Yes") |> 
   group_by(how) |> 
   summarise(
     Count = n(),  
     Percentage = (Count / n_distinct(anti_long$id)) * 100
   ) |> 
   ggplot(aes(x = fct_reorder(how, Percentage, .desc = TRUE), y = Percentage, fill = how)) + 
   geom_col(width = 0.6) +
   geom_text(aes(label = sprintf("%.2f%%", Percentage)),  
             vjust = -0.5, size = 6, fontface = "bold") +  
   scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +  # Ensure Y-axis goes up to 100%
   labs(title = "Poduct categories used in farms",
        x = "Products used",
        y = "Percentage (%)") +
   theme_classic() +
   theme(
     plot.title = element_text(hjust = 0.5, size = 16, face = "bold", color = "black"),  # Black title
     axis.title.x = element_text(hjust = 0.5, size = 14, color = "black",face = "bold"),  # Black X-axis title
     axis.title.y = element_text(hjust = 0.5, size = 14, color = "black"),  # Black Y-axis title
     axis.text.x = element_text(size = 13, color = "black", angle = 45, hjust = 1),  # Black X-axis text
     axis.text.y = element_text(size = 13, color = "black")   # Black Y-axis text
   ) +
   guides(fill = "none") 
 
 
 
 
 ################here diseases
 
 
 
 
 
 
 where <- survey_df |> 
   select(starts_with("disease_chicken"))
 
 
 # Reshape to long format
 where_long <- where |> 
   pivot_longer(
     cols = starts_with("disease_chicken"),
     names_to = "Variable",
     values_to = "Response"
   )
 
 
 # Total number of respondents
 total_n <- nrow(where)  # assuming one row per respondent
 
 # Summarize percentages and calculate SE
 summary_where <- where_long %>%
   filter(!is.na(Response)) %>%
   group_by(Variable, Response) %>%
   summarise(Count = n(), .groups = "drop") %>%
   group_by(Variable) %>%
   mutate(Percent = 100 * Count / sum(Count)) %>%
   filter(Response == "1") %>%
   ungroup() %>%
   mutate(
     
     SE = sqrt((Percent / 100) * (1 - Percent / 100) / total_n) * 100
   )
 
 where_long$Variable <- as.factor(where_long$Variable)  # Ensure Variable is a factor
 levels(where_long$Variable)
 
 # Rename the Variable names
 summary_where <- summary_where |> 
   mutate(Variable = fct_recode(Variable,
                                "Cholera" = "disease_chicken_cholera",
                                "Coryza" = "disease_chicken_coryza",
                                "New Caste" = "disease_chicken_newcastle" ,
                               "Worms" ="disease_chicken_worms",
                                "Coccidiosis" = "disease_chicken_coccidiosis",
                               "Infectious bursal" ="disease_chicken_infectious_bursal",
                                
                               " Parasites"="disease_chicken_parasites" ))
 
 # Plot with error bars
 cd<-ggplot(summary_where, aes(x = reorder(Variable, -Percent), y = Percent, fill = Variable)) +
   geom_col(width = 0.4) +
   geom_errorbar(aes(ymin = Percent - SE, ymax = Percent + SE), width = 0.2, color = "black") +
   geom_text(aes(y = Percent + SE + 1, label = sprintf("%.1f%%", Percent)), 
             size = 5, fontface = "bold", family = "Palatino Linotype") +
   scale_y_continuous(limits = c(0, 70), breaks = seq(0, 100, 20)) +
   labs(

     x = "Poultry diseases",
     y = "Percentage of respondents using antibiotics (%)"
   ) +
   theme_classic() +
   theme(
     legend.position = "none",
     axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black", family = "Palatino Linotype"),
     axis.title.x = element_text(size = 14, face = "bold", color = "black", family = "Palatino Linotype"),
     axis.text.y = element_text(hjust = 1, size = 12, color = "black", family = "Palatino Linotype"),
     axis.title.y = element_text(size = 14, face = "bold", color = "black", family = "Palatino Linotype"),
     plot.title = element_text(size = 17, face = "bold", color = "black", family = "Palatino Linotype")
   )
 
 cd
 
 # Save the plot with embedded fonts
 ggsave("Poultry diseases.pdf",
        plot = cd,
        width = 9, 
        height = 6,
        units = "in",
        device = cairo_pdf)  # Use Cairo PDF device which handles fonts better
 #******************************************************
 
 
 #########pig
 where <- survey_df |> 
   select(starts_with("disease_pig"))
 
 
 # Reshape to long format
 where_long <- where |> 
   pivot_longer(
     cols = starts_with("disease_pig"),
     names_to = "Variable",
     values_to = "Response"
   )
 
 
 # Total number of respondents
 total_n <- nrow(where)  # assuming one row per respondent
 
 # Summarize percentages and calculate SE
 summary_where <- where_long %>%
   filter(!is.na(Response)) %>%
   group_by(Variable, Response) %>%
   summarise(Count = n(), .groups = "drop") %>%
   group_by(Variable) %>%
   mutate(Percent = 100 * Count / sum(Count)) %>%
   filter(Response == "1") %>%
   ungroup() %>%
   mutate(
     
     SE = sqrt((Percent / 100) * (1 - Percent / 100) / total_n) * 100
   )
 
 where_long$Variable <- as.factor(where_long$Variable)  # Ensure Variable is a factor
 levels(where_long$Variable)
 
 # Rename the Variable names
 summary_where <- summary_where |> 
   mutate(Variable = fct_recode(Variable,
                                "Diarrhoea" = "disease_pig_diarrhea"  ,
                                "Bucellosis" ="disease_pig_brucellosis" ,
                                "Foot and Mouth" =  "disease_pig_foot_mouth" ,
                                "Scouring" ="disease_pig_scouring",
                                "Mange" = "disease_pig_mange",
                             
                                "Erysipelas" ="disease_pig_swine_erysipelas",
                                
                               
                                "Anthrax"="disease_pig_anthrax",
                                "Malnutrition"="disease_pig_malnutrition",
                                "Pneumonia"="disease_pig_pneumonia",
                                "Swine dysentry"="disease_pig_swine_dysentery",
                                "African swine"="disease_pigs_african_swine"))
 
 # Plot with error bars
pd<- ggplot(summary_where, aes(x = reorder(Variable, -Percent), y = Percent, fill = Variable)) +
   geom_col(width = 0.45) +
   geom_errorbar(aes(ymin = Percent - SE, ymax = Percent + SE), width = 0.2, color = "black") +
   geom_text(aes(y = Percent + SE + 1, label = sprintf("%.1f%%", Percent)), 
             size = 5, fontface = "bold", family = "Palatino Linotype") +
   scale_y_continuous(limits = c(0, 85), breaks = seq(0, 100, 20)) +
   labs(
    
     x = "Pig diseases",
     y = "Percentage of respondents using antibiotics (%)"
   ) +
   theme_classic() +
   theme(
     legend.position = "none",
     axis.text.x = element_text(angle = 45, hjust = 1, size = 12, color = "black", family = "Palatino Linotype"),
     axis.title.x = element_text(size = 14, face = "bold", color = "black", family = "Palatino Linotype"),
     axis.text.y = element_text(hjust = 1, size = 12, color = "black", family = "Palatino Linotype"),
     axis.title.y = element_text(size = 14, face = "bold", color = "black", family = "Palatino Linotype"),
     plot.title = element_text(size = 17, face = "bold", color = "black", family = "Palatino Linotype")
   )
 
pd

# Save the plot with embedded fonts
ggsave("Pig diseases.pdf",
       plot = pd,
       width = 9, 
       height = 6,
       units = "in",
       device = cairo_pdf)  # Use Cairo PDF device which handles fonts better
 ############### Microbial Correlation Analysis ###############
 # Load required libraries
 library(tidyverse)
 library(broom)
 library(gt)
 
 # Clean the data
 microbiology <- microbiology |> 
   mutate(
     sample = str_trim(sample),
     sample = case_when(
       str_detect(sample, regex("soil", ignore_case = TRUE)) ~ "Soil",
       str_detect(sample, regex("manure", ignore_case = TRUE)) ~ "Manure",
       str_detect(sample, regex("vegetable", ignore_case = TRUE)) ~ "Vegetable",
       TRUE ~ sample
     ),
     microbe = str_trim(microbe),
     microbe = case_when(
       str_detect(microbe, regex("E. coli", ignore_case = TRUE)) ~ "E. coli",
       str_detect(microbe, regex("Klebsiella", ignore_case = TRUE)) ~ "Klebsiella pneumoniae",
       TRUE ~ microbe
     )
   ) %>%
   filter(sample %in% c("Manure", "Soil", "Vegetable"))
 
 # Aggregate: mean inhibition per farm/sample/microbe
 agg_data <- microbiology %>%
   group_by(farm, microbe, sample) %>%
   summarise(
     mean_inhibition = mean(inhibition, na.rm = TRUE),
     .groups = "drop"
   )
 
 # Reshape to wide format
 wide_data <- agg_data %>%
   pivot_wider(
     names_from = sample,
     values_from = mean_inhibition
   )
 
 # Safe regression function
 safe_regression <- function(df) {
   complete_df <- df %>% 
     select(Manure, Soil, Vegetable) %>%
     drop_na()
   
   n_obs <- nrow(complete_df)
   
   if (n_obs >= 2) {  # allow regression with at least 2 complete observations
     model <- tryCatch(
       lm(Vegetable ~ Manure + Soil, data = complete_df),
       error = function(e) NULL
     )
     
     if (!is.null(model)) {
       tidy(model) %>% 
         mutate(n_obs = n_obs, .before = term)
     } else {
       tibble(term = c("(Intercept)", "Manure", "Soil"), 
              n_obs = n_obs, estimate = NA_real_, 
              std.error = NA_real_, statistic = NA_real_, 
              p.value = NA_real_)
     }
   } else {
     tibble(term = c("(Intercept)", "Manure", "Soil"), 
            n_obs = n_obs, estimate = NA_real_, 
            std.error = NA_real_, statistic = NA_real_, 
            p.value = NA_real_)
   }
 }
 
 # Apply regression by microbe
 results <- wide_data %>%
   group_by(microbe) %>%
   group_modify(~ safe_regression(.x)) %>%
   ungroup()
 
 # Clean up results for display
 results_clean <- results %>%
   select(microbe, term, estimate, std.error, p.value, n_obs) %>%
   mutate(
     significance = case_when(
       p.value < 0.001 ~ "***",
       p.value < 0.01 ~ "**",
       p.value < 0.05 ~ "*",
       TRUE ~ ""
     )
   ) %>%
   arrange(microbe, term)
 
 # Format using gt
 results_clean %>%
   gt() %>%
   fmt_number(columns = vars(estimate, std.error, p.value), decimals = 3) %>%
   tab_header(
     title = "Regression Results: Association between Manure, Soil and Vegetable"
   )
 
 
 # Diagnostic table: how many observations per microbe and sample
 diagnostic_table <- microbiology %>%
   group_by(microbe, sample) %>%
   summarise(n = n(), .groups = "drop") %>%
   pivot_wider(names_from = sample, values_from = n, values_fill = 0)
 
 # Show diagnostic table
 diagnostic_table %>% 
   gt() %>%
   tab_header(
     title = "Available Data Per Microbe and Sample"
   )

 
 # Focus only on analyzable microbes
 analyzable_microbes <- c("E. coli", "Klebsiella pneumoniae")
 
 
 
 # Filter and aggregate data
 agg_data <- microbiology %>%
   filter(microbe %in% analyzable_microbes) %>%
   group_by(farm, microbe, sample) %>%
   summarise(
     mean_inhibition = mean(inhibition, na.rm = TRUE),
     .groups = "drop"
   )
 
 # Reshape and clean
 wide_data <- agg_data %>%
   pivot_wider(names_from = sample, values_from = mean_inhibition) %>%
   filter(!is.na(Manure), !is.na(Soil), !is.na(Vegetable))
 
 # Run regressions
 results <- wide_data %>%
   group_by(microbe) %>%
   summarise(
     n_farms = n(),
     intercept = coef(lm(Vegetable ~ Manure + Soil))[1],
     manure_coef = coef(lm(Vegetable ~ Manure + Soil))[2],
     soil_coef = coef(lm(Vegetable ~ Manure + Soil))[3],
     manure_p = summary(lm(Vegetable ~ Manure + Soil))$coefficients[2,4],
     soil_p = summary(lm(Vegetable ~ Manure + Soil))$coefficients[3,4]
   ) %>%
   mutate(across(where(is.numeric), round, 3))
 
 # Format results
 results %>%
   select(microbe, n_farms, manure_coef, manure_p, soil_coef, soil_p) %>%
   gt() %>%
   tab_header(
     title = "Microbial Transmission Pathways",
     subtitle = "Regression of Vegetable Inhibition on Manure and Soil Content"
   ) %>%
   cols_label(
     microbe = "Microorganism",
     n_farms = "Farms",
     manure_coef = html("Manure β<br>(95% CI)"),
     manure_p = "Manure p-value",
     soil_coef = html("Soil β<br>(95% CI)"),
     soil_p = "Soil p-value"
   ) %>%
   fmt_number(columns = c(manure_coef, soil_coef), decimals = 3) %>%
   fmt_scientific(columns = c(manure_p, soil_p), decimals = 3) %>%
   tab_style(
     style = cell_fill(color = "lightgreen"),
     locations = cells_body(
       columns = manure_p,
       rows = manure_p < 0.05
     )
   ) %>%
   tab_style(
     style = cell_fill(color = "lightgreen"),
     locations = cells_body(
       columns = soil_p,
       rows = soil_p < 0.05
     )
   )
 # Pairwise association: Manure vs Vegetable
 pairwise_results <- agg_data %>%
   filter(sample %in% c("Soil", "Vegetable")) %>%
   pivot_wider(names_from = sample, values_from = mean_inhibition) %>%
   group_by(microbe) %>%
   group_modify(~ {
     df <- drop_na(.x, Soil, Vegetable)
     n_obs <- nrow(df)
     if (n_obs >= 2) {
       model <- lm(Vegetable ~ Soil, data = df)
       tidy(model) %>% mutate(n_obs = n_obs, .before = term)
     } else {
       tibble(term = "(Intercept)", estimate = NA, std.error = NA, statistic = NA, p.value = NA, n_obs = n_obs)
     }
   }) %>%
   ungroup()
 
 # Display pairwise result using gt
 pairwise_results %>%
   select(microbe, term, estimate, std.error, p.value, n_obs) %>%
   gt() %>%
   tab_header(
     title = "Pairwise Regression: Vegetable ~ Soil"
   )

# Antibiotics used --------------------------------------------------------
 library(tidyverse)
 
 # Define antibiotic abbreviation mapping
 abx_mapping <- c(
   "amoxicillin" = "AMOX",
   "colistin" = "COL",
   "doxycycline" = "DOX",
   "doxycycline1" = "DOX",
   "doxycycline2" = "DOX",
   "enrofloxacin_69" = "ENR",
   "enrofloxacin_84" = "ENR",
   "gentamicin" = "GEN",
   "other" = "Other",
   "oxytetracycline_66" = "OXY",
   "oxytetracycline_78" = "OXY",
   "oxytetracycline_87" = "OXY",
   "oxytetracycline1" = "OXY",
   "oxytetracycline2" = "OXY",
   "oxytetracycline3" = "OXY",
   "penicillin" = "PEN",
   "penicillin1" = "PEN",
   "penicillin2" = "PEN",
   "sulfadiazine" = "SDZ",
   "sulfamethoxazole1" = "TMX",
   "sulfamethoxazole2" = "TMX",
   "trimethoprim_sulfamethoxazole" = "TMX",
   "tylosin" = "TYL"
 )
 
 # Transform and aggregate data
 products_long <- survey_df |> 
   select(id, starts_with("used_")) |> 
   pivot_longer(
     cols = -id,
     names_to = "OriginalProduct",
     values_to = "used"
   ) |> 
   mutate(
     # Remove 'used_' prefix
     Product = str_remove(OriginalProduct, "^used_"),
     # Map to antibiotic abbreviations
     Antibiotic = recode(Product, !!!abx_mapping)
   ) |> 
   # Collapse multiple entries for same antibiotic per farm
   group_by(id, Antibiotic) |> 
   summarize(
     Used = as.integer(any(used == 1)),
     .groups = "drop"
   ) |> 
   mutate(Response = if_else(Used == 1, "Yes", "No"))
 
 # Calculate total farms
 total_n <- n_distinct(products_long$id)
 
 # Calculate summary statistics
 summary_data <- products_long |> 
   filter(Response == "Yes") |> 
   group_by(Antibiotic) |> 
   summarise(
     Count = n(),
     Percentage = (Count / total_n) * 100,
     SE = sqrt((Percentage/100) * (1 - Percentage/100) / total_n) * 100,
     .groups = 'drop'
   ) |> 
   filter(Percentage >= 1)  # remove rare categories
 
 # Generate plot with abbreviations
 anti<-ggplot(summary_data, aes(x = fct_reorder(Antibiotic, Percentage, .desc = TRUE), 
                          y = Percentage, 
                          fill = Antibiotic)) + 
   geom_col(width = 0.5) +
   geom_errorbar(aes(ymin = pmax(0, Percentage - SE), 
                     ymax = pmin(100, Percentage + SE)),
                 width = 0.5, color = "black") +
   geom_text(aes(y = pmin(100, Percentage + SE) + 2, 
                 label = sprintf("%.1f%%", Percentage)),
             size = 5, fontface = "bold", vjust = 0, family = "Palatino Linotype") +  
   scale_y_continuous(limits = c(0, 100), breaks = seq(0, 100, 20)) +  
   labs(x = "Antibiotic classes",
        y = " Percentage of respondents using each antibiotic class (%)") +
   theme_classic() +
   theme(
     plot.title = element_text(hjust = 0.5, size = 16, face = "bold", family = "Palatino Linotype"),
     axis.title = element_text(size = 14, colour="black",face = "bold", family = "Palatino Linotype"),
     axis.text.y = element_text(size = 12, color = "black",face = "bold", family = "Palatino Linotype"),
     axis.text.x = element_text(colour="black",face="bold",angle = 45, hjust = 1, vjust = 1, size = 12, family = "Palatino Linotype")
   ) +
   guides(fill = "none")
 
 anti
 
 # Save the plot with embedded fonts
 ggsave("Antibiptics.pdf",
        plot = anti,
        width = 9, 
        height = 6,
        units = "in",
        device = cairo_pdf)  # Use Cairo PDF device which handles fonts better
 #******************************************************
 