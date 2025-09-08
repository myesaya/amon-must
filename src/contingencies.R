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

# Step 1: Calculate Mean Inhibition and Standard Error
summary_data <- Bangwe |> 
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
  geom_line(size = 0.8) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean_inhibition - se_inhibition, ymax = mean_inhibition + se_inhibition), width = 0.2, size = 1.0) +
  geom_text(aes(label = round(mean_inhibition, 2)), vjust = -1, size = 5) + # Add mean values as text labels
  labs(title = "Microbial Inhibition Across Sample Types (Bangwe)",
       x = "Sample Type (Manure, Soil, Vegetables)",
       y = "Mean Inhibition") +
  theme_minimal()+
  theme(
    axis.title.x = element_text(size = 16), # Increase font size for x-axis title
    axis.title.y = element_text(size = 16), # Increase font size for y-axis title
    axis.text.x = element_text(size = 14, angle = 10, hjust = 1,color = "black"),   # Increase font size for x-axis categories (tick labels)
    axis.text.y = element_text(size = 14,color = "black"),     # Increase font size for y-axis categories (tick labels)
    plot.title = element_text(face = "bold", size = 16)  )

my_theme<- theme(
  axis.title.x = element_text(size = 16), # Increase font size for x-axis title
  axis.title.y = element_text(size = 16), # Increase font size for y-axis title
  axis.text.x = element_text(size = 14, angle = 10, hjust = 1,color = "black"),   # Increase font size for x-axis categories (tick labels)
  axis.text.y = element_text(size = 14,color = "black"),     # Increase font size for y-axis categories (tick labels)
  plot.title = element_text(face = "bold", size = 16)  )



# Display the plot
print(line_plot)


#I want to do anova 
# Two way ANOVA
model <- aov(inhibition ~ antibiotic , data = microbiology))
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


anova_table |> gtsave("Bangwe-anova.docx")


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

turkey<-final %>%
  mutate(Adjusted_p_value = sapply(Adjusted_p_value, format_p_value)) |> 
  gt() %>%
  tab_header(
    title = "Significant Difference
    
    s"
  ) |> 
  tab_footnote(
    footnote = "Significance codes: *** p < 0.001; ** p < 0.01; * p < 0.05", 
    locations = cells_column_labels(columns = Adjusted_p_value)  # Specify where to place the footnote
  )
turkey |> gtsave("Bangwe-turkey.docx")



# 3-way ANOVA -------------------------------------------------------------

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

turkey<-final %>%
  mutate(Adjusted_p_value = sapply(Adjusted_p_value, format_p_value)) |> 
  gt() %>%
  tab_header(
    title = "Significant Differences"
  ) |> 
  tab_footnote(
    footnote = "Significance codes: *** p < 0.001; ** p < 0.01; * p < 0.05", 
    locations = cells_column_labels(columns = Adjusted_p_value)  # Specify where to place the footnote
  )
turkey |> gtsave("Bangwe-antibiotic-turkey.docx")

#I now want the inhibition mean, se and n for each antibiotic
#is there a diference in the concentration of ECOLI in vegetables across all?

#I want to do anova 
# Two way ANOVA
model <- aov(inhibition ~ item , data = Bangwe)
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


anova_table |> gtsave("Bangwe-anova.docx")


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

turkey<-final %>%
  mutate(Adjusted_p_value = sapply(Adjusted_p_value, format_p_value)) |> 
  gt() %>%
  tab_header(
    title = "Significant Differences"
  ) |> 
  tab_footnote(
    footnote = "Significance codes: *** p < 0.001; ** p < 0.01; * p < 0.05", 
    locations = cells_column_labels(columns = Adjusted_p_value)  # Specify where to place the footnote
  )
turkey |> gtsave("Bangwe-turkey.docx")



# 3-way ANOVA -------------------------------------------------------------

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

turkey<-final %>%
  mutate(Adjusted_p_value = sapply(Adjusted_p_value, format_p_value)) |> 
  gt() %>%
  tab_header(
    title = "Significant Differences"
  ) |> 
  tab_footnote(
    footnote = "Significance codes: *** p < 0.001; ** p < 0.01; * p < 0.05", 
    locations = cells_column_labels(columns = Adjusted_p_value)  # Specify where to place the footnote
  )
turkey |> gtsave("Bangwe-antibiotic-turkey.docx")


# new tests ---------------------------------------------------------------

library(tidyverse)

# Step 1: Distinct microbes per farm and sample type
microbe_presence <- microbiology %>%
  filter(sample %in% c("Manure", "Soil", "Vegetable")) %>%
  distinct(farm, sample, microbe)  # Remove duplicate microbes per sample

# Step 2: Calculate pairwise Jaccard indices (similarity) within farms
results <- microbe_presence %>%
  group_by(farm) %>%
  summarise(
    # Manure-Soil similarity
    manure_soil = ifelse(
      "Manure" %in% sample & "Soil" %in% sample,
      length(intersect(
        microbe[sample == "Manure"], 
        microbe[sample == "Soil"]
      )) / length(union(
        microbe[sample == "Manure"], 
        microbe[sample == "Soil"]
      )),
      NA
    ),
    
    # Manure-Vegetable similarity
    manure_veg = ifelse(
      "Manure" %in% sample & "Vegetable" %in% sample,
      length(intersect(
        microbe[sample == "Manure"], 
        microbe[sample == "Vegetable"]
      )) / length(union(
        microbe[sample == "Manure"], 
        microbe[sample == "Vegetable"]
      )),
      NA
    ),
    
    # Soil-Vegetable similarity
    soil_veg = ifelse(
      "Soil" %in% sample & "Vegetable" %in% sample,  # Fix typo: "Vegetable"
      length(intersect(
        microbe[sample == "Soil"], 
        microbe[sample == "Vegetable"]
      )) / length(union(
        microbe[sample == "Soil"], 
        microbe[sample == "Vegetable"]
      )),
      NA
    )
  ) %>%
  ungroup()

# Step 3: Summarize results
summary_stats <- results %>%
  summarise(
    mean_manure_soil = mean(manure_soil, na.rm = TRUE),
    mean_manure_veg = mean(manure_veg, na.rm = TRUE),
    mean_soil_veg = mean(soil_veg, na.rm = TRUE)
  )

# Step 4: Identify key shared microbes
shared_microbes <- microbe_presence %>%
  group_by(microbe) %>%
  summarise(
    in_manure = any(sample == "Manure"),
    in_soil = any(sample == "Soil"),
    in_vegetable = any(sample == "Vegetable")
  ) %>%
  filter(in_manure & in_soil & in_vegetable)  # Microbes in all 3 types

# View results
print(summary_stats)
print(shared_microbes)
######################################
# Load required packages
library(tidyverse)
library(boot)

# Prepare the data (assuming 'microbiology' is already loaded)
microbe_presence <- microbiology %>%
  # Clean sample names and filter relevant types
  mutate(sample = str_trim(sample)) %>%
  filter(sample %in% c("Manure", "Soil", "Vegetable")) %>%
  # Get distinct microbes per farm and sample type
  distinct(farm, sample, microbe)

# Function to calculate Jaccard index
jaccard <- function(set1, set2) {
  intersection = length(intersect(set1, set2))
  union = length(union(set1, set2))
  if(union == 0) return(NA_real_)
  return(intersection/union)
}

# Calculate pairwise Jaccard indices per farm
farm_jaccard <- microbe_presence %>%
  group_by(farm) %>%
  summarise(
    manure_soil = jaccard(
      microbe[sample == "Manure"],
      microbe[sample == "Soil"]
    ),
    manure_veg = jaccard(
      microbe[sample == "Manure"],
      microbe[sample == "Vegetable"]
    ),
    soil_veg = jaccard(
      microbe[sample == "Soil"],
      microbe[sample == "Vegetable"]
    )
  ) %>%
  ungroup()

# Bootstrapping function for CI estimation
bootstrap_ci <- function(data, variable, n_boot = 5000) {
  # Remove NA values
  clean_data <- data[[variable]][!is.na(data[[variable]])]
  if(length(clean_data) < 2) return(tibble(mean = NA, lower_ci = NA, upper_ci = NA))
  
  # Perform bootstrapping
  boots <- boot(
    data = clean_data,
    statistic = function(x, i) mean(x[i]),
    R = n_boot
  )
  
  # Get BCa confidence interval
  ci <- tryCatch(
    boot.ci(boots, type = "bca"),
    error = function(e) NULL
  )
  
  if(is.null(ci)) {
    return(tibble(mean = mean(clean_data), lower_ci = NA, upper_ci = NA))
  }
  
  tibble(
    mean = mean(clean_data),
    lower_ci = ci$bca[4],
    upper_ci = ci$bca[5]
  )
}

# Calculate statistics for each comparison
comparisons <- c("manure_soil", "manure_veg", "soil_veg")
results <- map_dfr(comparisons, ~{
  ci_result <- bootstrap_ci(farm_jaccard, .x)
  ci_result %>%
    mutate(
      comparison = .x,
      # One-sample t-test against null (Jaccard = 0)
      t_test = t.test(
        farm_jaccard[[.x]], 
        alternative = "greater",
        mu = 0,
        na.action = na.omit
      )$p.value,
      # Interpret strength
      strength = case_when(
        mean < 0.1 ~ "Very Weak",
        mean < 0.3 ~ "Weak",
        mean < 0.5 ~ "Moderate",
        mean < 0.7 ~ "Strong",
        TRUE ~ "Very Strong"
      )
    )
}) %>%
  select(comparison, everything())

# Print results
cat("Farm-level Jaccard Indices:\n")
print(farm_jaccard, n = Inf)

cat("\nStatistical Summary:\n")
print(results)

#################################
# Load required packages
library(tidyverse)
library(boot)

# Prepare the data (assuming 'microbiology' is already loaded)
microbe_presence <- microbiology %>%
  mutate(sample = str_trim(sample)) %>%
  filter(sample %in% c("Manure", "Soil", "Vegetable")) %>%
  distinct(farm, sample, microbe)

# Jaccard function
jaccard <- function(set1, set2) {
  intersection = length(intersect(set1, set2))
  union = length(union(set1, set2))
  if(union == 0) return(NA_real_)
  return(intersection / union)
}

# Calculate pairwise Jaccard indices per farm
farm_jaccard <- microbe_presence %>%
  group_by(farm) %>%
  summarise(
    manure_soil = jaccard(
      microbe[sample == "Manure"],
      microbe[sample == "Soil"]
    ),
    manure_veg = jaccard(
      microbe[sample == "Manure"],
      microbe[sample == "Vegetable"]
    ),
    soil_veg = jaccard(
      microbe[sample == "Soil"],
      microbe[sample == "Vegetable"]
    ),
    .groups = 'drop'
  )

# Bootstrap function with full t-test extraction
bootstrap_ci <- function(x, n_boot = 5000) {
  x <- na.omit(x)
  if(length(x) < 2) return(tibble(mean = NA, lower_ci = NA, upper_ci = NA, t_value = NA, p_value = NA))
  
  # Bootstrapping for CI
  boots <- boot(
    data = x,
    statistic = function(data, i) mean(data[i]),
    R = n_boot
  )
  
  ci <- tryCatch(
    boot.ci(boots, type = "bca"),
    error = function(e) NULL
  )
  
  # One-sample t-test
  ttest <- t.test(x, alternative = "greater", mu = 0)
  
  tibble(
    mean = mean(x),
    lower_ci = if(!is.null(ci)) ci$bca[4] else NA,
    upper_ci = if(!is.null(ci)) ci$bca[5] else NA,
    t_value = ttest$statistic,
    p_value = ttest$p.value
  )
}

# Apply to each comparison
comparisons <- c("manure_soil", "manure_veg", "soil_veg")

results <- map_dfr(comparisons, function(comp) {
  stats <- bootstrap_ci(farm_jaccard[[comp]])
  stats %>%
    mutate(
      comparison = comp,
      strength = case_when(
        mean < 0.1 ~ "Very Weak",
        mean < 0.3 ~ "Weak",
        mean < 0.5 ~ "Moderate",
        mean < 0.7 ~ "Strong",
        TRUE ~ "Very Strong"
      )
    )
}) %>%
  select(comparison, mean, lower_ci, upper_ci, t_value, p_value, strength)

# Print results
print(results)

# Heatmap -----------------------------------------------------------------


library(tidyverse)
library(boot)
library(ggplot2)

# Jaccard function
jaccard <- function(set1, set2) {
  intersection = length(intersect(set1, set2))
  union = length(union(set1, set2))
  if (union == 0) return(NA_real_)
  return(intersection / union)
}

# Filter and prepare microbiology data
microbe_presence <- microbiology %>%
  mutate(sample = str_trim(sample)) %>%
  filter(sample %in% c("Manure", "Soil", "Vegetable")) %>%
  distinct(farm, sample, microbe)

# Calculate Jaccard per farm per pair
farm_jaccard <- microbe_presence %>%
  group_by(farm) %>%
  summarise(
    Manure_Soil = jaccard(
      microbe[sample == "Manure"],
      microbe[sample == "Soil"]
    ),
    Manure_Vegetable = jaccard(
      microbe[sample == "Manure"],
      microbe[sample == "Vegetable"]
    ),
    Soil_Vegetable = jaccard(
      microbe[sample == "Soil"],
      microbe[sample == "Vegetable"]
    ),
    .groups = 'drop'
  )

# Pivot to long format for plotting
jaccard_long <- farm_jaccard %>%
  pivot_longer(cols = -farm, names_to = "pair", values_to = "jaccard")

# Calculate mean Jaccard per pair for heatmap
jaccard_means <- jaccard_long %>%
  group_by(pair) %>%
  summarise(mean_jaccard = round(mean(jaccard, na.rm = TRUE), 2))

# Create a matrix-friendly format from pair names
jaccard_matrix <- jaccard_means %>%
  separate(pair, into = c("Comp1", "Comp2"), sep = "_") %>%
  mutate(Comp1 = str_to_title(Comp1),
         Comp2 = str_to_title(Comp2))

# Duplicate to make full symmetric matrix
heat_data <- bind_rows(
  jaccard_matrix,
  jaccard_matrix %>% rename(Comp1 = Comp2, Comp2 = Comp1)
)

# Add diagonal for self-comparison
diag_comp <- tibble(Comp1 = c("Manure", "Soil", "Vegetable"),
                    Comp2 = c("Manure", "Soil", "Vegetable"),
                    mean_jaccard = 1)

heat_data <- bind_rows(heat_data, diag_comp)

# Plot heatmap
ggplot(heat_data, aes(x = Comp1, y = Comp2, fill = mean_jaccard)) +
  geom_tile(color = "white") +
  geom_text(aes(label = sprintf("%.2f", mean_jaccard)), size = 5, , fontface = "bold") +
  scale_fill_gradient2(
    low = "#d73027", mid = "white", high = "#1a9850", midpoint = 0.5,
    name = "Jaccard Index"
  ) +
  labs(title = "Microbial Similarity Across Compartments",
       x = "Compartment",
       y = "Compartment") +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_text(face = "bold", colour = "black"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid = element_blank()
  )


# Heatmap revised ---------------------------------------------------------

library(tidyverse)
library(boot)
library(ggplot2)
library(glue)

# STEP 1: Prepare presence data
microbe_presence <- microbiology %>%
  mutate(sample = str_trim(sample)) %>%
  filter(sample %in% c("Manure", "Soil", "Vegetable")) %>%
  distinct(farm, sample, microbe)

# STEP 2: Jaccard function
jaccard <- function(set1, set2) {
  intersection <- length(intersect(set1, set2))
  union <- length(union(set1, set2))
  if (union == 0) return(NA_real_)
  intersection / union
}

# STEP 3: Calculate Jaccard per farm
farm_jaccard <- microbe_presence %>%
  group_by(farm) %>%
  summarise(
    Manure_Soil = jaccard(microbe[sample == "Manure"], microbe[sample == "Soil"]),
    Manure_Vegetable = jaccard(microbe[sample == "Manure"], microbe[sample == "Vegetable"]),
    Soil_Vegetable = jaccard(microbe[sample == "Soil"], microbe[sample == "Vegetable"]),
    .groups = 'drop'
  )

# STEP 4: Bootstrap CI function
bootstrap_ci <- function(x, n_boot = 5000) {
  x <- na.omit(x)
  if(length(x) < 2) return(tibble(mean = NA, lower_ci = NA, upper_ci = NA, p_value = NA))
  
  boots <- boot(data = x, statistic = function(data, i) mean(data[i]), R = n_boot)
  ci <- tryCatch(boot.ci(boots, type = "bca"), error = function(e) NULL)
  ttest <- t.test(x, alternative = "greater", mu = 0)
  
  tibble(
    mean = round(mean(x), 2),
    lower_ci = if (!is.null(ci)) round(ci$bca[4], 2) else NA,
    upper_ci = if (!is.null(ci)) round(ci$bca[5], 2) else NA,
    p_value = round(ttest$p.value, 3)
  )
}

# STEP 5: Calculate stats per pair
comparisons <- c("Manure_Soil", "Manure_Vegetable", "Soil_Vegetable")
results <- map_dfr(comparisons, function(comp) {
  stats <- bootstrap_ci(farm_jaccard[[comp]])
  stats$pair <- comp
  stats
})

# STEP 6: Reformat into matrix-friendly structure
heat_data <- results %>%
  separate(pair, into = c("Comp1", "Comp2"), sep = "_") %>%
  mutate(
    Comp1 = str_to_title(Comp1),
    Comp2 = str_to_title(Comp2),
    label = glue("{mean}\n(CI: {lower_ci}–{upper_ci}, p={p_value})")
  )

# Duplicate rows for symmetry
heat_data_sym <- bind_rows(
  heat_data,
  heat_data %>% rename(Comp1 = Comp2, Comp2 = Comp1)
)

# Add diagonal
diag_vals <- tibble(
  Comp1 = c("Manure", "Soil", "Vegetable"),
  Comp2 = c("Manure", "Soil", "Vegetable"),
  mean = 1,
  lower_ci = NA,
  upper_ci = NA,
  p_value = NA,
  label = "1.00\n(Self)"
)

# Combine all heatmap data
heatmap_df <- bind_rows(heat_data_sym, diag_vals)

# STEP 7: Plot
ggplot(heatmap_df, aes(x = Comp1, y = Comp2, fill = mean)) +
  geom_tile(color = "white") +
  geom_text(aes(label = label), size = 4.2, fontface = "bold", lineheight = 1.1)+
  scale_fill_gradient2(
    low = "#d73027", mid = "white", high = "#1a9850", midpoint = 0.5,
    name = "Jaccard Index", limits = c(0, 1)
  ) +
  labs(
    title = "Microbial Similarity Across Compartments",
    subtitle = "Jaccard index with 95% CI and p-values",
    x = "Compartment", y = "Compartment"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_text(face = "bold", colour = "black"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid = element_blank()
  )

# Jacard revised ----------------------------------------------------------
bootstrap_microbe_sample <- function(data, sample_col) {
  # Define which samples are being compared
  samples <- c("Manure", "Soil", "Vegetable")
  other_samples <- setdiff(samples, sample_col)
  
  x <- data[[sample_col]]
  y <- (data[[other_samples[1]]] + data[[other_samples[2]]]) > 0
  
  if (sum(x | y) == 0) {
    return(tibble(mean = NA, lower_ci = NA, upper_ci = NA, p_value = NA))
  }
  
  # Perform bootstrapping
  boots <- boot(data = data.frame(x, y), statistic = function(d, i) {
    jaccard_index(d$x[i], d$y[i])
  }, R = 5000)
  
  boot_vals <- boots$t[is.finite(boots$t)]
  
  # Handle constant or NA bootstrap values
  if (length(unique(boot_vals)) <= 1) {
    return(tibble(
      mean = round(mean(boot_vals), 2),
      lower_ci = NA,
      upper_ci = NA,
      p_value = NA
    ))
  }
  
  ci <- tryCatch(boot.ci(boots, type = "bca"), error = function(e) NULL)
  ttest <- tryCatch(t.test(boot_vals, alternative = "greater", mu = 0), error = function(e) NULL)
  
  tibble(
    mean = round(mean(boot_vals), 2),
    lower_ci = if (!is.null(ci)) round(ci$bca[4], 2) else NA,
    upper_ci = if (!is.null(ci)) round(ci$bca[5], 2) else NA,
    p_value = if (!is.null(ttest)) round(ttest$p.value, 3) else NA
  )
}

# Heatmap revised2 --------------------------------------------------------

# Load libraries
library(tidyverse)
library(boot)
library(ggplot2)
library(glue)

# STEP 1: Clean input data
microbe_presence <- microbiology %>%
  mutate(
    sample = str_trim(sample),
    microbe = str_trim(microbe)
  ) %>%
  filter(sample %in% c("Manure", "Soil", "Vegetable")) %>%
  distinct(farm, sample, microbe)

# STEP 2: Keep only microbes found in ALL THREE sample types
microbes_all_three <- microbe_presence %>%
  distinct(sample, microbe) %>%
  group_by(microbe) %>%
  summarise(n_samples = n_distinct(sample), .groups = "drop") %>%
  filter(n_samples == 3) %>%
  pull(microbe)

# STEP 3: Filter to relevant microbes only
filtered_data <- microbe_presence %>%
  filter(microbe %in% microbes_all_three)

# STEP 4: Create presence matrix: one row per farm × microbe
presence_matrix <- filtered_data %>%
  mutate(present = 1) %>%
  pivot_wider(
    names_from = sample,
    values_from = present,
    values_fill = 0
  )

# STEP 5: Jaccard index function
jaccard_index <- function(x, y) {
  intersection <- sum(x & y)
  union <- sum(x | y)
  if (union == 0) return(NA_real_)
  intersection / union
}

# STEP 6: Bootstrap function per microbe-sample
bootstrap_microbe_sample <- function(df, sample_col) {
  samples <- c("Manure", "Soil", "Vegetable")
  other_samples <- setdiff(samples, sample_col)
  
  x <- df[[sample_col]]
  y <- (df[[other_samples[1]]] + df[[other_samples[2]]]) > 0
  
  if (sum(x | y) == 0) {
    return(tibble(mean = NA, lower_ci = NA, upper_ci = NA, p_value = NA))
  }
  
  boots <- boot(data = data.frame(x, y), statistic = function(d, i) {
    jaccard_index(d$x[i], d$y[i])
  }, R = 5000)
  
  boot_vals <- boots$t[is.finite(boots$t)]
  
  if (length(unique(boot_vals)) <= 1) {
    return(tibble(
      mean = round(mean(boot_vals), 2),
      lower_ci = NA,
      upper_ci = NA,
      p_value = NA
    ))
  }
  
  ci <- tryCatch(boot.ci(boots, type = "bca"), error = function(e) NULL)
  ttest <- tryCatch(t.test(boot_vals, alternative = "greater", mu = 0), error = function(e) NULL)
  
  tibble(
    mean = round(mean(boot_vals), 2),
    lower_ci = if (!is.null(ci)) format(ci$bca[4], digits = 2, nsmall = 2) else NA,
    upper_ci = if (!is.null(ci)) format(ci$bca[5], digits = 2, nsmall = 2) else NA,
    p_value = if (!is.null(ttest)) {
      if (ttest$p.value < 0.001) "p<0.001" else glue("p={format(round(ttest$p.value, 3), nsmall = 3)}")
    } else NA
  )
}

# STEP 7: Loop over each microbe × sample
jaccard_results <- presence_matrix %>%
  group_split(microbe) %>%
  map_dfr(function(df) {
    microbe_name <- df$microbe[1]
    map_dfr(c("Manure", "Soil", "Vegetable"), function(smp) {
      res <- bootstrap_microbe_sample(df, smp)
      res$microbe <- microbe_name
      res$sample <- smp
      res
    })
  })

# STEP 8: Add label and factor levels
jaccard_results <- jaccard_results %>%
  mutate(
    label = glue("{mean}\n({lower_ci}–{upper_ci}, {p_value})"),
    sample = factor(sample, levels = c("Manure", "Soil", "Vegetable"))
  )

# STEP 9: Plot the heatmap
ggplot(jaccard_results, aes(x = sample, y = fct_rev(factor(microbe)), fill = mean)) +
  geom_tile(color = "white") +
  geom_text(aes(label = label), size = 4, fontface = "bold", lineheight = 1.1) +
  scale_fill_gradient2(
    low = "#1a9850", mid = "pink", high = "orange",
    midpoint = 0.5, name = "Jaccard Index", limits = c(0, 1), na.value = "grey80"
  ) +
  scale_y_discrete(labels = function(x) parse(text = paste0("italic('", x, "')"))) +
  labs(
    title = "Consistency of Microbial Presence Across Farms",
    subtitle = "Jaccard index by sample type (with 95% CI and p-values)",
    x = "Sample Type",
    y = "Microbe"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text = element_text(face = "bold", colour = "black"),
    axis.title = element_text(face = "bold"),
    plot.title = element_text(face = "bold", hjust = 0.5),
    plot.subtitle = element_text(hjust = 0.5),
    panel.grid = element_blank()
  )

