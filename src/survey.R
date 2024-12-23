
library (tidyverse)
library(tidyverse)
library(janitor)
library(tidyquant)
library(patchwork)
library(survival)
library(survminer)


survey<- read_csv(here::here("data/raw/Antibiotic_Usage_Practices_among_Pig_and_Poultry_Farmers_-_all_versions.csv")) |> 
  clean_names() |> 
  mutate_if(is.character, as_factor)
