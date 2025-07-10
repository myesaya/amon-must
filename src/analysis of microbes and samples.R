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


microbiology<- read_csv(here::here("data/raw/water-quality.csv")) |> 
  clean_names() |> 
  mutate_if(is.character, as_factor) 

