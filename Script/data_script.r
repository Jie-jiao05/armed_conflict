install.packages("here")
install.packages("countrycode")
library(tidyverse)
library(dplyr)
library(here)
library(countrycode)
# Prepare World Bank data

# Read in maternal mortality data
matmor0 <- read.csv(here("Data", "Raw_Data", "week3_raw", "maternal_mortality.csv"), header = TRUE)
#other dataset
infmor0 <- read.csv(here("Data", "Raw_Data", "week3_raw","infant_mortality.csv"), header = TRUE)
neomor0 <- read.csv(here("Data", "Raw_Data", "week3_raw", "neonatal_mortality.csv"), header = TRUE)
un5mor0 <- read.csv(here("Data", "Raw_Data", "week3_raw","under5_mortality.csv"), header = TRUE)

# Change wide to long format
matmor_long <- matmor0 |>
  pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X", # removes X from year column
                 values_to = "maternal_mortality") |> 
  mutate(year = as.numeric(year)) |> # change year to numeric
  select(iso, year, maternal_mortality)

# the above work for one case just maternal one

#try to replicate it to all and make a funciton
wbfun <- function(dataname, varname){
  dataname |>
    dplyr::select(iso, X2000:X2019) |>
    pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X",
                 values_to = varname) |>
    mutate(year = as.numeric(year)) |>
    arrange(iso, year)
}

matmor <- wbfun(dataname = matmor0, varname = "matmor")
infmor <- wbfun(dataname = infmor0, varname = "infmor")
neomor <- wbfun(dataname = neomor0, varname = "neomor")
un5mor <- wbfun(dataname = un5mor0, varname = "un5mor")


#Prepare disaster data
install.packages("janitor")
library(janitor)
disas0 <- read.csv(here("Data", "Raw_Data", "week3_raw","disaster.csv"), header = TRUE)

disas <- disas0 |>
  janitor::clean_names() |>

  dplyr::filter(
    year >= 2000 & year <= 2019,disaster_type %in% c("Earthquake", "Drought")
  ) |>
  
  dplyr::select(year,iso,disaster_type
  ) |>
  
  dplyr::mutate(drought = ifelse(disaster_type == "Drought", 1, 0),earthquake = ifelse(disaster_type == "Earthquake", 1, 0)
  ) |>
    dplyr::select(year,iso,earthquake,drought)
    
# #Prepare conflict data
confl0 <- read.csv(here("Data", "Raw_Data", "week3_raw","conflict.csv"), header = TRUE)

confl <- confl0 |>
  dplyr::group_by(iso, year) |>
  dplyr::summarise(deaths = sum(best, na.rm = TRUE),.groups = "drop") |>
  
  dplyr::mutate(
    conflict = ifelse(deaths >= 25, 1, 0)
) |>

  dplyr::group_by(iso) |>
  dplyr::arrange(year, .by_group = TRUE) |>
  dplyr::mutate(
    conflict_lag = dplyr::lag(conflict, 1)
  ) |>
  dplyr::ungroup()

# Merge
final_data <- list(matmor,infmor,neomor,un5mor,disas,confl) |>
  reduce(
    full_join,
    by = c("iso", "year")
  )

write.csv(final_data,"/Users/shanjiejiao/Desktop/CHL5233.nosync/Week 2/Data/Processed_Data/final_data.csv")


### ANY FINDNING for CODEX?
# Well not too much, but in the methodology, codex tend to use different code that Professor's provided, but the result is the same.


