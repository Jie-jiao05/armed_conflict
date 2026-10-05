install.packages("here")
install.packages("countrycode")
library(tidyverse)
library(dplyr)
library(here)
library(countrycode)
# Read in maternal mortality data
matmor <- read.csv(here("Data", "Raw_Data", "week3_raw", "maternal_mortality.csv"), header = TRUE)
#other dataset
infmor <- read.csv(here("Data", "Raw_Data", "week3_raw","infant_mortality.csv"), header = TRUE)
neomor <- read.csv(here("Data", "Raw_Data", "week3_raw", "neonatal_mortality.csv"), header = TRUE)
un5mor <- read.csv(here("Data", "Raw_Data", "week3_raw","under5_mortality.csv"), header = TRUE)

# Change wide to long format
matmor_long <- matmor |>
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

matmor <- wbfun(dataname = matmor, varname = "matmor")
infmor <- wbfun(dataname = infmor, varname = "infmor")
neomor <- wbfun(dataname = neomor, varname = "neomor")
un5mor <- wbfun(dataname = un5mor, varname = "un5mor")