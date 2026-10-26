library(dplyr)
library(tidyr)
library(purrr)
library(here)
library(janitor)

# Read all seven datasets.
raw_dir <- here("Data", "Raw_Data", "week3_raw")
matmor0 <- read.csv(file.path(raw_dir, "maternal_mortality.csv"))
infmor0 <- read.csv(file.path(raw_dir, "infant_mortality.csv"))
neomor0 <- read.csv(file.path(raw_dir, "neonatal_mortality.csv"))
un5mor0 <- read.csv(file.path(raw_dir, "under5_mortality.csv"))
disas0 <- read.csv(file.path(raw_dir, "disaster.csv"))
confl0 <- read.csv(file.path(raw_dir, "conflict.csv"))
covariates <- read.csv(file.path(raw_dir, "covariates.csv"))

# Convert each mortality dataset to one row per country and year.
wbfun <- function(dataname, varname) {
  dataname |>
    select(iso, all_of(paste0("X", 2000:2019))) |>
    pivot_longer(
      cols = -iso,
      names_to = "year",
      names_prefix = "X",
      values_to = varname
    ) |>
    mutate(year = as.integer(year)) |>
    arrange(iso, year)
}

matmor <- wbfun(matmor0, "maternal_mortality")
infmor <- wbfun(infmor0, "infant_mortality")
un5mor <- wbfun(un5mor0, "under5_mortality")
neomor <- wbfun(neomor0, "neonatal_mortality")

# Collapse multiple disasters to binary country-year indicators.
disas <- disas0 |>
  clean_names() |>
  filter(between(year, 2000L, 2019L),
         disaster_type %in% c("Earthquake", "Drought")) |>
  group_by(iso, year) |>
  summarise(
    drought = as.integer(any(disaster_type == "Drought")),
    earthquake = as.integer(any(disaster_type == "Earthquake")),
    .groups = "drop"
  )

# Retain the original threshold: at least 25 total deaths per country-year.
# Use prior-year exposure: conflict in 1999 maps to mortality in 2000.
# Shifting the calendar year avoids lagging across gaps in observations.
confl <- confl0 |>
  group_by(iso, year) |>
  summarise(deaths = sum(best, na.rm = TRUE), .groups = "drop") |>
  transmute(
    iso,
    year = as.integer(year) + 1L,
    armed_conflict = as.integer(deaths >= 25)
  ) |>
  filter(between(year, 2000L, 2019L))

# Check keys before joining so repeated events cannot multiply rows.
check_keys <- function(data) {
  stopifnot(
    !anyNA(data$iso), !anyNA(data$year),
    anyDuplicated(data[c("iso", "year")]) == 0L
  )
}
walk(list(matmor, infmor, un5mor, neomor, disas, confl, covariates),
     check_keys)

final_columns <- c(
  "iso", "year", "armed_conflict", "maternal_mortality",
  "infant_mortality", "under5_mortality", "neonatal_mortality",
  "drought", "earthquake", "gdp_1000", "oecd", "pop_dens",
  "urban", "age_dep", "male_edu", "temp", "rainfall_1000"
)

# Keep the 2000-2019 mortality panel and join every source by iso and year.
# No listed drought/earthquake means 0; missing outcome/covariate values
# remain NA.
final_data <- list(matmor, infmor, un5mor, neomor) |>
  reduce(full_join, by = c("iso", "year")) |>
  left_join(confl, by = c("iso", "year")) |>
  left_join(disas, by = c("iso", "year")) |>
  left_join(covariates, by = c("iso", "year")) |>
  mutate(
    drought = replace_na(drought, 0L),
    earthquake = replace_na(earthquake, 0L)
  ) |>
  select(all_of(final_columns)) |>
  arrange(iso, year)

check_keys(final_data)
stopifnot(
  identical(names(final_data), final_columns),
  nrow(final_data) == nrow(matmor),
  all(final_data$year %in% 2000:2019)
)

output_dir <- here("Data", "Processed_Data")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(final_data, file.path(output_dir, "final_data.csv"),
          row.names = FALSE)
