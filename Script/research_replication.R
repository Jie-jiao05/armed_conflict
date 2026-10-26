# Alternative preparation of all seven datasets in the assignment.
# Run this script directly; it imports all required raw data.
# This script does not source or edit data_script.r.

library(dplyr)
library(tidyr)
library(purrr)
library(janitor)
library(here)

raw_dir <- here("Data", "Raw_Data", "week3_raw")
matmor0 <- read.csv(file.path(raw_dir, "maternal_mortality.csv"))
infmor0 <- read.csv(file.path(raw_dir, "infant_mortality.csv"))
neomor0 <- read.csv(file.path(raw_dir, "neonatal_mortality.csv"))
un5mor0 <- read.csv(file.path(raw_dir, "under5_mortality.csv"))
disas0 <- read.csv(file.path(raw_dir, "disaster.csv"))
confl0 <- read.csv(file.path(raw_dir, "conflict.csv"))
covariates <- read.csv(file.path(raw_dir, "covariates.csv"))

# 1. World Bank mortality: one reusable function for all four outcomes.
# pivot_longer() belongs to tidyr. Keep missing mortality values as NA.
prepare_mortality <- function(data, outcome_name) {
  data |>
    select(iso, all_of(paste0("X", 2000:2019))) |>
    pivot_longer(
      cols = -iso,
      names_to = "year",
      names_prefix = "X",
      names_transform = list(year = as.integer),
      values_to = outcome_name
    ) |>
    arrange(iso, year)
}

mortality_tables <- list(
  maternal_mortality = matmor0,
  infant_mortality = infmor0,
  neonatal_mortality = neomor0,
  under5_mortality = un5mor0
) |>
  imap(prepare_mortality)

# Check the keys before joining, to avoid multiplying observations.
check_country_year <- function(data) {
  stopifnot(
    !anyNA(data$iso),
    !anyNA(data$year),
    all(nzchar(data$iso)),
    anyDuplicated(data[c("iso", "year")]) == 0L
  )
  invisible(data)
}
walk(mortality_tables, check_country_year)

mortality_alt <- reduce(mortality_tables, full_join, by = c("iso", "year"))
country_years <- mortality_alt |> distinct(iso, year)

# 2. Disasters: any qualifying event makes the indicator 1, even if
# multiple events occurred in the same country and year.
disaster_events <- disas0 |>
  clean_names() |>
  filter(
    between(year, 2000L, 2019L),
    disaster_type %in% c("Earthquake", "Drought")
  ) |>
  select(year, iso, disaster_type) |>
  group_by(iso, year) |>
  summarise(
    earthquake = as.integer(any(disaster_type == "Earthquake")),
    drought = as.integer(any(disaster_type == "Drought")),
    .groups = "drop"
  )

# Use the mortality panel to include country-years with no listed disaster.
# As required by the assignment, no matching event is coded as 0.
disaster_alt <- country_years |>
  left_join(disaster_events, by = c("iso", "year")) |>
  mutate(
    earthquake = replace_na(earthquake, 0L),
    drought = replace_na(drought, 0L)
  ) |>
  select(year, iso, earthquake, drought)

# 3. Conflict: sum event deaths within each conflict-country-year first,
# then flag whether ANY conflict reaches 25 deaths in that country-year.
# This follows the paper's calendar-conflict-year definition:
# https://journals.plos.org/plosmedicine/article?id=10.1371/journal.pmed.1003810
# Do not count repeated conflict IDs as separate conflicts or pool several
# below-threshold conflicts to create a qualifying conflict.
#
# In this supplied file, rows with both conflict_id and best missing are
# placeholders for country-years without recorded conflict: code them 0.
# A missing death estimate attached to an actual conflict remains unknown
# unless the observed deaths already establish that the threshold was met.
conflict_by_id <- confl0 |>
  filter(between(year, 1999L, 2018L)) |>
  mutate(best = if_else(is.na(conflict_id) & is.na(best), 0, best)) |>
  group_by(iso, year, conflict_id) |>
  summarise(
    observed_deaths = sum(best, na.rm = TRUE),
    incomplete_deaths = any(is.na(best)),
    .groups = "drop"
  ) |>
  mutate(
    qualifies = case_when(
      observed_deaths >= 25 ~ TRUE,
      incomplete_deaths ~ NA,
      TRUE ~ FALSE
    )
  )

conflict_alt <- conflict_by_id |>
  group_by(iso, year) |>
  summarise(conflict = as.integer(any(qualifies)), .groups = "drop") |>
  # Shift the calendar year rather than lagging rows in a possibly sparse
  # table: conflict in 1999 predicts mortality in 2000; 2018 predicts 2019.
  transmute(iso, year = as.integer(year) + 1L, armed_conflict = conflict)

# 4. Merge: retain the country-years in the supplied mortality datasets.
# These inputs contain 186 countries; this preparation does not apply the
# additional sample restrictions or fit the regression models in the paper.
check_country_year(disaster_alt)
check_country_year(conflict_alt)
check_country_year(covariates)

final_columns <- c(
  "iso", "year", "armed_conflict", "maternal_mortality",
  "infant_mortality", "under5_mortality", "neonatal_mortality",
  "drought", "earthquake", "gdp_1000", "oecd", "pop_dens",
  "urban", "age_dep", "male_edu", "temp", "rainfall_1000"
)

final_data_alt <- mortality_alt |>
  left_join(disaster_alt, by = c("iso", "year")) |>
  left_join(conflict_alt, by = c("iso", "year")) |>
  left_join(covariates, by = c("iso", "year")) |>
  select(all_of(final_columns)) |>
  arrange(iso, year)

check_country_year(final_data_alt)
stopifnot(
  identical(names(final_data_alt), final_columns),
  nrow(final_data_alt) == nrow(country_years),
  all(final_data_alt$year %in% 2000:2019),
  all(final_data_alt$earthquake %in% 0:1),
  all(final_data_alt$drought %in% 0:1),
  all(is.na(final_data_alt$armed_conflict) |
        final_data_alt$armed_conflict %in% 0:1)
)

# 5. Save the replication dataset separately from final_data.csv.
output_dir <- here("Data", "Processed_Data")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
write.csv(final_data_alt,
          file.path(output_dir, "final_data_replication.csv"),
          row.names = FALSE)
