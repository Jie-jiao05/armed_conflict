### read in all data sets

matmor0 <- read.csv(here("week3-functions", "data", "original", "maternalmortality.csv"), header = TRUE)
infmor0 <- read.csv(here("week3-functions", "data", "original", "infant_mortality.csv"), header = TRUE)
neomor0 <- read.csv(here("week3-functions", "data", "original", "neonatal_mortality.csv"), header = TRUE)
un5mor0 <- read.csv(here("week3-functions", "data", "original", "under5_mortality.csv"), header = TRUE)

matmor0$iso <- countrycode(matmor0$Country.Name, 
                          origin = "country.name", 
                          destination = "iso3c") 
matmor <- matmor0 |>
  dplyr::mutate(country = Country.Name, indicator = Indicator.Name) |>
  dplyr::select(iso, country, indicator, X2000:X2019)

un5mor0$iso <- countrycode(un5mor0$Country.Name, 
                           origin = "country.name", 
                           destination = "iso3c") 
un5mor <- un5mor0 |>
  dplyr::mutate(country = Country.Name, indicator = Indicator.Name) |>
  dplyr::select(iso, country, indicator, X2000:X2019)

write.csv(matmor, here("week3-functions", "data", "original", "maternal_mortality.csv"), row.names = F)

### write a function that does the above manipulation to each data
wbfun <- function(dataname, varname){
  dataname |>
    dplyr::select(Country.Name, X2000:X2019) |>
    pivot_longer(cols = starts_with("X"),
                 names_to = "year",
                 names_prefix = "X",
                 values_to = varname) |>
    mutate(year = as.numeric(year)) |>
    arrange(Country.Name, year)
}

matmor <- wbfun(dataname = matmor0, varname = "matmor")
infmor <- wbfun(dataname = infmor0, varname = "infmor")
neomor <- wbfun(dataname = neomor0, varname = "neomor")
un5mor <- wbfun(dataname = un5mor0, varname = "un5mor")

#put all data frames into list
wblist <- list(matmor, infmor, neomor, un5mor)

#merge all data frames in list
wblist |> reduce(full_join, by = c('Country.Name', 'year')) -> wbdata


# add ISO-3 to data
wbdata$ISO <- countrycode(wbdata$Country.Name, 
                          origin = "country.name", 
                          destination = "iso3c")
wbdata <- wbdata |>
  dplyr::select(-Country.Name)

