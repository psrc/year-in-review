library(tidyverse)
library(psrcrtp)

current_pop_year <- 2025

population_data <- regional_population_data() |>
    filter(year <= current_pop_year & grouping == "Total") |>
    select("year", "variable", "estimate") |>
    pivot_wider(names_from = variable, values_from = estimate) |>
    as_tibble() |>
    select(Year = "year", "Observed", "Forecast") |>
    mutate(
        Forecast = case_when(
            is.na(Forecast) ~ Observed,
            !(is.na(Forecast)) ~ Forecast
        )
    )

saveRDS(population_data, "data/regional_population.rds")
