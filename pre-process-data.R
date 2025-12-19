library(tidyverse)
library(tidycensus)
library(psrcrtp)

first_pop_year <- 2010
current_pop_year <- 2025
first_vision_year <- 2018
current_acs_year <- 2024

regional_geographies <- c(
    "Metropolitan Cities",
    "Core Cities",
    "High Capacity Transit Community",
    "Cities & Towns",
    "Unincorporated",
    "Region"
)

population_region <- regional_population_data() |>
    filter(year <= current_pop_year & grouping == "Total") |>
    select(
        Geography = "geography",
        Year = "year",
        Estimate = "estimate",
        Variable = "variable"
    ) |>
    mutate(Year = as.numeric(Year))

saveRDS(population_region, "data/regional_population.rds")

population_regional_geography <- get_ofm_pop_data() |>
    filter(
        Year >= first_pop_year - 1 &
            Variable == "Total Population" &
            !(str_detect(Jurisdiction, "Region"))
    ) |>
    group_by(regional_geography, Year) |>
    summarise(Estimate = sum(Estimate)) |>
    as_tibble() |>
    group_by(regional_geography) |>
    mutate(Change = Estimate - lag(Estimate)) |>
    as_tibble() |>
    filter(
        Year >= first_pop_year & regional_geography %in% regional_geographies
    ) |>
    filter(Year >= first_vision_year) |>
    group_by(regional_geography) |>
    summarise(Change = sum(Change)) |>
    as_tibble()

population_regional_geography <- population_regional_geography |>
    bind_rows(
        population_regional_geography |>
            summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) |>
            mutate(regional_geography = "Region")
    ) |>
    mutate(
        Geography = factor(regional_geography, levels = regional_geographies)
    ) |>
    arrange(Geography) |>
    select("Geography", "Change") |>
    mutate(Year = paste0(first_vision_year, " to ", current_pop_year))

r <- population_regional_geography |>
    filter(Geography == "Region") |>
    select("Change") |>
    pull()

population_regional_geography <- population_regional_geography |>
    mutate(Share = Change / r) |>
    select("Geography", "Change", "Share", "Year")

rm(r)

saveRDS(
    population_regional_geography,
    "data/regional_geography_population_growth.rds"
)

population_by_race <- NULL
for (y in c(first_vision_year, current_acs_year)) {
    p <- get_acs(
        geography = "county",
        state = "WA",
        county = c(
            "King County",
            "Kitsap County",
            "Pierce County",
            "Snohomish County"
        ),
        table = c("B03002"),
        year = y,
        survey = "acs1"
    ) |>
        filter(
            variable %in%
                c(
                    "B03002_003",
                    "B03002_004",
                    "B03002_005",
                    "B03002_006",
                    "B03002_007",
                    "B03002_008",
                    "B03002_009",
                    "B03002_012"
                )
        ) |>
        mutate(geography = "Region") |>
        mutate(
            Race = case_when(
                variable == "B03002_003" ~ "White",
                variable == "B03002_004" ~ "Black or African American",
                variable == "B03002_005" ~ "American Indian and Alaska Native",
                variable == "B03002_006" ~ "Asian and Pacific Islander",
                variable == "B03002_007" ~ "Asian and Pacific Islander",
                variable == "B03002_008" ~ "Some other race",
                variable == "B03002_009" ~ "Two or more races",
                variable == "B03002_012" ~ "Hispanic or Latinx"
            )
        ) |>
        group_by(geography, Race) |>
        summarise(Estimate = sum(estimate)) |>
        as_tibble() |>
        mutate(Year = y) |>
        select("Year", "Race", "Estimate")

    p <- p |>
        bind_rows(
            p |>
                summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) |>
                mutate(Race = "Region")
        ) |>
        mutate(Year = y)

    r <- p |>
        filter(Race == "Region") |>
        select("Estimate") |>
        pull()

    p <- p |>
        mutate(Share = Estimate / r)

    if (is.null(population_by_race)) {
        population_by_race <- p
    } else {
        population_by_race <- bind_rows(population_by_race, p)
    }
    rm(p, r)
}

rm(y)
saveRDS(population_by_race, "data/regional_population_by_race.rds")
