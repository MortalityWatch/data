source("lib/common.r")
source("lib/asmr.r")
source("lib/life_expectancy.r")
source("lib/parallel.r")

source("mortality/world_dataset_functions.r")

asmr_types <- c("asmr_who", "asmr_esp", "asmr_usa", "asmr_country")

# Load Data
source("mortality/_collection/un.r")
source("mortality/_collection/mortality_org.r")
source("mortality/_collection/world_mortality.r")
source("mortality/_collection/eurostat.r")
source("mortality/usa/mortality_states.r")
source("mortality/can/mortality_states.r")
source("mortality/deu/mortality_states.r")

data <- rbind(
  deu_mortality_states,
  can_mortality_states,
  usa_mortality_states,
  eurostat,
  world_mortality,
  mortality_org,
  un
) |>
  arrange(iso3c, date, desc(type), source)

rm(
  deu_mortality_states,
  can_mortality_states,
  usa_mortality_states,
  eurostat,
  world_mortality,
  mortality_org,
  un
)

# Define type resolution priorities
priority_weekly <- c(3, 2, 1) # Prefer 3, then 2, then 1
priority_monthly <- c(2, 3, 1) # Prefer 2, then 3, then 1
priority_yearly <- c(1, 2, 3) # Prefer 1, then 2, then 3

# Minimum source type required for each output resolution
# Type 3 = weekly, Type 2 = monthly, Type 1 = yearly
# This prevents interpolation artifacts (e.g., yearly LE appearing in weekly output)
min_type_for_output <- c(
  "yearweek" = 3,    # Weekly output requires weekly source
  "yearmonth" = 2,   # Monthly output requires monthly or weekly source
  "yearquarter" = 2, # Quarterly output requires monthly or weekly source
  "year" = 1,        # Yearly output accepts any source
  "fluseason" = 2,   # Flu season requires monthly or weekly source
  "midyear" = 2      # Midyear requires monthly or weekly source
)
priority_dataset <- c(
  "cdc", "statcan", "destatis", "eurostat", "world_mortality", "mortality_org",
  "un"
)

# Create a subset of the data for Development or Testing only.
if (Sys.getenv("STAGE") != "") {
  data <- data |> filter(iso3c %in% c("USA", "SWE", "JPN", "DEU", "AFG"))
}
if (Sys.getenv("ISO3C") != "") {
  data <- data |> filter(str_detect(iso3c, Sys.getenv("ISO3C")))
}

# Country names are saved in meta data.
source("mortality/world_iso.r")
save_info(
  df = data |> inner_join(iso3c_jurisdiction, by = c("iso3c")),
  upload = FALSE
)

rm(iso3c_jurisdiction)

# Function to select the correct row per date based on priority order
# min_type: optional minimum source type required (filters out lower resolution data)
filter_by_priority <- function(data, priority_order, min_type = 1) {
  data |>
    filter(type >= min_type) |>
    group_by(date, age_group) |>
    # Arrange by the given priority order
    arrange(match(source, priority_dataset), match(type, priority_order)) |>
    slice(1) |> # Select the first row per date
    ungroup()
}

process_country <- function(df) {
  iso3c <- df[1, ]$iso3c
  print(paste0("ISO: ", iso3c))
  # Make furr pull in these functions
  fluseason <- fluseason
  midyear <- midyear
  yearweek <- yearweek
  dd <- df |>
    expand_daily() |>
    mutate(cmr = deaths / population * 100000)

  dd_all <- dd |> filter(age_group == "all")
  dd_age <- dd |> filter(age_group != "all")
  dd_asmr <- dd_age
  dd_le_all <- tibble()
  if (nrow(dd_age)) {
    dd_asmr <- dd_age |>
      group_by(iso3c, type, n_age_groups, source) |>
      group_modify(~ calculate_asmr_variants(.x), .keep = TRUE) |>
      ungroup()
    dd_asmr$age_group <- "all"

    # Calculate life expectancy for ALL ages from age-stratified data
    dd_le_all <- dd_age |>
      group_by(iso3c, date, type, n_age_groups, source) |>
      group_modify(~ calculate_le_all_ages(.x), .keep = TRUE) |>
      ungroup()

    # Extract e0 for "all" age group (must start at age 0)
    dd_le_e0 <- dd_le_all |>
      filter(grepl("^0", age_group)) |>
      group_by(iso3c, date, type, source) |>
      slice(1) |>
      ungroup() |>
      select(iso3c, date, type, source, n_age_groups, le)

    # Pick best LE per date (prefer non-NA with highest n_age_groups)
    # This allows using eurostat LE even when destatis is preferred for deaths
    dd_le_best <- dd_le_e0 |>
      filter(!is.na(le)) |>
      group_by(iso3c, date) |>
      arrange(desc(n_age_groups)) |>
      slice(1) |>
      ungroup() |>
      select(iso3c, date, le, source_le = source)

    # Merge best LE into ASMR data (not requiring source match)
    dd_asmr <- dd_asmr |>
      left_join(dd_le_best, by = c("iso3c", "date"))

    # Merge LE into age-specific data (for individual age group outputs)
    dd_age <- dd_age |>
      left_join(
        dd_le_all |> select(iso3c, date, type, source, age_group, le),
        by = c("iso3c", "date", "type", "source", "age_group")
      )
  }

  # Apply filtering to both datasets
  # For "all" ages (deaths/population only), no min_type needed
  dd_all_weekly <- filter_by_priority(dd_all, priority_weekly)
  dd_all_monthly <- filter_by_priority(dd_all, priority_monthly)
  dd_all_yearly <- filter_by_priority(dd_all, priority_yearly)

  # For ASMR/LE data, apply min_type to prevent interpolation artifacts
  dd_asmr_weekly <- filter_by_priority(dd_asmr, priority_weekly, min_type_for_output["yearweek"])
  dd_asmr_monthly <- filter_by_priority(dd_asmr, priority_monthly, min_type_for_output["yearmonth"])
  dd_asmr_yearly <- filter_by_priority(dd_asmr, priority_yearly, min_type_for_output["year"])

  # For age-specific data (includes LE), apply min_type
  dd_age_weekly <- filter_by_priority(dd_age, priority_weekly, min_type_for_output["yearweek"])
  dd_age_monthly <- filter_by_priority(dd_age, priority_monthly, min_type_for_output["yearmonth"])
  dd_age_yearly <- filter_by_priority(dd_age, priority_yearly, min_type_for_output["year"])

  for (ag in unique(dd$age_group)) {
    print(paste0("Age Group: ", ag))
    if (ag == "all") {
      write_dataset(
        iso3c, ag,
        weekly = summarize_data_all(dd_all_weekly, dd_asmr_weekly,
          type = "yearweek"
        ),
        monthly = summarize_data_all(dd_all_monthly, dd_asmr_monthly,
          type = "yearmonth"
        ),
        quarterly = summarize_data_all(dd_all_monthly, dd_asmr_monthly,
          type = "yearquarter"
        ),
        yearly = summarize_data_all(dd_all_yearly, dd_asmr_yearly,
          type = "year"
        ),
        by_fluseason = summarize_data_all(dd_all_monthly, dd_asmr_monthly,
          type = "fluseason"
        ),
        by_midyear = summarize_data_all(dd_all_monthly, dd_asmr_monthly,
          type = "midyear"
        )
      )
    } else {
      dd_ag_f_weekly <- dd_age_weekly |>
        filter(age_group == ag) |>
        distinct(iso3c, date, age_group, .keep_all = TRUE)
      dd_ag_f_monthly <- dd_age_monthly |>
        filter(age_group == ag) |>
        distinct(iso3c, date, age_group, .keep_all = TRUE)
      dd_ag_f_yearly <- dd_age_yearly |>
        filter(age_group == ag) |>
        distinct(iso3c, date, age_group, .keep_all = TRUE)
      write_dataset(
        iso3c, ag,
        weekly = summarize_data_by_time(dd_ag_f_weekly,
          type = "yearweek"
        ),
        monthly = summarize_data_by_time(dd_ag_f_monthly,
          type = "yearmonth"
        ),
        quarterly = summarize_data_by_time(dd_ag_f_monthly,
          type = "yearquarter"
        ),
        yearly = summarize_data_by_time(dd_ag_f_yearly,
          type = "year"
        ),
        by_fluseason = summarize_data_by_time(dd_ag_f_monthly,
          type = "fluseason"
        ),
        by_midyear = summarize_data_by_time(dd_ag_f_monthly,
          type = "midyear"
        )
      )
    }
  }
}

if (Sys.getenv("CI") != 1) {
  countries <- unique(data$iso3c)
  with_progress({
    p <- progressor(steps = length(countries))
    data |>
      group_split(iso3c) |>
      future_walk(~ {
        process_country(.x)
        p()
      })
  })
} else {
  df <- data |>
    group_split(iso3c) |>
    walk(process_country)
}

print("Finished.")

# source("mortality/world_dataset.r")
