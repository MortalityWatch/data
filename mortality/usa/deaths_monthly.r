source("lib/common.r")

parse_totals <- function(year) {
  df_raw <- suppress_warnings(
    read_delim(paste0(
      "../wonder_dl/data_wonder/monthly/all/", year, ".txt"
    ), delim = "\t", col_types = cols(.default = "c")),
    "One or more parsing issues"
  )
  state_code_column <- intersect(
    c("Residence State Code", "State Code"),
    names(df_raw)
  )[1]
  if (is.na(state_code_column)) {
    stop(paste0(
      "Missing state code column in monthly totals file for ",
      year,
      ". Columns: ",
      paste(names(df_raw), collapse = ", ")
    ))
  }
  df_raw$state_code <- df_raw[[state_code_column]]

  df <- df_raw |>
    rowwise() |>
    mutate(
      deaths = as_integer(Deaths),
      year = as_integer(left(`Month Code`, 4)),
      month = as_integer(right(`Month Code`, 2)),
      id = sprintf("%02s", state_code)
    ) |>
    mutate(date = make_yearmonth(year, month)) |>
    filter(!is.na(date)) |>
    inner_join(us_states_iso3c,
      by = join_by(id),
      relationship = "many-to-many"
    ) |>
    select(
      "iso3c", "date", "year", "month", "deaths"
    )

  # National Totals via State Totals (no suppressed values, so ok)
  df_us <- df |>
    group_by(date, year, month) |>
    summarize(deaths = sum(deaths), .groups = "drop")
  df_us$iso3c <- "USA"
  df <- bind_rows(df, df_us)
  df$age <- "all"

  return(df)
}

parse_data <- function(df, state) {
  df <- df |>
    as_tibble() |>
    rowwise() |>
    mutate(
      deaths = as_integer(Deaths),
      year = as_integer(left(`Month Code`, 4)),
      month = as_integer(right(`Month Code`, 2))
    ) |>
    mutate(date = make_yearmonth(year, month))

  df$iso3c <- (us_states_iso3c |> filter(id == state))$iso3c

  df |>
    rename(age = `Single-Year Ages Code`) |>
    filter(!is.na(date), age != "") |>
    select(
      "iso3c", "date", "year", "month", "age", "deaths"
    ) |>
    mutate(age = ifelse(age == "100", "100+", age))
}

get_csv <- function(year, state) {
  parse_data(
    suppress_warnings(read_delim(
      paste0(
        "../wonder_dl/data_wonder/monthly/age/", year, "/", state, ".txt"
      ),
      delim = "\t",
      col_types = cols(.default = "c")
    ), "One or more parsing issues"),
    state
  )
}

state_ids <- fromJSON("../wonder_dl/data_wonder/states.json")
us_states_iso3c <- read_csv("./data_static/usa_states_iso3c.csv")

result_1y <- tibble()

get_max_year <- function() {
  files <- list.files(
    "../wonder_dl/data_wonder/monthly/all/",
    pattern = "\\d{4}\\.txt$", full.names = FALSE
  )
  years <- as.numeric(sub("\\.txt$", "", files))
  max(years, na.rm = TRUE)
}

max_year <- get_max_year()
years <- 1999:max_year

assert_files_exist(
  file.path("../wonder_dl/data_wonder/monthly/all", paste0(years, ".txt")),
  "CDC WONDER monthly totals input"
)

monthly_age_files <- expand.grid(
  year = years,
  state = state_ids,
  stringsAsFactors = FALSE
) |>
  mutate(path = file.path(
    "../wonder_dl/data_wonder/monthly/age",
    year,
    paste0(state, ".txt")
  )) |>
  pull(path)

assert_files_exist(
  monthly_age_files,
  "CDC WONDER monthly age input"
)

# Get Totals per state/month
for (year in years) {
  df_all <- parse_totals(year)
  result_1y <- bind_rows(result_1y, df_all)
}

# Age groups per state/month
for (year in years) {
  print(paste0("Processing ", year))

  df_us <- get_csv(year, "all")
  result_1y <- bind_rows(result_1y, df_us)
  join_columns <- setdiff(names(df_us), c("iso3c", "deaths"))

  # For each state calculate the difference between national and national w/o
  # state as actual values
  for (state in state_ids[state_ids != "all"]) {
    df_state_diff <- get_csv(year, state)
    df_state <- df_us |>
      inner_join(df_state_diff, by = join_columns) |>
      mutate(iso3c = iso3c.y, deaths = deaths.x - deaths.y) |>
      select(-iso3c.x, -iso3c.y, -deaths.x, -deaths.y)

    result_1y <- bind_rows(result_1y, df_state)
  }
}

# Verify
missing <- result_1y |>
  filter(year == 1999) |>
  filter(year <= max(result_1y$year - 2)) |>
  complete(iso3c, date, age) |>
  filter(is.na(deaths))
stopifnot(nrow(missing) == 0)

# Sort
age_levels <- c(as.character(0:100), "100+", "NS", "all")

print("Complete NS based on all...")
calc_ns <- function(data) {
  split_data <- split(data, data$age != "all")
  all <- split_data[["FALSE"]]
  ages <- split_data[["TRUE"]]

  data |>
    mutate(deaths = ifelse(age == "NS",
      sum(all$deaths) - sum(ages$deaths, na.rm = TRUE),
      deaths
    ))
}

result_1y <- result_1y |> group_by(iso3c, year, month)
df3_incomplete <- result_1y |> filter(any(is.na(deaths)) & !all(!is.na(deaths)))
df3_complete <- anti_join(
  result_1y, df3_incomplete,
  by = c("iso3c", "year", "month")
)
df3_incomplete <- df3_incomplete |>
  group_split() |>
  map(calc_ns) |>
  bind_rows()
result_1y <- rbind(df3_complete, df3_incomplete) |> ungroup()
rm(df3_complete, df3_incomplete)

# Convert the 'age' column to a factor with the specified levels
result_1y$age <- factor(result_1y$age, levels = age_levels)
result_1y <- result_1y[order(result_1y$age), ]

result_5y <- result_1y |>
  distinct(iso3c, date, year, month, age, .keep_all = TRUE) |>
  mutate(
    age_group = case_when(
      age %in% c("0", "1", "2", "3", "4") ~ "0-4",
      age %in% c("5", "6", "7", "8", "9") ~ "5-9",
      age %in% c("10", "11", "12", "13", "14") ~ "10-14",
      age %in% c("15", "16", "17", "18", "19") ~ "15-19",
      age %in% c("20", "21", "22", "23", "24") ~ "20-24",
      age %in% c("25", "26", "27", "28", "29") ~ "25-29",
      age %in% c("30", "31", "32", "33", "34") ~ "30-34",
      age %in% c("35", "36", "37", "38", "39") ~ "35-39",
      age %in% c("40", "41", "42", "43", "44") ~ "40-44",
      age %in% c("45", "46", "47", "48", "49") ~ "45-49",
      age %in% c("50", "51", "52", "53", "54") ~ "50-54",
      age %in% c("55", "56", "57", "58", "59") ~ "55-59",
      age %in% c("60", "61", "62", "63", "64") ~ "60-64",
      age %in% c("65", "66", "67", "68", "69") ~ "65-69",
      age %in% c("70", "71", "72", "73", "74") ~ "70-74",
      age %in% c("75", "76", "77", "78", "79") ~ "75-79",
      age %in% c("80", "81", "82", "83", "84") ~ "80-84",
      age %in% c("85", "86", "87", "88", "89") ~ "85-89",
      age %in% c("90", "91", "92", "93", "94") ~ "90-94",
      age %in% c("95", "96", "97", "98", "99") ~ "95-99",
      .default = age
    ),
    age_group = factor(age_group, levels = c(
      "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39",
      "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74",
      "75-79", "80-84", "85-89", "90-94", "95-99", "100+", "NS", "all"
    ))
  ) |>
  group_by(iso3c, date, age_group, year, month) |>
  summarise(deaths = sum(deaths), .groups = "drop") |>
  arrange(iso3c, date, age_group)

result_10y <- result_5y |>
  mutate(
    age_group = case_when(
      age_group %in% c("0-4") ~ "0-9",
      age_group %in% c("5-9") ~ "0-9",
      age_group %in% c("10-14") ~ "10-19",
      age_group %in% c("15-19") ~ "10-19",
      age_group %in% c("20-24") ~ "20-29",
      age_group %in% c("25-29") ~ "20-29",
      age_group %in% c("30-34") ~ "30-39",
      age_group %in% c("35-39") ~ "30-39",
      age_group %in% c("40-44") ~ "40-49",
      age_group %in% c("45-49") ~ "40-49",
      age_group %in% c("50-54") ~ "50-59",
      age_group %in% c("55-59") ~ "50-59",
      age_group %in% c("60-64") ~ "60-69",
      age_group %in% c("65-69") ~ "60-69",
      age_group %in% c("70-74") ~ "70-79",
      age_group %in% c("75-79") ~ "70-79",
      age_group %in% c("80-84") ~ "80-89",
      age_group %in% c("85-89") ~ "80-89",
      age_group %in% c("90-94") ~ "90-99",
      age_group %in% c("95-99") ~ "90-99",
      .default = age_group
    ),
    age_group = factor(age_group,
      levels = c(
        "0-9", "10-19", "20-29", "30-39", "40-49",
        "50-59", "60-69", "70-79", "80-89",
        "90-99", "100+", "NS", "all"
      )
    )
  ) |>
  group_by(iso3c, date, age_group, year, month) |>
  summarise(deaths = sum(deaths), .groups = "drop")

result_20y <- result_10y |>
  mutate(
    age_group = case_when(
      age_group %in% c("0-9") ~ "0-19",
      age_group %in% c("10-19") ~ "0-19",
      age_group %in% c("20-29") ~ "20-39",
      age_group %in% c("30-39") ~ "20-39",
      age_group %in% c("40-49") ~ "40-59",
      age_group %in% c("50-59") ~ "40-59",
      age_group %in% c("60-69") ~ "60-79",
      age_group %in% c("70-79") ~ "60-79",
      age_group %in% c("80-89") ~ "80-99",
      age_group %in% c("90-99") ~ "80-99",
      .default = age_group
    ),
    age_group = factor(
      age_group,
      levels = c(
        "0-19", "20-39", "40-59", "60-79",
        "80-99", "100+", "NS", "all"
      )
    )
  ) |>
  group_by(iso3c, date, age_group, year, month) |>
  summarise(deaths = sum(deaths), .groups = "drop")

save_csv_zip(
  result_1y |> arrange(iso3c, date, age),
  "deaths/usa/monthly_1y"
)
save_csv_zip(
  result_5y |> arrange(iso3c, date, age_group),
  "deaths/usa/monthly_5y"
)
save_csv_zip(
  result_10y |> arrange(iso3c, date, age_group),
  "deaths/usa/monthly_10y"
)
save_csv_zip(
  result_20y |> arrange(iso3c, date, age_group),
  "deaths/usa/monthly_20y"
)

# source("./mortality/usa/deaths_monthly.r")
