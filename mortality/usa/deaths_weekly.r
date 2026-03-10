source("lib/common.r")
options(warn = 1)

parse_data <- function(df, jurisdiction_column, age_group) {
  df <- df |>
    mutate(
      year = as.numeric(left(`MMWR Week Code`, 4)),
      week = as.numeric(right(`MMWR Week Code`, 2))
    ) |>
    filter(!is.na(year), !is.na(week), week != 99) |>
    rowwise() |>
    mutate(date = make_yearweek(year, week), Deaths = as_integer(Deaths))
  if (nchar(jurisdiction_column) == 0) {
    df <- df |>
      select("date", "year", "week", "Deaths") |>
      setNames(c("date", "year", "week", "deaths"))
    df$iso3c <- "USA"
    df |>
      filter(!is.na(date)) |>
      select("iso3c", "date", "year", "week", "deaths") |>
      mutate(age_group = gsub("_", "-", age_group), .after = date)
  } else {
    df |>
      select(!!jurisdiction_column, "date", "year", "week", "Deaths") |>
      setNames(c("jurisdiction", "date", "year", "week", "deaths")) |>
      left_join(us_states_iso3c, by = "jurisdiction") |>
      filter(!is.na(iso3c), !is.na(date)) |>
      select("iso3c", "date", "year", "week", "deaths") |>
      mutate(age_group = gsub("_", "-", age_group), .after = date)
  }
}

get_csv <- function(j, a) {
  files <- Sys.glob(paste0("../wonder_dl/data_wonder/weekly/", j, "_", a, "_*.txt"))
  df <- bind_rows(lapply(files, function(f) {
    suppressWarnings(
      read_delim(f, delim = "\t", col_types = cols(.default = "c"))
    )
  }))
  parse_data(df, ifelse(j == "usa", "", "Residence State"), a)
}

us_states_iso3c <- read_csv("./data_static/usa_states_iso3c.csv")

five_year_age_groups <- c(
  "0-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44",
  "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84",
  "85-89", "90-94", "95-100"
)

df_result <- tibble()
for (j in c("usa", "usa-state")) {
  df_all <- get_csv(j, "all")
  # First calculate NS
  df_ns <- get_csv(j, "NS") |>
    inner_join(df_all |> select(iso3c, year, week, deaths),
      by = join_by(iso3c, year, week)
    ) |>
    mutate(deaths = deaths.y - deaths.x) |>
    select(-deaths.x, -deaths.y)
  df_result <- rbind(df_result, df_all, df_ns)

  # Then all age_groups
  for (ag in c(five_year_age_groups)) {
    df <- get_csv(j, ag) |>
      inner_join(df_all |> select(iso3c, year, week, deaths),
        by = join_by(iso3c, year, week)
      ) |>
      inner_join(df_ns |> select(iso3c, year, week, deaths),
        by = join_by(iso3c, year, week)
      ) |>
      # all - NS - (all but ag)
      mutate(deaths = deaths.y - deaths - deaths.x) |>
      select(-deaths.x, -deaths.y)
    df_result <- rbind(df_result, df)
  }
}

result_5y <- df_result |>
  distinct(iso3c, date, age_group, .keep_all = TRUE) |>
  mutate(age_group = ifelse(age_group == "95-100", "95+", age_group)) |>
  arrange(iso3c, date, age_group)

missing <- result_5y |>
  complete(iso3c, date, age_group) |>
  filter(is.na(deaths), date < max(result_5y$date) - 8)
stopifnot(nrow(missing) == 0)

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
      age_group %in% c("90-94") ~ "90+",
      age_group %in% c("95+") ~ "90+",
      .default = age_group
    )
  ) |>
  group_by(iso3c, date, age_group, year, week) |>
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
      age_group %in% c("80-89") ~ "80+",
      age_group %in% c("90+") ~ "80+",
      .default = age_group
    )
  ) |>
  group_by(iso3c, date, age_group, year, week) |>
  summarise(deaths = sum(deaths), .groups = "drop")

save_csv_zip(
  result_5y |> arrange(iso3c, year, week, age_group),
  "deaths/usa/weekly_5y"
)
save_csv_zip(
  result_10y |> arrange(iso3c, year, week, age_group),
  "deaths/usa/weekly_10y"
)
save_csv_zip(
  result_20y |> arrange(iso3c, year, week, age_group),
  "deaths/usa/weekly_20y"
)

# source("./mortality/usa/deaths_weekly.r")
