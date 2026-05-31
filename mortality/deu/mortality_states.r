source("lib/common.r")

parse_age_groups <- function(df) {
  df$age_group <- sub("unter 1 Jahr", "0", df$age_group)
  df$age_group <- sub("unter 15 Jahre", "0-14", df$age_group)
  df$age_group <- sub("50 Jahre und mehr", "50+", df$age_group)
  df$age_group <- sub("85 Jahre und mehr", "85+", df$age_group)
  df$age_group <- sub("Insgesamt", "all", df$age_group)
  df$age_group <- sub("Alter unbekannt", NA, df$age_group)
  df$age_group <- sub("-Jährige", "", df$age_group)
  df$age_group <- sub("100 Jahre und mehr", "100+", df$age_group)
  df
}

summarize_age_groups <- function(x) {
  x |>
    mutate(
      age_group = case_when(
        age_group %in% c("30-34", "35-39") ~ "30-39",
        age_group %in% c("40-44", "45-49") ~ "40-49",
        age_group %in% c("50-54", "55-59") ~ "50-59",
        age_group %in% c("60-64", "65-69") ~ "60-69",
        age_group %in% c("70-74", "75-79") ~ "70-79",
        age_group %in% c("80-84", "85-89", "90-94", "95+") ~ "80+",
        .default = age_group
      )
    ) |>
    group_by_at(vars(-deaths)) |>
    summarise(deaths = sum(deaths), .groups = "drop")
}

de_states <- as_tibble(read.csv("./data_static/deu_states_iso3c.csv"))
# Read CSV with col_select to only read specific columns, avoiding any empty columns
url <- paste0(
  "https://s3.mortality.watch/data/deaths/deu/deaths.csv",
  "?cachebust=", as.numeric(Sys.time())
)
message("Reading remote file: ", url)
response <- GET(url)
stop_for_status(response)
content_text <- content(response, "text")
df <- read_csv(
  I(content_text),
  col_select = c(jurisdiction, year, age_group, week, deaths),
  show_col_types = FALSE
) |>
  inner_join(de_states, by = "jurisdiction")
rm(de_states)

# Genesis 12411-0005: Bevölkerung: Deutschland, Stichtag, Altersjahre
deaths_raw <- fetch_genesis_data(
  "https://apify.mortality.watch/destatis-genesis/12613-0003.csv.gz", 5, -4
)

# Process data
# Find both-sexes section: between blank Insgesamt separator and final Insgesamt total
# Data structure: column 1 has age labels, columns 2+ have year data
insgesamt_rows <- which(str_detect(deaths_raw[[1]], "Insgesamt"))
# Row indices: male total, female total, blank separator, both-sexes total
# We want rows between the 3rd (blank separator) and 4th (both-sexes total), inclusive of total
df_year <- deaths_raw %>%
  filter(row_number() > insgesamt_rows[3] & row_number() <= insgesamt_rows[4]) %>%
  rename(age_group = 1) %>%
  pivot_longer(cols = -age_group, names_to = "date", values_to = "deaths") |>
  parse_age_groups() |>
  mutate(
    age_group = case_when(
      age_group %in% c(
        "0", "1", "2", "3", "4", "5", "6", "7", "8", "9"
      ) ~ "0-9",
      age_group %in% c(
        "10", "11", "12", "13", "14", "15", "16", "17", "18", "19"
      ) ~ "10-19",
      age_group %in% c(
        "20", "21", "22", "23", "24", "25", "26", "27", "28", "29"
      ) ~ "20-29",
      age_group %in% c(
        "30", "31", "32", "33", "34", "35", "36", "37", "38", "39"
      ) ~ "30-39",
      age_group %in% c(
        "40", "41", "42", "43", "44", "45", "46", "47", "48", "49"
      ) ~ "40-49",
      age_group %in% c(
        "50", "51", "52", "53", "54", "55", "56", "57", "58", "59"
      ) ~ "50-59",
      age_group %in% c(
        "60", "61", "62", "63", "64", "65", "66", "67", "68", "69"
      ) ~ "60-69",
      age_group %in% c(
        "70", "71", "72", "73", "74", "75", "76", "77", "78", "79"
      ) ~ "70-79",
      age_group %in% c(
        "80", "81", "82", "83", "84", "85", "86", "87", "88", "89",
        "90", "91", "92", "93", "94", "95", "96", "97", "98", "99", "100+"
      ) ~ "80+",
      age_group == "all" ~ "all",
      TRUE ~ NA_character_
    )
  ) |>
  group_by(date, age_group) |>
  rowwise() |>
  mutate(deaths = as_integer(deaths)) |>
  ungroup() |>
  group_by(date, age_group) |>
  summarize(deaths = sum(deaths), .groups = "drop") |>
  filter(!is.na(age_group)) |>
  filter(date != "")  # Remove empty column names from Genesis CSV
df_year$iso3c <- "DEU"
df_year$type <- 1
df_year$date <- date(sprintf("%s-01-01", df_year$date))
df_year$n_age_groups <- length(unique(df_year$age_group)) - 1

# All Ages
df_all <- df |>
  filter(age_group == "Insgesamt") |>
  mutate(
    date = date_parse(paste(year, week, 1), format = "%G %V %u"),
    age_group = "all"
  ) |>
  select(iso3c, date, age_group, deaths)
df_all$type <- 3
df_all$n_age_groups <- 1

# By 10y age groups, national
df_age_d_7 <- df |>
  filter(jurisdiction == "Deutschland" & age_group != "Insgesamt") |>
  mutate(date = date_parse(paste(year, week, 1), format = "%G %V %u")) |>
  summarize_age_groups() |>
  select(iso3c, date, age_group, deaths)
df_age_d_7$type <- 3
df_age_d_7$n_age_groups <- 7

# By age group, states
df_age_states <- df |>
  filter(jurisdiction != "Deutschland" & age_group != "Insgesamt") |>
  mutate(date = date_parse(paste(year, week, 1), format = "%G %V %u")) |>
  group_by(iso3c, date, age_group) |>
  summarise(deaths = sum(deaths)) |>
  ungroup()
df_age_states$type <- 3
df_age_states$n_age_groups <- 4

rm(df)

# Population
source("population/deu/deu.r")

population <- de_population |>
  filter(year >= 1990) |> # Before 1990 data only for West-Germany available.
  mutate(date = date(sprintf("%d-12-31", year)), .after = iso3c) |>
  group_by(iso3c) |>
  nest() |>
  mutate(
    data = map(data, ~ . |>
      group_by(age_group) |>
      nest() |>
      mutate(data = lapply(data, interpolate_population)) |>
      unnest(cols = "data"))
  ) |>
  unnest(cols = "data") |>
  select(iso3c, date, age_group, population)
rm(de_population)

deu_mortality_states <- rbind(df_year, df_all, df_age_d_7, df_age_states) |>
  inner_join(population, by = c("iso3c", "date", "age_group")) |>
  arrange(iso3c, date, age_group) |>
  distinct(iso3c, date, age_group, n_age_groups, .keep_all = TRUE)

deu_mortality_states$source <- "destatis"

rm(df_year, df_all, df_age_d_7, df_age_states, population)

deu <- deu_mortality_states |> filter(iso3c == "DEU")
stopifnot(length(unique(deu$age_group)) == 11)
by <- deu_mortality_states |> filter(iso3c == "DEU-BY")
stopifnot(length(unique(by$age_group)) == 5)

# source("./mortality/deu/mortality_states.r")
