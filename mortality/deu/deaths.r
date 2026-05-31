source("lib/common.r")
options(warn = 1)

# National, until 2020
df1 <- read_excel(
  "./data_static/sonderauswertung-sterbefaelle-endgueltige-daten.xlsx",
  sheet = "csv-12613-02",
  range = "B1:G99999"
) |> rename(
  jurisdiction = Gebiet, year = Jahre, age_group = Alter,
  sex = Geschlecht, week = Kalenderwoche, deaths = Sterbefaelle
)
df2 <- read_excel(
  "./data/sonderauswertung-sterbefaelle.xlsx",
  sheet = "csv-12613-01",
  range = "B1:G99999"
) |>
  rename(
    jurisdiction = Gebiet, year = Jahr, age_group = Alter,
    sex = Geschlecht, week = Kalenderwoche, deaths = Sterbefaelle
  )

result1 <- rbind(df1, df2) |>
  mutate(deaths = as.integer(deaths)) |>
  filter(sex == "Insgesamt", !is.na(deaths)) |>
  select(-sex)


# States
df1 <- read_excel(
  "./data_static/sonderauswertung-sterbefaelle-endgueltige-daten.xlsx",
  sheet = "csv-12613-09",
  range = "B1:G99999"
) |> rename(
  jurisdiction = Gebiet, year = Jahre, age_group = Alter,
  sex = Geschlecht, week = Kalenderwoche, deaths = Sterbefaelle
)
df2 <- read_excel(
  "./data/sonderauswertung-sterbefaelle.xlsx",
  sheet = "csv-12613-08",
  range = "B1:G99999"
) |> rename(
  jurisdiction = Gebiet, year = Jahr, age_group = Alter,
  sex = Geschlecht, week = Kalenderwoche, deaths = Sterbefaelle
)

result2 <- rbind(df1, df2) |>
  mutate(deaths = as.integer(deaths)) |>
  filter(sex == "Insgesamt", !is.na(deaths)) |>
  select(-sex)

df <- rbind(result1, result2) |>
  mutate(
    year = as.integer(year),
    week = as.integer(week),
    deaths = as.integer(deaths)
  ) |>
  filter(!is.na(deaths)) |>
  arrange(year, jurisdiction, age_group, week) |>
  distinct(jurisdiction, year, week, age_group, .keep_all = TRUE) |>
  select(jurisdiction, year, age_group, week, deaths)  # Explicitly select only valid columns

date <- now() %m-% weeks(2)
y <- year(date)
w <- week(date)

len <- nrow(df |> filter(
  jurisdiction == "Deutschland",
  year == y,
  week == w
))

save_csv(df, paste0("deaths/deu/Tote_", y, "_", sprintf("%02d", w)))
save_csv(df, paste0("deaths/deu/deaths"))

# source("./mortality/deu/deaths.r")
