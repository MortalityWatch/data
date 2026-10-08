source("lib/wonder_weekly.r")

old <- data.frame("Residence State" = "Alabama", Deaths = "1313", check.names = FALSE)
new <- data.frame(
  "Residence State (without Puerto Rico)" = "Alaska",
  Deaths = "109", check.names = FALSE
)
combined <- rbind(normalize_weekly_state_column(old), normalize_weekly_state_column(new))
stopifnot(identical(combined[["Residence State"]], c("Alabama", "Alaska")))
stopifnot(identical(combined$Deaths, c("1313", "109")))
bad <- data.frame("Residence State Code" = "01", check.names = FALSE)
stopifnot(inherits(try(normalize_weekly_state_column(bad), silent = TRUE), "try-error"))
counts <- c("1313", "0", "Suppressed", "Not Available", NA_character_, "1.5", "-1")
stopifnot(identical(parse_weekly_deaths(counts), c(1313L, 0L, NA_integer_, NA_integer_, NA_integer_, NA_integer_, NA_integer_)))
# Vectorized dates must preserve the existing week-53 rollover semantics.
years <- c(2018L, 2020L, 2025L, 2026L)
weeks <- c(1L, 53L, 53L, 1L)
scalar_dates <- vapply(seq_along(years), function(i) {
  as.character(tsibble::make_yearweek(years[i], weeks[i]))
}, character(1))
stopifnot(identical(as.character(tsibble::make_yearweek(years, weeks)), scalar_dates))
cat("Weekly state header regression checks passed.\n")
