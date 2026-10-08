# Normalize each export before binding historical and refreshed tranches.
# CDC added "(without Puerto Rico)" to the residence-state column in 2026.
normalize_weekly_state_column <- function(df) {
  state_columns <- grep(
    "^Residence State( \\([^)]*\\))?$", names(df), value = TRUE
  )
  if (length(state_columns) != 1) {
    stop("Expected exactly one Residence State column in weekly WONDER export")
  }
  names(df)[names(df) == state_columns[[1]]] <- "Residence State"
  df
}

# Same integer-only rule as as_integer(), applied to an entire column.
parse_weekly_deaths <- function(values) {
  values[!grepl("^[0-9]+$", values)] <- NA_character_
  as.integer(values)
}
