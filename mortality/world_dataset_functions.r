filter_by_complete_temp_values <- function(data, fun_name, n) {
  start <- data |>
    filter(.data[[fun_name]] == head(data[[fun_name]], n = 1)) |>
    group_by(across(all_of(fun_name))) |>
    filter(n() >= n) |>
    ungroup()
  mid <- data |>
    filter(!.data[[fun_name]] %in% c(
      head(data, n = 1)[[fun_name]],
      tail(data, n = 1)[[fun_name]]
    ))
  end <- data |>
    filter(.data[[fun_name]] == tail(data, n = 1)[[fun_name]]) |>
    group_by(across(all_of(fun_name))) |>
    filter(n() >= n) |>
    ungroup()

  rbind(start, mid, end) |>
    group_by(across(all_of(c("iso3c", fun_name))))
}

aggregate_data <- function(data, type) {
  # Filter ends
  result <- switch(type,
    "yearweek" = filter_by_complete_temp_values(data, type, 7),
    "yearmonth" = filter_by_complete_temp_values(data, type, 28),
    "yearquarter" = filter_by_complete_temp_values(data, type, 90),
    "year" = filter_by_complete_temp_values(data, type, 365),
    "fluseason" = filter_by_complete_temp_values(data, type, 365),
    "midyear" = filter_by_complete_temp_values(data, type, 365)
  )
  has_le <- "le" %in% names(data)

  if ("cmr" %in% names(data)) {
    if (has_le) {
      result <- result |>
        summarise(
          deaths = round(sum_if_not_empty(deaths)),
          population = round(mean(.data$population)),
          cmr = round(sum_if_not_empty(.data$cmr), digits = 1),
          le = round(mean(.data$le, na.rm = TRUE), 2),
          type = toString(unique(.data$type)),
          source = toString(unique(.data$source)),
          .groups = "drop"
        )
      if (all(is.na(result$le) | is.nan(result$le))) {
        result <- result |> select(-le)
      }
    } else {
      result <- result |>
        summarise(
          deaths = round(sum_if_not_empty(deaths)),
          population = round(mean(.data$population)),
          cmr = round(sum_if_not_empty(.data$cmr), digits = 1),
          type = toString(unique(.data$type)),
          source = toString(unique(.data$source)),
          .groups = "drop"
        )
    }
  }

  if ("asmr_who" %in% names(data)) {
    has_source_le <- "source_le" %in% names(data)
    if (has_le) {
      result <- result |>
        summarise(
          asmr_who = round(sum_if_not_empty(.data$asmr_who), digits = 1),
          asmr_esp = round(sum_if_not_empty(.data$asmr_esp), digits = 1),
          asmr_usa = round(sum_if_not_empty(.data$asmr_usa), digits = 1),
          asmr_country = round(sum_if_not_empty(.data$asmr_country), digits = 1),
          le = round(mean(.data$le, na.rm = TRUE), 2),
          source_asmr = toString(unique(.data$source)),
          source_le = if (has_source_le) toString(unique(.data$source_le)) else NA_character_,
          .groups = "drop"
        )
      if (all(is.na(result$le) | is.nan(result$le))) {
        result <- result |> select(-le, -source_le)
      }
    } else {
      result <- result |>
        summarise(
          asmr_who = round(sum_if_not_empty(.data$asmr_who), digits = 1),
          asmr_esp = round(sum_if_not_empty(.data$asmr_esp), digits = 1),
          asmr_usa = round(sum_if_not_empty(.data$asmr_usa), digits = 1),
          asmr_country = round(sum_if_not_empty(.data$asmr_country), digits = 1),
          source_asmr = toString(unique(.data$source)),
          .groups = "drop"
        )
    }
  }

  result |>
    dplyr::rename("date" = all_of(type)) |>
    select(-"iso3c")
}

sma <- function(vec, n) {
  vec_len <- length(vec)
  res <- rep(NA, vec_len)
  if (n > vec_len) {
    return(res)
  }

  # Calculate SMA
  for (i in min(vec_len, (1 + n)):vec_len) {
    res[i] <- mean(vec[(i - n + 1):i])
  }

  xts::reclass(res, vec)
}

calc_sma <- function(data, n) {
  if (nrow(data) < n) {
    return(data[c(), ])
  }

  data$deaths <- round(sma(data$deaths, n = n), 3)
  data$cmr <- round(sma(data$cmr, n = n), 3)
  if ("asmr_who" %in% colnames(data)) {
    data$asmr_who <- round(sma(data$asmr_who, n = n), 3)
    data$asmr_esp <- round(sma(data$asmr_esp, n = n), 3)
    data$asmr_usa <- round(sma(data$asmr_usa, n = n), 3)
    data$asmr_country <- round(sma(data$asmr_country, n = n), 3)
  }
  data
}

get_period_multiplier <- function(chart_type) {
  if (chart_type %in% c("yearly", "fluseason", "midyear")) {
    return(1)
  } else if (chart_type == "quarterly") {
    return(4)
  } else if (chart_type == "monthly") {
    return(12)
  } else if (chart_type == "weekly") {
    return(52.143)
  } else {
    return(52) # SMA
  }
}

round_x <- function(data, col_name, digits = 0) {
  data |>
    mutate(
      "{col_name}_baseline" :=
        round(!!sym(paste0(col_name, "_baseline")), digits),
      "{col_name}_baseline_lower" :=
        round(!!sym(paste0(col_name, "_baseline_lower")), digits),
      "{col_name}_baseline_upper" :=
        round(!!sym(paste0(col_name, "_baseline_upper")), digits),
      "{col_name}_excess" :=
        round(!!sym(paste0(col_name, "_excess")), digits),
      "{col_name}_excess_lower" :=
        round(!!sym(paste0(col_name, "_excess_lower")), digits),
      "{col_name}_excess_upper" :=
        round(!!sym(paste0(col_name, "_excess_upper")), digits)
    )
}

summarize_data_all <- function(dd_all, dd_asmr, type) {
  a <- summarize_data_by_time(dd_all, type)
  if (nrow(dd_asmr) == 0) {
    return(a)
  }
  b <- summarize_data_by_time(dd_asmr, type)
  a |> left_join(b, by = c("iso3c", "age_group", "date"))
}

summarize_data_by_time <- function(df, type) {
  if (nrow(df) == 0) {
    return(df)
  }
  fun <- get(type)
  result <- df |>
    mutate(!!type := fun(date), .after = date) |>
    group_by(.data$iso3c, .data$age_group) |>
    group_modify(~ aggregate_data(.x, type), .keep = TRUE) |>
    ungroup()

  # Apply STL smoothing to LE for sub-yearly data
  if ("le" %in% names(result) && type %in% c("yearweek", "yearmonth", "yearquarter")) {
    result <- result |>
      group_by(.data$iso3c, .data$age_group) |>
      group_modify(~ smooth_le_stl(.x, type) |> select(-any_of(c("iso3c", "age_group"))), .keep = TRUE) |>
      ungroup()
  }

  result
}

#' Apply STL decomposition to create seasonally adjusted life expectancy
#'
#' @param df Data frame with date and le columns
#' @param type Period type: yearweek, yearmonth, yearquarter
#' @return Data frame with le (raw) and le_adj (seasonally adjusted = trend + residual)
smooth_le_stl <- function(df, type) {
  if (!"le" %in% names(df) || all(is.na(df$le))) {
    return(df)
  }

  # Determine frequency for STL
  freq <- switch(type,
    "yearweek" = 52,
    "yearmonth" = 12,
    "yearquarter" = 4,
    1
  )

  # Need at least 2 full cycles for STL
  min_periods <- freq * 2
  le_values <- df$le
  n_valid <- sum(!is.na(le_values))

  if (n_valid < min_periods) {
    return(df)
  }

  # Interpolate NAs for STL (it doesn't handle them)
  le_clean <- zoo::na.approx(le_values, na.rm = FALSE)
  le_clean <- zoo::na.locf(zoo::na.locf(le_clean, na.rm = FALSE), fromLast = TRUE, na.rm = FALSE)

  if (any(is.na(le_clean))) {
    return(df)
  }

  # Apply STL
  le_ts <- ts(le_clean, frequency = freq)
  stl_result <- tryCatch(
    stl(le_ts, s.window = "periodic"),
    error = function(e) NULL
  )

  if (is.null(stl_result)) {
    return(df)
  }

  # Seasonally adjusted = trend + residual (removes seasonal artifact only)
  trend <- as.numeric(stl_result$time.series[, "trend"])
  residual <- as.numeric(stl_result$time.series[, "remainder"])
  df$le_adj <- round(trend + residual, 2)

  df
}

fill_gaps_na <- function(df) {
  ts <- df |> as_tsibble(index = date)
  if (!tsibble::has_gaps(ts)) {
    return(ts)
  }
  ts |>
    tsibble::fill_gaps() |>
    tidyr::fill(population, .direction = "down") |>
    fill(source, .direction = "down")
}

save_info <- function(df, upload) {
  result <- tibble()
  for (code in unique(df$iso3c)) {
    df_country <- df |> filter(.data$iso3c == code)
    for (t in unique(df_country$type)) {
      df_country_type <- df_country |> filter(.data$type == t)
      for (s in unique(df_country_type$source)) {
        df_country_type_source <- df_country_type |> filter(.data$source == s)
        result <- rbind(
          result,
          tibble(
            iso3c = code,
            jurisdiction = head(df_country_type_source$jurisdiction, n = 1),
            type = t,
            source = s,
            min_date = min(df_country_type_source$date),
            max_date = max(df_country_type_source$date),
            age_groups = paste(
              unique(df_country_type_source$age_group),
              collapse = ", "
            )
          )
        )
      }
    }
  }
  save_csv(result, "mortality/world_meta", upload)
}

expand_daily <- function(df) {
  cols <- c("deaths")
  ex_cols <- c("population")
  yearly <- df |>
    filter(.data$type == 1) |>
    get_daily_from_yearly(cols, ex_cols)
  monthly <- df |>
    filter(.data$type == 2) |>
    get_daily_from_monthly(cols, ex_cols)
  weekly <- df |>
    filter(.data$type == 3) |>
    get_daily_from_weekly(cols, ex_cols)

  rbind(yearly, monthly, weekly) |>
    arrange(date) |>
    group_by(age_group, type, source) |>
    mutate(across(population, ~ na.approx(., rule = 2, na.rm = FALSE))) |>
    ungroup()
}

write_dataset <- function(
    iso3c,
    ag,
    weekly,
    monthly,
    quarterly,
    yearly,
    by_fluseason,
    by_midyear) {
  postfix <- ifelse(ag == "all", "", paste0("_", ag))

  write_and_select <- function(data, name_prefix) {
    data |>
      select(-all_of("age_group")) |>
      write_csv(name = paste0("mortality/", iso3c, "/", name_prefix, postfix))
  }

  calc_and_write_sma <- function(data, weeks, name_suffix) {
    data |>
      calc_sma(weeks) |>
      select(-all_of("age_group")) |>
      filter(!is.na(.data$deaths)) |>
      write_csv(name = paste0("mortality/", iso3c, "/", name_suffix, postfix))
  }

  write_and_select(weekly, "weekly")
  calc_and_write_sma(weekly, 104, "weekly_104w_sma")
  calc_and_write_sma(weekly, 52, "weekly_52w_sma")
  calc_and_write_sma(weekly, 26, "weekly_26w_sma")
  calc_and_write_sma(weekly, 14, "weekly_13w_sma")

  write_and_select(monthly, "monthly")
  write_and_select(quarterly, "quarterly")
  write_and_select(yearly, "yearly")
  write_and_select(by_fluseason, "fluseason")
  write_and_select(by_midyear, "midyear")
}
