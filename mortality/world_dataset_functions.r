# Drop daily rows belonging to a (iso3c, source, age_group, type) source-period
# whose total deaths is implausibly low compared to neighbouring periods of the
# same series. This catches partial / sentinel rows that a publisher emits at
# the leading edge of their data (e.g. CDC publishing an essentially empty
# Feb 2025 monthly bucket which then wins the priority race against
# mortality.org's complete row — see issue #17).
#
# The rule is data-shape-driven, not country-specific: for each source-period,
# compare its total deaths against the median of up to `window` neighbouring
# periods of the same series (excluding self). Drop the period if its total is
# below `min_ratio` of that neighbour median (and the median is non-trivial).
filter_anomalous_periods <- function(
    dd,
    window = 12,
    min_ratio = 0.2,
    min_neighbour_median = 100) {
  if (nrow(dd) == 0) return(dd)
  if (!all(c("type", "source", "age_group", "deaths") %in% names(dd))) {
    return(dd)
  }

  # Source-native period id per row
  dd_marked <- dd |>
    mutate(period_id = case_when(
      .data$type == 3 ~ as.character(tsibble::yearweek(.data$date)),
      .data$type == 2 ~ as.character(tsibble::yearmonth(.data$date)),
      TRUE            ~ as.character(year(.data$date))
    ))

  # Recover the original source-row total deaths by summing the daily rows
  # produced by expand_daily() for each source-period.
  totals <- dd_marked |>
    group_by(.data$iso3c, .data$source, .data$age_group, .data$type, .data$period_id) |>
    summarise(period_deaths = sum(.data$deaths, na.rm = TRUE), .groups = "drop") |>
    arrange(.data$iso3c, .data$source, .data$age_group, .data$type, .data$period_id)

  # For each row: median of up to `window` surrounding rows of the same series
  # (centred, excluding self). Uses base R rather than zoo::rollapply to avoid
  # leading/trailing NA blocks at the edges where we most need the check.
  totals <- totals |>
    group_by(.data$iso3c, .data$source, .data$age_group, .data$type) |>
    mutate(neighbour_median = neighbour_median_excluding_self(.data$period_deaths, window)) |>
    ungroup()

  # Only flag rows that are (a) far below their neighbours' median AND
  # (b) belong to a series with non-trivial volume — Poisson noise alone can
  # drop a tiny series (e.g. <100 deaths/month) below 20 % of its median,
  # so we don't second-guess publishers in that regime.
  bad <- totals |>
    filter(
      !is.na(.data$neighbour_median),
      .data$neighbour_median >= min_neighbour_median,
      .data$period_deaths < min_ratio * .data$neighbour_median
    )

  if (nrow(bad) > 0) {
    bad |>
      mutate(msg = sprintf(
        "[filter_anomalous_periods] dropping %s/%s/%s type=%s %s: deaths=%g vs neighbour median=%g",
        .data$iso3c, .data$source, .data$age_group, .data$type,
        .data$period_id, .data$period_deaths, .data$neighbour_median
      )) |>
      pull("msg") |>
      walk(message)
  }

  dd_marked |>
    anti_join(
      bad |> select("iso3c", "source", "age_group", "type", "period_id"),
      by = c("iso3c", "source", "age_group", "type", "period_id")
    ) |>
    select(-"period_id")
}

# Median of up to `window` neighbours centred on each position, excluding self.
# Falls back to the median of whatever neighbours exist (so edge positions still
# get a check), and returns NA only when there are zero neighbours.
neighbour_median_excluding_self <- function(x, window) {
  n <- length(x)
  half <- window %/% 2
  vapply(seq_len(n), function(i) {
    lo <- max(1, i - half)
    hi <- min(n, i + half)
    others <- x[setdiff(lo:hi, i)]
    if (length(others) == 0) return(NA_real_)
    median(others, na.rm = TRUE)
  }, numeric(1))
}

# Recompute period-level (yearly / fluseason / midyear) life expectancy from
# annual age-stratified mortality, using calculate_e0() from lib/life_expectancy.r.
#
# Why this exists (issue #17, also #16):
# When the LE source is sub-yearly (eurostat monthly LE for DEU, mortality.org
# weekly LE for USA, ...), each published `le` value is itself an annualised
# *single-period* estimate with strong seasonality (DEU 2025 monthly LEs swing
# 64-72). Averaging those — which is what the default mean(le) aggregator does
# — gives the seasonal mean of an estimator that wasn't designed to be
# averaged, not the country's true annual LE. The only mathematically
# defensible aggregation is to recompute LE from the year's age-stratified
# mortality sums.
#
# `dd_age_input` is the priority-filtered, daily-expanded age-stratified frame
# that feeds the period aggregation (dd_age_yearly for "year",
# dd_age_monthly for "fluseason"/"midyear"). Returns a tibble with columns
# (iso3c, period, le_recomputed, has_subyearly_le_source).
recompute_period_le <- function(dd_age_input, type) {
  empty <- tibble(
    iso3c = character(), period = character(),
    le_recomputed = numeric(), has_subyearly_le_source = logical()
  )
  if (!type %in% c("year", "fluseason", "midyear")) return(empty)
  if (nrow(dd_age_input) == 0) return(empty)
  required <- c("age_group", "deaths", "population", "type", "source", "n_age_groups")
  if (!all(required %in% names(dd_age_input))) return(empty)

  fun <- get(type)
  dd <- dd_age_input |> mutate(period = as.character(fun(.data$date)))

  # Per (iso3c, period): does any contributing row come from a sub-yearly LE
  # source? If `le_source_type` was attached upstream, prefer it; otherwise
  # fall back to the row's own `type` (which equals the LE source type in the
  # age-stratified path, since LE is computed per source row).
  src_col <- if ("le_source_type" %in% names(dd)) "le_source_type" else "type"
  has_sub <- dd |>
    group_by(.data$iso3c, .data$period) |>
    summarise(
      has_subyearly_le_source = any(.data[[src_col]] != 1, na.rm = TRUE),
      .groups = "drop"
    )

  # Day-coverage per (iso3c, period, source): how many distinct days does this
  # source contribute to the period? Used to skip sources with incomplete
  # period coverage (e.g. CDC USA 2025 missing Feb after the anomaly filter
  # would otherwise produce an under-estimated mortality rate / over-estimated
  # LE if used for recomputation).
  coverage <- dd |>
    distinct(.data$iso3c, .data$period, .data$source, .data$type, .data$date) |>
    group_by(.data$iso3c, .data$period, .data$source) |>
    summarise(
      coverage_days = n(),
      source_type = max(.data$type, na.rm = TRUE),
      .groups = "drop"
    )

  coverage <- coverage |>
    mutate(
      required_coverage_days = case_when(
        .env$type == "year" & .data$source_type == 3 ~ 357,
        TRUE ~ 365
      ),
      coverage_ratio = .data$coverage_days / .data$required_coverage_days
    )

  # Recompute LE per (iso3c, period, source, n_age_groups): sum daily deaths
  # back to period totals per age group within ONE source/age-scheme combo
  # (mixing schemes across sources would corrupt the life table), then run
  # calculate_e0 with annualize=FALSE since deaths are already annualised.
  per_source <- dd |>
    group_by(.data$iso3c, .data$period, .data$source, .data$n_age_groups,
             .data$age_group) |>
    summarise(
      deaths = sum(.data$deaths, na.rm = TRUE),
      population = mean(.data$population, na.rm = TRUE),
      .groups = "drop"
    ) |>
    group_by(.data$iso3c, .data$period, .data$source, .data$n_age_groups) |>
    group_modify(~ tibble(le_recomputed = calculate_e0(.x, annualize = FALSE))) |>
    ungroup() |>
    left_join(coverage, by = c("iso3c", "period", "source"))

  # Pick the best per (iso3c, period): require >=90% day coverage of the
  # period (else mortality rate is biased low → LE biased high), prefer
  # non-NA, then highest n_age_groups (matches dd_le_best's selection for
  # sub-yearly LE).
  recomputed <- per_source |>
    filter(
      !is.na(.data$le_recomputed),
      .data$coverage_ratio >= 0.9
    ) |>
    group_by(.data$iso3c, .data$period) |>
    arrange(desc(.data$n_age_groups), desc(.data$coverage_ratio)) |>
    slice(1) |>
    ungroup() |>
    select("iso3c", "period", "le_recomputed")

  recomputed |>
    left_join(has_sub, by = c("iso3c", "period")) |>
    mutate(has_subyearly_le_source = coalesce(.data$has_subyearly_le_source, FALSE))
}

# Override `le` in an aggregated period frame with the value from
# recompute_period_le() when (a) a recomputed value is available and (b) the
# original le was a mean of sub-yearly source values (i.e. structurally wrong
# under the default mean(le) aggregation — see issue #17). Yearly-source LE
# values (destatis annual etc.) are kept untouched.
override_le_with_recomputed <- function(aggregated, recomputed) {
  if (nrow(aggregated) == 0 || !"le" %in% names(aggregated)) return(aggregated)
  if (nrow(recomputed) == 0) return(aggregated)

  # `aggregated$date` carries the period label (numeric year, or string
  # like "2024-2025"); coerce both sides to character for a robust join.
  rhs <- recomputed |>
    mutate(period_key = as.character(.data$period)) |>
    select("period_key", "le_recomputed", "has_subyearly_le_source")

  joined <- aggregated |>
    mutate(period_key = as.character(.data$date)) |>
    left_join(rhs, by = "period_key") |>
    select(-"period_key")

  joined |>
    mutate(
      le = if_else(
        !is.na(.data$le_recomputed) & coalesce(.data$has_subyearly_le_source, FALSE),
        round(.data$le_recomputed, 2),
        .data$le
      )
    ) |>
    select(-any_of(c("le_recomputed", "has_subyearly_le_source")))
}

# Keep head/tail periods only when they have enough daily rows to be considered
# complete. `n` is the standard threshold; `n_iso_week` (optional) is a relaxed
# threshold that applies when every row of the head/tail period was sourced
# from ISO weekly data (type == 3). ISO weeks straddle calendar-year
# boundaries, so a fully-published ISO year (W01..W52/W53) covers ~362
# calendar days of the nominal year — it never reaches the 365-day threshold
# even when the source is complete (see issue #18).
filter_by_complete_temp_values <- function(data, fun_name, n, n_iso_week = n) {
  keep_period <- function(df) {
    if (nrow(df) >= n) {
      return(TRUE)
    }
    if ("type" %in% names(df) && all(df$type == 3) && nrow(df) >= n_iso_week) {
      return(TRUE)
    }
    FALSE
  }

  filter_edge <- function(df) {
    if (nrow(df) == 0) {
      return(df)
    }
    df |>
      group_by(across(all_of(fun_name))) |>
      group_modify(~ if (keep_period(.x)) .x else .x[0, ]) |>
      ungroup()
  }
  start <- data |>
    filter(.data[[fun_name]] == head(data[[fun_name]], n = 1)) |>
    filter_edge()
  mid <- data |>
    filter(!.data[[fun_name]] %in% c(
      head(data, n = 1)[[fun_name]],
      tail(data, n = 1)[[fun_name]]
    ))
  end <- data |>
    filter(.data[[fun_name]] == tail(data, n = 1)[[fun_name]]) |>
    filter_edge()

  rbind(start, mid, end) |>
    group_by(across(all_of(c("iso3c", fun_name))))
}

# Pick the most common non-NA reason (first wins on ties). Returns
# `NA_character_` when there are no reasons. Used to propagate
# `le_unavailable_reason` through aggregations where multiple per-period
# rows collapse into one.
most_common_reason <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }
  vals <- x[!is.na(x)]
  if (length(vals) == 0) {
    return(NA_character_)
  }
  tab <- sort(table(vals), decreasing = TRUE)
  names(tab)[1]
}

aggregate_data <- function(data, type) {
  # Filter ends. For "year", allow ISO-weekly-sourced years (type == 3) to
  # pass at >= 51 weeks (357 days) since ISO weeks cannot cover all 365
  # calendar days of a year (issue #18). fluseason / midyear keep the strict
  # 365-day threshold for now — their boundaries don't line up with ISO
  # week 1, so the same relaxation isn't obviously safe there.
  result <- switch(type,
    "yearweek" = filter_by_complete_temp_values(data, type, 7),
    "yearmonth" = filter_by_complete_temp_values(data, type, 28),
    "yearquarter" = filter_by_complete_temp_values(data, type, 90),
    "year" = filter_by_complete_temp_values(data, type, 365, n_iso_week = 357),
    "fluseason" = filter_by_complete_temp_values(data, type, 365),
    "midyear" = filter_by_complete_temp_values(data, type, 365)
  )
  has_le <- "le" %in% names(data)
  has_le_reason <- "le_unavailable_reason" %in% names(data)

  if ("cmr" %in% names(data)) {
    if (has_le) {
      result <- result |>
        summarise(
          deaths = round(sum_if_not_empty(deaths)),
          population = round(mean(.data$population)),
          cmr = round(sum_if_not_empty(.data$cmr), digits = 1),
          le = round(mean(.data$le, na.rm = TRUE), 2),
          le_unavailable_reason = if (has_le_reason) {
            most_common_reason(.data$le_unavailable_reason)
          } else {
            NA_character_
          },
          n_age_groups_le = if ("n_age_groups_le" %in% names(data)) round(mean(.data$n_age_groups_le, na.rm = TRUE)) else if ("n_age_groups" %in% names(data)) round(mean(.data$n_age_groups, na.rm = TRUE)) else NA_real_,
          type = toString(unique(.data$type)),
          source = toString(unique(.data$source)),
          .groups = "drop"
        )
      if (all(is.na(result$le) | is.nan(result$le))) {
        # LE itself is uncomputable for every aggregated row; drop the column
        # but keep `le_unavailable_reason` so the frontend can still explain
        # why. If no reason was carried through either, drop it too.
        result <- result |> select(-le)
        if (all(is.na(result$le_unavailable_reason))) {
          result <- result |> select(-le_unavailable_reason)
        }
      } else {
        # LE is at least partially computable; the reason column is only
        # meaningful when LE is NA, so blank it out elsewhere.
        result <- result |>
          mutate(
            le_unavailable_reason = ifelse(
              is.na(.data$le), .data$le_unavailable_reason, NA_character_
            )
          )
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
          le_unavailable_reason = if (has_le_reason) {
            most_common_reason(.data$le_unavailable_reason)
          } else {
            NA_character_
          },
          n_age_groups_le = if ("n_age_groups_le" %in% names(data)) round(mean(.data$n_age_groups_le, na.rm = TRUE)) else if ("n_age_groups" %in% names(data)) round(mean(.data$n_age_groups, na.rm = TRUE)) else NA_real_,
          source_asmr = toString(unique(.data$source)),
          source_le = if (has_source_le) toString(unique(.data$source_le)) else NA_character_,
          .groups = "drop"
        )
      if (all(is.na(result$le) | is.nan(result$le))) {
        # See note in the cmr branch: keep `le_unavailable_reason` so the
        # frontend can still explain unavailability; drop only when nothing
        # was carried through.
        result <- result |> select(-le, -source_le)
        if (all(is.na(result$le_unavailable_reason))) {
          result <- result |> select(-le_unavailable_reason)
        }
      } else {
        result <- result |>
          mutate(
            le_unavailable_reason = ifelse(
              is.na(.data$le), .data$le_unavailable_reason, NA_character_
            )
          )
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
  if ("population" %in% colnames(data)) {
    data$population <- round(sma(data$population, n = n))
  }
  if ("le" %in% colnames(data)) {
    data$le <- round(sma(data$le, n = n), 2)
  }
  if ("le_adj" %in% colnames(data)) {
    data$le_adj <- round(sma(data$le_adj, n = n), 2)
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

# LE bin-bias correction (years) based on validation against single-age reference.
# Applied only to le_adj (sub-yearly adjusted LE), keeping raw le unchanged.
get_le_bin_bias <- function(n_age_groups_le) {
  ifelse(
    is.na(n_age_groups_le), 0,
    ifelse(n_age_groups_le == 19, 0.0551, ifelse(n_age_groups_le == 11, 0.0101, 0))
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
  df$le_adj <- trend + residual

  # Blend bin-structure correction into adjusted LE (default display path).
  # If n_age_groups_le is unavailable, no correction is applied.
  n_le <- if ("n_age_groups_le" %in% names(df)) df$n_age_groups_le else NA_real_
  df$le_adj <- round(df$le_adj - get_le_bin_bias(n_le), 2)

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
  parse_age_group_start <- function(age_group) {
    ag <- trimws(age_group)
    m <- regmatches(ag, regexpr("^[0-9]+", ag))
    if (length(m) == 0 || m == "") {
      return(Inf)
    }
    as.numeric(m)
  }

  canonicalize_age_groups <- function(age_groups) {
    ag <- unique(trimws(age_groups))
    ag <- ag[!is.na(ag) & ag != ""]
    has_all <- any(tolower(ag) == "all")
    bins <- ag[tolower(ag) != "all"]

    if (length(bins) > 0) {
      bins <- bins[order(vapply(bins, parse_age_group_start, numeric(1)), bins)]
    }

    age_groups_bins <- paste(bins, collapse = ", ")
    age_groups_canonical <- if (has_all) {
      if (nzchar(age_groups_bins)) {
        paste0("all, ", age_groups_bins)
      } else {
        "all"
      }
    } else {
      age_groups_bins
    }
    bin_schema_id <- if (length(bins) == 0) {
      "all"
    } else {
      gsub("[^0-9a-z]+", "_", tolower(age_groups_bins))
    }

    list(
      age_groups_bins = age_groups_bins,
      age_groups_canonical = age_groups_canonical,
      n_age_groups_meta = length(bins),
      bin_schema_id = bin_schema_id
    )
  }

  result <- tibble()
  for (code in unique(df$iso3c)) {
    df_country <- df |> filter(.data$iso3c == code)
    for (t in unique(df_country$type)) {
      df_country_type <- df_country |> filter(.data$type == t)
      for (s in unique(df_country_type$source)) {
        df_country_type_source <- df_country_type |> filter(.data$source == s)
        age_meta <- canonicalize_age_groups(unique(df_country_type_source$age_group))
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
            ),
            age_groups_bins = age_meta$age_groups_bins,
            age_groups_canonical = age_meta$age_groups_canonical,
            n_age_groups_meta = age_meta$n_age_groups_meta,
            bin_schema_id = age_meta$bin_schema_id,
            le_bin_bias_adj_years = get_le_bin_bias(age_meta$n_age_groups_meta)
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
