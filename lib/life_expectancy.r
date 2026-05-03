# Life Expectancy Calculation using Chiang's Method
# Pure R implementation for abridged life tables
#
# Methodology:
# ------------
# Life expectancy is calculated using Chiang's method for abridged life tables.
# This is the standard demographic approach when data is available in age groups
# rather than single years of age.
#
# Key Parameters:
# - nax: Average years lived in interval by those who die. Critical for accuracy.
#   * Infant mortality (age 0): Deaths concentrated early -> nax ≈ 0.1-0.3
#   * Ages 1-4: Deaths more evenly distributed -> nax ≈ 1.5
#   * Ages 5+: Approximately midpoint -> nax ≈ n/2
#   * Open-ended (85+): Uses 1/Mx (Keyfitz approximation)
#
# Coale-Demeny Coefficients:
# Empirically derived formulas that estimate nax based on mortality level.
# Originally published by Coale & Demeny (1966), refined by Preston et al. (2001).
#
# Data Quality Requirements:
# For reliable LE estimates, data should have:
# - First age group starting at 0
# - First age group width <= 15 years (0-4, 0-9, or 0-14)
# - Open-ended terminal age group (80+, 85+, 90+)
# - Complete age coverage (no gaps)
#
# References:
# - Chiang, C.L. (1984). The Life Table and Its Applications
# - Preston, S.H., Heuveline, P., & Guillot, M. (2001). Demography: Measuring
#   and Modeling Population Processes
# - Coale, A.J. & Demeny, P. (1966). Regional Model Life Tables and Stable
#   Populations

# Maximum width of first age group for reliable LE calculation
# Groups broader than this (e.g., 0-24, 0-44) are too heterogeneous
MAX_FIRST_AGE_GROUP_WIDTH <- 15

#' Estimate nax (average years lived in interval by those who die)
#'
#' Uses Coale-Demeny coefficients for infant/child mortality and simplified
#' UN method for other ages. Handles various first age group widths:
#' - 0-1 (single year): Standard Coale-Demeny
#' - 0-4 (5 years): Extended Coale-Demeny (Preston et al. 2001)
#' - 0-9 (10 years): Weighted approximation (~70% infant mortality)
#' - 0-14 (15 years): Weighted approximation (~65% infant mortality)
#'
#' @param nMx Age-specific mortality rates
#' @param ages Age group starts
#' @param AgeInt Age interval widths
#' @param sex Character: "m", "f", or "t"
#' @return Numeric vector of nax values
estimate_nax <- function(nMx, ages, AgeInt, sex = "t") {
  n_ages <- length(ages)
  nax <- rep(NA, n_ages)

  for (i in 1:n_ages) {
    age <- ages[i]
    n <- if (is.na(AgeInt[i])) 5 else AgeInt[i]

    if (age == 0) {
      # First age group starting at 0 - use appropriate Coale-Demeny variant
      m0 <- nMx[i]

      if (n == 1) {
        # Standard Coale-Demeny for single-year infant (age 0)
        nax[i] <- if (sex == "m") {
          if (m0 >= 0.107) 0.330 else 0.045 + 2.684 * m0
        } else if (sex == "f") {
          if (m0 >= 0.107) 0.350 else 0.053 + 2.800 * m0
        } else {
          if (m0 >= 0.107) 0.340 else 0.049 + 2.742 * m0
        }

      } else if (n == 5) {
        # Coale-Demeny for 0-4 combined (Preston et al. 2001)
        nax[i] <- if (sex == "m") {
          if (m0 >= 0.107) 1.651 else 0.0425 + 2.875 * m0
        } else if (sex == "f") {
          if (m0 >= 0.107) 1.750 else 0.0480 + 2.948 * m0
        } else {
          if (m0 >= 0.107) 1.700 else 0.0453 + 2.911 * m0
        }

      } else if (n == 10) {
        # 0-9: Approximate using weighted infant concentration
        # Weights are empirical approximations based on typical mortality patterns
        # in populations where ~70% of 0-9 deaths occur in age 0
        # Note: m0 here is group mortality rate, used as proxy for infant mortality
        a0 <- if (m0 >= 0.107) 0.34 else 0.049 + 2.742 * m0
        nax[i] <- 0.70 * a0 + 0.20 * (1 + 1.5) + 0.10 * (5 + 2.5)

      } else if (n <= 15) {
        # 0-14: Similar approach, deaths concentrated in infancy
        # Weights are empirical approximations (~65% in age 0, ~20% in 1-4)
        a0 <- if (m0 >= 0.107) 0.34 else 0.049 + 2.742 * m0
        nax[i] <- 0.65 * a0 + 0.20 * (1 + 1.5) + 0.15 * (5 + 5)

      } else {
        # Broader than 0-14: fallback for direct calls to estimate_nax()
        # Note: calculate_le_all_ages() and calculate_e0() return NA for these
        # cases before reaching here, so this only applies to direct usage
        nax[i] <- n / 3
      }

    } else if (age == 1 && n == 4) {
      # Ages 1-4: Coale-Demeny (depends on infant mortality level)
      m0 <- nMx[1]
      nax[i] <- if (sex == "m") {
        if (m0 >= 0.107) 1.352 else 1.651 - 2.816 * m0
      } else if (sex == "f") {
        if (m0 >= 0.107) 1.361 else 1.522 - 1.518 * m0
      } else {
        if (m0 >= 0.107) 1.356 else 1.587 - 2.167 * m0
      }

    } else if (i == n_ages) {
      # Open-ended interval: use 1/Mx if available
      if (nMx[i] > 0) {
        nax[i] <- 1 / nMx[i]
      } else {
        nax[i] <- n / 2
      }

    } else {
      # Standard: deaths distributed evenly through interval
      nax[i] <- n / 2
    }
  }

  # Handle any remaining NAs
  nax[is.na(nax)] <- 2.5 # Default for 5-year groups

  nax
}

#' Chiang life table calculation
#'
#' Pure R implementation of Chiang's method for abridged life tables.
#'
#' @param nMx Age-specific mortality rates (deaths/population, NOT per 100k)
#' @param ages Age group starts
#' @param AgeInt Age interval widths
#' @param sex Character: "m", "f", or "t" for nax estimation
#' @return List with life table columns including ex (life expectancy)
chiang_life_table <- function(nMx, ages, AgeInt, sex = "t") {
  n_ages <- length(ages)

  # Estimate nax (average years lived in interval by those who die)
  nax <- estimate_nax(nMx, ages, AgeInt, sex)

  # Replace NA interval width for open-ended group (terminal age only)
  # In standard life tables, only the last age group is open-ended (e.g., "85+")
  # We use Keyfitz approximation: n = 1/Mx for the terminal interval
  n <- AgeInt
  n[is.na(n)] <- 1 / nMx[n_ages] # Use 1/Mx for open interval (Keyfitz)
  n[is.na(n) | is.infinite(n)] <- 25 # Fallback if Mx is 0

  # Calculate probability of death (Chiang's formula)
  nqx <- (n * nMx) / (1 + (n - nax) * nMx)
  nqx[n_ages] <- 1 # Terminal age group: everyone dies
  nqx[nqx > 1] <- 1
  nqx[nqx < 0] <- 0
  nqx[is.na(nqx)] <- 0

  # Survival probability
  npx <- 1 - nqx

  # Survivors (radix = 100,000)
  lx <- numeric(n_ages)
  lx[1] <- 100000
  for (i in 2:n_ages) {
    lx[i] <- lx[i - 1] * npx[i - 1]
  }

  # Deaths
  ndx <- lx * nqx

  # Person-years lived
  nLx <- n * lx - (n - nax) * ndx

  # For open-ended interval, use Lx = lx / Mx
  if (nMx[n_ages] > 0) {
    nLx[n_ages] <- lx[n_ages] / nMx[n_ages]
  }

  # Total person-years remaining
  Tx <- rev(cumsum(rev(nLx)))

  # Life expectancy
  ex <- Tx / lx
  ex[is.na(ex) | is.infinite(ex)] <- 0

  list(
    Age = ages,
    AgeInt = AgeInt,
    nMx = nMx,
    nax = nax,
    nqx = nqx,
    lx = lx,
    ndx = ndx,
    nLx = nLx,
    Tx = Tx,
    ex = ex
  )
}

#' Calculate life expectancy at birth (e0) from age-stratified mortality data
#'
#' @param df Data frame with age_group, deaths, population columns.
#'   Deaths should be daily counts (annual deaths / 365) when annualize=TRUE.
#' @param sex Character: "m", "f", or "t" (default: "t")
#' @param annualize Logical: if TRUE (default), multiply mortality rates by 365
#'   to convert daily rates to annual rates for life table calculation.
#'   Set to FALSE if input data already has annual death counts.
#' @return Numeric: life expectancy at birth (e0)
calculate_e0 <- function(df, sex = "t", annualize = TRUE) {
  # Need at least some data
  if (nrow(df) == 0) {
    return(NA_real_)
  }

  # Parse age groups to get numeric starts
  age_info <- parse_age_groups_for_le(df$age_group)

  if (is.null(age_info) || length(age_info$ages) < 2) {
    return(NA_real_)
  }

  # Calculate mortality rates (deaths per person, NOT per 100k)
  nMx <- df$deaths / df$population

  # Handle edge cases
  nMx[is.na(nMx) | is.infinite(nMx)] <- 0

  # Check if all mortality rates are zero (no deaths)
  if (all(nMx == 0)) {
    return(NA_real_)
  }

  # Annualize daily mortality rates to get annual rates for life table
  # The daily data has deaths = annual_deaths / 365, so we multiply back
  if (annualize) {
    nMx <- nMx * 365
  }

  # Ensure data is sorted by age
  ord <- order(age_info$ages)
  ages <- age_info$ages[ord]
  AgeInt <- age_info$intervals[ord]
  nMx <- nMx[ord]

  # Skip if first age group is too broad for reliable LE calculation
  if (ages[1] == 0 && !is.na(AgeInt[1]) && AgeInt[1] > MAX_FIRST_AGE_GROUP_WIDTH) {
    return(NA_real_)
  }

  # Build life table
  lt <- tryCatch(
    chiang_life_table(nMx, ages, AgeInt, sex),
    error = function(e) NULL
  )

  if (is.null(lt)) {
    return(NA_real_)
  }

  # Return e0 (life expectancy at birth)
  e0 <- lt$ex[1]
  if (is.na(e0) || is.nan(e0) || is.infinite(e0) || e0 <= 0 || e0 > 120) {
    return(NA_real_)
  }

  round(e0, 2)
}

#' Parse age groups to extract numeric age starts and intervals
#'
#' @param age_groups Character vector of age group labels
#' @return List with ages and intervals vectors, or NULL if parsing fails
#' @note Input should be pre-filtered to exclude "all" and NA age groups.
#'   If these are present, the function returns NULL.
parse_age_groups_for_le <- function(age_groups) {
  ages <- numeric(length(age_groups))
  intervals <- numeric(length(age_groups))

  for (i in seq_along(age_groups)) {
    ag <- age_groups[i]

    # Reject "all" or NA - caller should filter these out first
    if (is.na(ag) || ag == "all") {
      return(NULL)
    }

    # Handle open-ended (e.g., "85+", "90+")
    if (grepl("\\+$", ag)) {
      ages[i] <- as.numeric(sub("\\+$", "", ag))
      intervals[i] <- NA # Open-ended
      next
    }

    # Handle ranges (e.g., "0-4", "5-14", "15-24")
    if (grepl("-", ag)) {
      parts <- strsplit(ag, "-")[[1]]
      start <- as.numeric(parts[1])
      end <- as.numeric(parts[2])
      ages[i] <- start
      intervals[i] <- end - start + 1
      next
    }

    # Handle single ages (e.g., "0", "1")
    if (grepl("^[0-9]+$", ag)) {
      ages[i] <- as.numeric(ag)
      intervals[i] <- 1
      next
    }

    # Unknown format
    return(NULL)
  }

  list(ages = ages, intervals = intervals)
}

#' Calculate life expectancy at all ages from grouped mortality data
#'
#' This function calculates ex (remaining life expectancy) at each age
#' from age-stratified data. Used with group_modify().
#'
#' @param df Data frame with age_group, deaths, population columns.
#'   Deaths should be daily counts (annual deaths / 365) when annualize=TRUE.
#' @param annualize Logical: if TRUE (default), multiply mortality rates by 365
#'   to convert daily rates to annual rates. Set to FALSE for annual data.
#' @return Data frame with age_group, le, and le_unavailable_reason columns.
#'   `le_unavailable_reason` is `NA_character_` when LE is computable, otherwise
#'   a short machine-readable string explaining why LE is unavailable (e.g.
#'   "first_age_group_too_broad" when the source's first age bucket is wider
#'   than `MAX_FIRST_AGE_GROUP_WIDTH`). Frontends can read this column to
#'   render an explanation instead of silently disabling the metric.
calculate_le_all_ages <- function(df, annualize = TRUE) {
  empty_result <- tibble(
    age_group = character(),
    le = numeric(),
    le_unavailable_reason = character()
  )

  # Need at least some data
  if (nrow(df) == 0) {
    return(empty_result)
  }

  # Parse age groups to get numeric starts
  age_info <- parse_age_groups_for_le(df$age_group)

  if (is.null(age_info) || length(age_info$ages) < 2) {
    return(empty_result)
  }

  # Calculate mortality rates (deaths per person, NOT per 100k)
  nMx <- df$deaths / df$population

  # Handle edge cases
  nMx[is.na(nMx) | is.infinite(nMx)] <- 0

  # Check if all mortality rates are zero (no deaths)
  if (all(nMx == 0)) {
    return(empty_result)
  }

  # Annualize daily mortality rates to get annual rates for life table
  if (annualize) {
    nMx <- nMx * 365
  }

  # Ensure data is sorted by age
  ord <- order(age_info$ages)
  ages <- age_info$ages[ord]
  AgeInt <- age_info$intervals[ord]
  nMx <- nMx[ord]
  age_groups_sorted <- df$age_group[ord]

  # Skip if first age group is too broad for reliable LE calculation
  # (e.g., 0-24, 0-44, 0-64 from some data sources). Return NA `le` plus a
  # reason on every age row so downstream consumers (and ultimately the
  # frontend) can explain *why* LE is unavailable instead of silently
  # dropping the metric. See issue #15 / closed PR #20 for context.
  if (ages[1] == 0 && !is.na(AgeInt[1]) && AgeInt[1] > MAX_FIRST_AGE_GROUP_WIDTH) {
    return(tibble(
      age_group = age_groups_sorted,
      le = NA_real_,
      le_unavailable_reason = "first_age_group_too_broad"
    ))
  }

  # Build life table
  lt <- tryCatch(
    chiang_life_table(nMx, ages, AgeInt, "t"),
    error = function(e) NULL
  )

  if (is.null(lt)) {
    return(empty_result)
  }

  # Return ex for all ages
  ex <- lt$ex
  ex[is.na(ex) | is.nan(ex) | is.infinite(ex) | ex <= 0 | ex > 120] <- NA_real_

  tibble(
    age_group = age_groups_sorted,
    le = round(ex, 2),
    le_unavailable_reason = NA_character_
  )
}
