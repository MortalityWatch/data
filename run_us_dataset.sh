#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

# Fail before the first R script uploads anything, including when fetch is skipped.
node ../wonder_dl/scripts/validate-weekly.cjs

Rscript mortality/usa/deaths_weekly.r
Rscript mortality/usa/deaths_weekly_25y_20y_10y.r
Rscript mortality/usa/deaths_monthly.r
Rscript mortality/usa/deaths_yearly.r
