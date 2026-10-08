#!/usr/bin/env bash
set -euo pipefail

cd "$(dirname "$0")"

# The R importers also read ../wonder_dl/data_wonder. Keep validation tied to
# that same checkout so we never validate a different set of inputs.
wonder_validator="../wonder_dl/scripts/validate-weekly.cjs"
if [ ! -f "$wonder_validator" ]; then
  echo "Missing $wonder_validator. Clone/update USMortality/wonder_dl beside this repository, including PR #8, before publishing US datasets." >&2
  exit 1
fi
if ! command -v node >/dev/null 2>&1; then
  echo "Node.js is required to validate weekly WONDER inputs before publication." >&2
  exit 1
fi
if [ ! -f ../wonder_dl/scripts/provisional-years.sh ]; then
  echo "Update the sibling wonder_dl checkout to include scripts/provisional-years.sh before publishing." >&2
  exit 1
fi
source ../wonder_dl/scripts/provisional-years.sh

# Fail before the first R script uploads anything, including when fetch is skipped.
node "$wonder_validator"

Rscript mortality/usa/deaths_weekly.r
Rscript mortality/usa/deaths_weekly_25y_20y_10y.r
Rscript mortality/usa/deaths_monthly.r
Rscript mortality/usa/deaths_yearly.r
