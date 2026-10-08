#!/usr/bin/env bash
set -euo pipefail

export SKIP_WONDER_FETCH="${SKIP_WONDER_FETCH:-0}"
# Preserve the existing job's monthly refresh default: cached provisional
# months otherwise remain unchanged. Weekly provisional exports always refresh.
export FULL_WONDER_REFRESH="${FULL_WONDER_REFRESH:-1}"
export WONDER_MAX_YEAR="${WONDER_MAX_YEAR:-$(date +%Y)}"

cd /opt/cronicle
[ -d wonder_dl ] || git clone "https://x-access-token:${GITHUB_TOKEN}@github.com/USMortality/wonder_dl.git"
cd wonder_dl
git fetch origin
git reset --hard origin/master
if [ ! -f scripts/refresh.sh ] || [ ! -f scripts/validate-weekly.cjs ] || [ ! -f scripts/provisional-years.sh ]; then
  echo "wonder_dl is missing required scripts. Update it to include CDC provisional-year detection before running this job." >&2
  exit 1
fi
source scripts/provisional-years.sh
bash scripts/refresh.sh

cd /opt/cronicle
[ -d charts ] || git clone "https://x-access-token:${GITHUB_TOKEN}@github.com/MortalityWatch/data.git" charts
cd charts
git remote set-url origin "https://x-access-token:${GITHUB_TOKEN}@github.com/MortalityWatch/data.git"
git fetch origin
git reset --hard origin/master
echo "charts HEAD: $(git rev-parse --short HEAD) $(git log -1 --pretty=%s)"
./run_us_dataset.sh
