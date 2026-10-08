# Mortality.Watch Charts
Collection of R scripts, that download, combine, and display several data sources.
It's main purpose is the generation of mortality data for mortality.watch.

# S3
Data and Charts are stored on MinIO/S3 at s3.mortality.watch.

# Usage
Sample usage:
```
./install_r_deps.sh
Rscript update_datasets.r
```

For US CDC WONDER imports, clone `USMortality/wonder_dl` beside this repository
and run `./run_us_dataset.sh`. The runner resolves its working directory from
its own location, so it can be invoked from any directory. Both validation and
the existing R importers use the sibling `../wonder_dl` checkout; changing only
the validator path would validate different files from those being imported.
Node.js and the refresh/validation scripts from `USMortality/wonder_dl` PR #8
must be installed before running the US pipeline.

Use `cronicle_us_dataset.sh` as the replacement Cronicle job body after deploying
both repositories. `FULL_WONDER_REFRESH=1` deliberately preserves the original
job's default of refreshing monthly files for the configured provisional years.
Set it to `0` to reuse existing monthly files (including their provisional
counts); provisional weekly files still refresh. `SKIP_WONDER_FETCH=1` skips
downloads but never bypasses weekly input validation.

The job sources `wonder_dl/scripts/provisional-years.sh` to detect the last final
year from CDC's Current Final section and determine the provisional range.
Update the sibling downloader to include this script before deploying this
runner. Remove fixed `WONDER_MIN_YEAR` assignments from Cronicle to enable
detection; an explicit value remains an override. The detected policy is shared
by download and publication validation. Newly finalized years are refreshed
before the downloader marks them safe to cache.

See https://www.mortality.watch/ for the frontend.

# Configuration

## MinIO/S3
Set build args:
```
dokku docker-options:add cronicle build '--build-arg S3_SECRET=...'
```

## Cronicle/Job Scheduler
Runs on a cronicle job scheduler, daily.

Dokku config:
```
dokku config:set cronicle AWS_ACCESS_KEY_ID=minio \ 
AWS_DEFAULT_REGION=s3 \ 
AWS_S3_ENDPOINT=mortality.watch \ 
AWS_SECRET_ACCESS_KEY=your_secret_access_key_here \ 
CRONICLE_job_memory_max=0 \ 
CRONICLE_smtp_hostname=smtp-relay.sendinblue.com \ 
CRONICLE_smtp_port=587 \ 
CRONICLE_smtp_secure=false \ 
CRONICLE_mail_options__auth__pass=your_email_password_here \ 
CRONICLE_mail_options__auth__user=benmarten1986@gmail.com \ 
CRONICLE_email_from=cron@mortality.watch \
ZIP_PASSWORD=your_zip_password_here
```

Each cronicle job is setup like this:
```sh
#!/usr/bin/env bash

[ ! -d \"charts\" ] && git clone https://github.com/USMortality/charts.git
cd charts
git fetch
git reset origin/master --hard
Rscript script.r
```

Backup is in cronicle.txt.

# Data Format
This is the data format used in the world_dataset.r script.

Example:
   iso3c date       age_group deaths population  type n_age_groups source
   <chr> <date>     <chr>      <int>      <dbl> <dbl>        <dbl> <chr> 
 1 USA   1999-01-01 0-9         3094   39742004     2            9 cdc   
 2 USA   1999-01-01 10-19       1413   40297966     2            9 cdc   
 3 USA   1999-01-01 20-29       2813   38166486     2            9 cdc   
