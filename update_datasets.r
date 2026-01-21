source("./lib/common.r")
options(timeout = 600)

urls <- list(
  c(
    paste0(
      "https://www.destatis.de/DE/Themen/Gesellschaft-Umwelt/Bevoelkerung/",
      "Sterbefaelle-Lebenserwartung/Publikationen/Downloads-Sterbefaelle/",
      "statistischer-bericht-sterbefaelle-tage-wochen-monate-aktuell-5126109",
      ".xlsx?__blob=publicationFile"
    ),
    "sonderauswertung-sterbefaelle.xlsx"
  ),
  c(
    paste0(
      "https://catalog.ourworldindata.org/garden/covid/latest/compact/",
      "compact.csv"
    ),
    "owid.csv"
  ),
  c(
    paste0(
      "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/",
      "demo_r_mwk_10/?format=TSV&compressed=true&i"
    ),
    "eurostat_weekly.tsv.gz"
  ),
  c(
    paste0(
      "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/",
      "demo_pjangroup/?format=TSV&compressed=true&i"
    ),
    "eurostat_population.tsv.gz"
  ),
  c(
    paste0(
      "https://ec.europa.eu/eurostat/api/dissemination/sdmx/2.1/data/",
      "tps00199/?format=TSV&compressed=true&i"
    ),
    "eurostat_fertility.tsv.gz"
  ),
  c(
    "https://apify.mortality.watch/mortality-org-stmf.csv",
    "mortality_org.csv"
  ),
  c(
    paste0(
      "https://github.com/akarlinsky/world_mortality/raw/main/",
      "world_mortality.csv"
    ),
    "world_mortality.csv"
  )
)

dir <- "data/"
for (url in urls) {
  print(paste0(dir, url[2]))
  retry_download(url[1], paste0(dir, url[2]))
}

# source("./update_datasets.r")
