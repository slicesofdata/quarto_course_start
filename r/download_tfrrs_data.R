zip_url <- "https://github.com/slicesofdata/dataviz23/raw/main/data/tfrrs.zip"

download.file(zip_url, destfile = here::here("data", "tfrrs.zip"))

unzip(here::here("data", "tfrrs.zip"), 
      overwrite = TRUE, 
      exdir = here::here("data")
)

