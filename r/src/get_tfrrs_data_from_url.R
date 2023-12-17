library(rvest)
library(tidyverse)
library(magrittr)

#readr::read_csv(here::here("data", "swim", "mm8results1coldefaultcumsub.csv"), col_names = F) %>%
#  view_html()

##########################################################
# Extract links from a url
##########################################################
scrap_links_from_url <- function(url){
  # Create an html document from the url
  webpage <- xml2::read_html(url)
  Sys.sleep(10)
  # Extract the URLs
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  # Extract the link text
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  return(tibble::tibble(link = link_, url = url_))
}

##########################################################
##########################################################

###################################################################
# read tables and change names in list to link names
###################################################################
read_tables_from_page <- function(page) {
  # read tables from url to a list
  tables = page %>% rvest::read_html() %>% rvest::html_table()
  
  Sys.sleep(5)
  
  
  return(tables)
}  

var_names_keep <- c("Meet", "Meet Date", "Athlete", "Year", "Time", "Mark")
table_list <- lapply(table_list, function(x) { names(x) = gsub("Athletes", "Athlete", names(x)) }  )
#table_list <- lapply(table_list, function(x) { x[!(names(x) %in% c("Wind", "wind"))] }  )
lapply(table_list, function(x) { x[(names(x) %in% var_names_keep)] }  )

lapply(table_list, function(x) { dplyr::select(x, any(var_names_keep)) }  )

summarize(d, mean(X100.Time, na.rm = T), .by = "X100.Meet")

quantile_df <- function(x, probs = c(0.25, 0.5, 0.75)) {
  tibble(
    val = quantile(x, probs, na.rm = TRUE),
    quant = probs
  )
}
lapply(table_list, function(x) { print(dim(x)[2]) }  ) 
lapply(table_list, function(x) { print(names(x)) }  ) 

# add names based on extracted links
add_names_to_list_elements <- function(
    table_list,
    list_names
) {
  
  # set names to list elements
  names(table_list) = list_names
  
  return(table_list)
}

# then remove missing cols
add_new_cols <- function(table_list, 
                         team        = "",
                         event       = "",
                         season      = "",
                         location    = "",
                         remove_cols = c("wind")
                         ) {
  lapply(table_list, function(x) {

  # keep names that are not missing
  dat = select(x, names(x)[nzchar(names(x))])
  
  # clean names
  names(dat) = gsub(" ", "_", names(dat))
  
  # wind variable is missing after 2010, so remove from all
  dat = select(., -dplyr::any_of(remove_cols))
  
  #names(dat) = names(dat)[!tolower(names(dat)) %in% tolower(remove_cols)]
  
  
  # add new variables
  dat = dat %>% 
    mutate(., 
           Team     = team,
           Event    = event,
           Season   = season,
           Location = location,
    )
  
})
}

###################################################################
# Modify and save data frame
###################################################################
modify_and_save <- function(table_list, location = "Outdoor") {

for (x in 1:length(table_list)) {
  
  #location = location
  dat          = table_list[[x]]
  dat$Location = location
  dat$Season   = stringr::str_split_fixed(dat$Meet_Date, " ", 3)[,3]
  dat$Event    = names(table_list)[[x]]
  team         = unique(dat$Team)
  event        = unique(dat$Event)
  season       = unique(dat$Season)
  
  dat = dat %>% select(., Season, Location, Meet, Meet_Date, Team, Event, Athlete, Time)
  #print(dat)
  write.csv(dat,
            file = here::here("data", "tfrrs", 
                              paste(
                                paste(season, "CMS", team, location, event, sep = "_"), 
                                ".csv", sep = "")
            ),
            row.names = F
  )
}
}

##########################################################
# List of Urls ###########################################
##########################################################
# The outdoor URLs
stag_url_list_outdoor <- list(
  "2023" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=4153&season_hnd=608",
  "2022" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=3730&season_hnd=568",
  #"2021" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=3200&season_hnd=530",
  #"2020" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=2906&season_hnd=496",
  #"2019" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=2573&season_hnd=453",
  #"2018" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=2278&season_hnd=414",
  # NCAA?
  #"2017" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=1915&season_hnd=377",
  # NCAA?
  #"2016" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=1683&season_hnd=336",
  #"2015" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=1552&season_hnd=303",
  #"2014" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=1251&season_hnd=256",
  "2013" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=1047&season_hnd=221",
  "2012" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=863&season_hnd=191",
  "2011" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=695&season_hnd=158"
  #"2010" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=600&season_hnd=131"
)

athena_url_list_outdoor <- list(
  #"2023" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=4153&season_hnd=608",
  #"2022" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=3730&season_hnd=568",
  #"2021" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=3200&season_hnd=530",
  #"2020" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=2906&season_hnd=496",
  #"2019" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=2573&season_hnd=453",
  #"2018" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=2278&season_hnd=414",
  # NCAA?
  #"2017" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=1915&season_hnd=377",
  # NCAA?
  #"2016" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=1683&season_hnd=336",
  #"2015" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=1552&season_hnd=303",
  #"2014" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=1251&season_hnd=256",
  #"2013" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=1047&season_hnd=221",
  #"2012" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=863&season_hnd=191",
  #"2011" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=695&season_hnd=158",
  "2010" = "https://www.tfrrs.org/all_performances/CA_college_f_Claremont_Mudd_Scripps.html?list_hnd=600&season_hnd=131"
)

##########################################################
# RUNNING THE PROGRAM ####################################
##########################################################
# get url and links

# stag_url_list_outdoor[3] 
# page_url = stag_url_list_outdoor[[3]]
# page_url = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=1047&season_hnd=221"
# links <- scrap_links_from_url(page_url)
# page_data <- read_tables_from_page(page_url)
# table_list <- ''
# add_names_to_list_elements(page_data, links$link)
# table_list_new <- add_new_cols(table_list, team = "Stag")
# table_list_new

for (page_url in stag_url_list_outdoor) {
  #page_url = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=4153&season_hnd=608"
  
  message(paste("Reading:", page_url))
  
  links <- scrap_links_from_url(page_url) %>% 
    filter(., stringr::str_detect(url, "event")) %>%
    mutate(.,
         link = gsub(" ", "", as.character(link)),
         link = gsub(",", "", as.character(link)),
  )

  # get links and update tables in list
  page_data <- read_tables_from_page(page_url)
  
  # read tables if present
  if (length(page_data > 0)) {
    
    # replace list names with link names
    table_list <- add_names_to_list_elements(page_data, links$link)
    
    # add new columns
    table_list_new <- add_new_cols(table_list, team = "Stag")
    
    # then modify and save
    modify_and_save(table_list_new, location = "Outdoor")
    
  } else {
        message(paste("No tables available at url."))
  }
}

###################################################################
# Get Athena Data
###################################################################

for (page_url in athena_url_list_outdoor) {

  message(paste("Reading:", page_url))
  
  links <- scrap_links_from_url(page_url) %>% 
    filter(., stringr::str_detect(url, "event")) %>%
    mutate(.,
           link = gsub(" ", "", as.character(link)),
           link = gsub(",", "", as.character(link)),
    )
  
  # get links and update tables in list
  page_data <- read_tables_from_page(page_url)
  
  # read tables if present
  if (length(page_data > 0)) {
    
    # replace list names with link names
    table_list <- add_names_to_list_elements(page_data, links$link)
    
    # add new columns
    table_list_new <- add_new_cols(table_list, team = "Athena")
    
    # then modify and save
    modify_and_save(table_list_new, location = "Outdoor")
    
  } else {
    message(paste("No tables available at url."))
  }
}


##########################################################
# END RUNNING THE PROGRAM ################################
##########################################################  

events_with_8_cols <- c("2010_CMS_Athena_Outdoor_4x100.csv", "2010_CMS_Athena_Outdoor_4x1600.csv", 
  "2010_CMS_Athena_Outdoor_4x400.csv", "2010_CMS_Athena_Outdoor_4x800.csv", 
  "2010_CMS_Stag_Outdoor_4x100.csv", "2010_CMS_Stag_Outdoor_4x1600.csv", 
  "2010_CMS_Stag_Outdoor_4x400.csv", "2010_CMS_Stag_Outdoor_4x800.csv", 
  "2011_CMS_Athena_Outdoor_4x100.csv", "2011_CMS_Athena_Outdoor_4x400.csv", 
  "2011_CMS_Athena_Outdoor_DMR.csv", "2011_CMS_Athena_Outdoor_SMR.csv", 
  "2011_CMS_Stag_Outdoor_4x100.csv", "2011_CMS_Stag_Outdoor_4x400.csv", 
  "2011_CMS_Stag_Outdoor_DMR.csv", "2012_CMS_Athena_Outdoor_4x100.csv", 
  "2012_CMS_Athena_Outdoor_4x400.csv", "2012_CMS_Athena_Outdoor_DMR.csv", 
  "2012_CMS_Stag_Outdoor_4x100.csv", "2012_CMS_Stag_Outdoor_4x400.csv", 
  "2012_CMS_Stag_Outdoor_DMR.csv", "2013_CMS_Athena_Outdoor_4x100.csv", 
  "2013_CMS_Athena_Outdoor_4x400.csv", "2013_CMS_Athena_Outdoor_4x800.csv", 
  "2013_CMS_Athena_Outdoor_DMR.csv", "2013_CMS_Stag_Outdoor_4x100.csv", 
  "2013_CMS_Stag_Outdoor_4x400.csv", "2013_CMS_Stag_Outdoor_4x800.csv", 
  "2013_CMS_Stag_Outdoor_DMR.csv", "2014_CMS_Athena_Outdoor_4x100.csv", 
  "2014_CMS_Athena_Outdoor_4x400.csv", "2014_CMS_Athena_Outdoor_4x800.csv", 
  "2014_CMS_Athena_Outdoor_DMR.csv", "2014_CMS_Stag_Outdoor_4x100.csv", 
  "2014_CMS_Stag_Outdoor_4x400.csv", "2014_CMS_Stag_Outdoor_4x800.csv", 
  "2014_CMS_Stag_Outdoor_DMR.csv", "2015_CMS_Athena_Outdoor_4x100.csv", 
  "2015_CMS_Athena_Outdoor_4x400.csv", "2015_CMS_Athena_Outdoor_4x800.csv", 
  "2015_CMS_Athena_Outdoor_DMR.csv", "2015_CMS_Stag_Outdoor_4x100.csv", 
  "2015_CMS_Stag_Outdoor_4x400.csv", "2015_CMS_Stag_Outdoor_4x800.csv", 
  "2015_CMS_Stag_Outdoor_DMR.csv", "2016_CMS_Athena_Outdoor_4x100.csv", 
  "2016_CMS_Athena_Outdoor_4x400.csv", "2016_CMS_Athena_Outdoor_4x800.csv", 
  "2016_CMS_Athena_Outdoor_DMR.csv", "2016_CMS_Stag_Outdoor_4x100.csv", 
  "2016_CMS_Stag_Outdoor_4x400.csv", "2016_CMS_Stag_Outdoor_4x800.csv", 
  "2016_CMS_Stag_Outdoor_DMR.csv", "2017_CMS_Athena_Outdoor_4x100.csv", 
  "2017_CMS_Athena_Outdoor_4x400.csv", "2017_CMS_Athena_Outdoor_4x800.csv", 
  "2017_CMS_Athena_Outdoor_DMR.csv", "2017_CMS_Stag_Outdoor_4x100.csv", 
  "2017_CMS_Stag_Outdoor_4x400.csv", "2017_CMS_Stag_Outdoor_4x800.csv", 
  "2017_CMS_Stag_Outdoor_DMR.csv", "2018_CMS_Athena_Outdoor_4x100.csv", 
  "2018_CMS_Athena_Outdoor_4x400.csv", "2018_CMS_Athena_Outdoor_4x800.csv", 
  "2018_CMS_Athena_Outdoor_DMR.csv", "2018_CMS_Stag_Outdoor_4x100.csv", 
  "2018_CMS_Stag_Outdoor_4x400.csv", "2019_CMS_Athena_Outdoor_4x100.csv", 
  "2019_CMS_Athena_Outdoor_4x400.csv", "2019_CMS_Athena_Outdoor_DMR.csv", 
  "2019_CMS_Stag_Outdoor_4x100.csv", "2019_CMS_Stag_Outdoor_4x400.csv", 
  "2019_CMS_Stag_Outdoor_DMR.csv", "2020_CMS_Athena_Outdoor_4x100.csv", 
  "2020_CMS_Athena_Outdoor_4x400.csv", "2020_CMS_Athena_Outdoor_4x800.csv", 
  "2020_CMS_Athena_Outdoor_DMR.csv", "2020_CMS_Stag_Outdoor_4x100.csv", 
  "2020_CMS_Stag_Outdoor_4x400.csv", "2020_CMS_Stag_Outdoor_4x800.csv", 
  "2020_CMS_Stag_Outdoor_DMR.csv", "2022_CMS_Athena_Outdoor_4x100.csv", 
  "2022_CMS_Athena_Outdoor_4x400.csv", "2022_CMS_Athena_Outdoor_4x800.csv", 
  "2022_CMS_Athena_Outdoor_DMR.csv", "2022_CMS_Stag_Outdoor_4x100.csv", 
  "2022_CMS_Stag_Outdoor_4x400.csv", "2022_CMS_Stag_Outdoor_4x800.csv", 
  "2022_CMS_Stag_Outdoor_DMR.csv", "2022_CMS_Stag_Outdoor_SMR(800).csv", 
  "2023_CMS_Athena_Outdoor_4x100.csv", "2023_CMS_Athena_Outdoor_4x400.csv", 
  "2023_CMS_Stag_Outdoor_4x100.csv", "2023_CMS_Stag_Outdoor_4x400.csv"
)

events_with_9_cols <- c("2010_CMS_Athena_Outdoor_10000.csv", "2010_CMS_Athena_Outdoor_1500.csv", 
  "2010_CMS_Athena_Outdoor_3000.csv", "2010_CMS_Athena_Outdoor_3000S.csv", 
  "2010_CMS_Athena_Outdoor_400.csv", "2010_CMS_Athena_Outdoor_400H.csv", 
  "2010_CMS_Athena_Outdoor_5000.csv", "2010_CMS_Athena_Outdoor_800.csv", 
  "2010_CMS_Athena_Outdoor_Hep.csv", "2010_CMS_Stag_Outdoor_10000.csv", 
  "2010_CMS_Stag_Outdoor_1500.csv", "2010_CMS_Stag_Outdoor_3000.csv", 
  "2010_CMS_Stag_Outdoor_3000S.csv", "2010_CMS_Stag_Outdoor_400.csv", 
  "2010_CMS_Stag_Outdoor_400H.csv", "2010_CMS_Stag_Outdoor_5000.csv", 
  "2010_CMS_Stag_Outdoor_800.csv", "2010_CMS_Stag_Outdoor_Dec.csv", 
  "2011_CMS_Athena_Outdoor_10000.csv", "2011_CMS_Athena_Outdoor_1500.csv", 
  "2011_CMS_Athena_Outdoor_3000.csv", "2011_CMS_Athena_Outdoor_3000S.csv", 
  "2011_CMS_Athena_Outdoor_400.csv", "2011_CMS_Athena_Outdoor_400H.csv", 
  "2011_CMS_Athena_Outdoor_5000.csv", "2011_CMS_Athena_Outdoor_800.csv", 
  "2011_CMS_Athena_Outdoor_Hep.csv", "2011_CMS_Stag_Outdoor_10000.csv", 
  "2011_CMS_Stag_Outdoor_1500.csv", "2011_CMS_Stag_Outdoor_3000.csv", 
  "2011_CMS_Stag_Outdoor_3000S.csv", "2011_CMS_Stag_Outdoor_400.csv", 
  "2011_CMS_Stag_Outdoor_400H.csv", "2011_CMS_Stag_Outdoor_5000.csv", 
  "2011_CMS_Stag_Outdoor_800.csv", "2012_CMS_Athena_Outdoor_10000.csv", 
  "2012_CMS_Athena_Outdoor_1500.csv", "2012_CMS_Athena_Outdoor_3000.csv", 
  "2012_CMS_Athena_Outdoor_3000S.csv", "2012_CMS_Athena_Outdoor_400.csv", 
  "2012_CMS_Athena_Outdoor_400H.csv", "2012_CMS_Athena_Outdoor_5000.csv", 
  "2012_CMS_Athena_Outdoor_800.csv", "2012_CMS_Stag_Outdoor_10000.csv", 
  "2012_CMS_Stag_Outdoor_1500.csv", "2012_CMS_Stag_Outdoor_3000.csv", 
  "2012_CMS_Stag_Outdoor_3000S.csv", "2012_CMS_Stag_Outdoor_400.csv", 
  "2012_CMS_Stag_Outdoor_400H.csv", "2012_CMS_Stag_Outdoor_5000.csv", 
  "2012_CMS_Stag_Outdoor_800.csv", "2013_CMS_Athena_Outdoor_10000.csv", 
  "2013_CMS_Athena_Outdoor_1500.csv", "2013_CMS_Athena_Outdoor_3000.csv", 
  "2013_CMS_Athena_Outdoor_3000S.csv", "2013_CMS_Athena_Outdoor_400.csv", 
  "2013_CMS_Athena_Outdoor_400H.csv", "2013_CMS_Athena_Outdoor_5000.csv", 
  "2013_CMS_Athena_Outdoor_800.csv", "2013_CMS_Athena_Outdoor_Hep.csv", 
  "2013_CMS_Stag_Outdoor_10000.csv", "2013_CMS_Stag_Outdoor_1500.csv", 
  "2013_CMS_Stag_Outdoor_3000.csv", "2013_CMS_Stag_Outdoor_3000S.csv", 
  "2013_CMS_Stag_Outdoor_400.csv", "2013_CMS_Stag_Outdoor_400H.csv", 
  "2013_CMS_Stag_Outdoor_5000.csv", "2013_CMS_Stag_Outdoor_800.csv", 
  "2013_CMS_Stag_Outdoor_Dec.csv", "2014_CMS_Athena_Outdoor_1000.csv", 
  "2014_CMS_Athena_Outdoor_10000.csv", "2014_CMS_Athena_Outdoor_1500.csv", 
  "2014_CMS_Athena_Outdoor_3000.csv", "2014_CMS_Athena_Outdoor_3000S.csv", 
  "2014_CMS_Athena_Outdoor_400.csv", "2014_CMS_Athena_Outdoor_400H.csv", 
  "2014_CMS_Athena_Outdoor_5000.csv", "2014_CMS_Athena_Outdoor_800.csv", 
  "2014_CMS_Athena_Outdoor_Hep.csv", "2014_CMS_Athena_Outdoor_Mile.csv", 
  "2014_CMS_Stag_Outdoor_1000.csv", "2014_CMS_Stag_Outdoor_10000.csv", 
  "2014_CMS_Stag_Outdoor_1500.csv", "2014_CMS_Stag_Outdoor_3000.csv", 
  "2014_CMS_Stag_Outdoor_3000S.csv", "2014_CMS_Stag_Outdoor_400.csv", 
  "2014_CMS_Stag_Outdoor_400H.csv", "2014_CMS_Stag_Outdoor_5000.csv", 
  "2014_CMS_Stag_Outdoor_800.csv", "2014_CMS_Stag_Outdoor_Mile.csv", 
  "2015_CMS_Athena_Outdoor_1000.csv", "2015_CMS_Athena_Outdoor_10000.csv", 
  "2015_CMS_Athena_Outdoor_1500.csv", "2015_CMS_Athena_Outdoor_3000.csv", 
  "2015_CMS_Athena_Outdoor_3000S.csv", "2015_CMS_Athena_Outdoor_400.csv", 
  "2015_CMS_Athena_Outdoor_400H.csv", "2015_CMS_Athena_Outdoor_5000.csv", 
  "2015_CMS_Athena_Outdoor_800.csv", "2015_CMS_Athena_Outdoor_Hep.csv", 
  "2015_CMS_Athena_Outdoor_Mile.csv", "2015_CMS_Stag_Outdoor_1000.csv", 
  "2015_CMS_Stag_Outdoor_10000.csv", "2015_CMS_Stag_Outdoor_1500.csv", 
  "2015_CMS_Stag_Outdoor_3000.csv", "2015_CMS_Stag_Outdoor_3000S.csv", 
  "2015_CMS_Stag_Outdoor_400.csv", "2015_CMS_Stag_Outdoor_400H.csv", 
  "2015_CMS_Stag_Outdoor_5000.csv", "2015_CMS_Stag_Outdoor_800.csv", 
  "2015_CMS_Stag_Outdoor_Mile.csv", "2016_CMS_Athena_Outdoor_1000.csv", 
  "2016_CMS_Athena_Outdoor_10000.csv", "2016_CMS_Athena_Outdoor_1500.csv", 
  "2016_CMS_Athena_Outdoor_3000.csv", "2016_CMS_Athena_Outdoor_3000S.csv", 
  "2016_CMS_Athena_Outdoor_400.csv", "2016_CMS_Athena_Outdoor_400H.csv", 
  "2016_CMS_Athena_Outdoor_5000.csv", "2016_CMS_Athena_Outdoor_800.csv", 
  "2016_CMS_Athena_Outdoor_Mile.csv", "2016_CMS_Stag_Outdoor_10000.csv", 
  "2016_CMS_Stag_Outdoor_1500.csv", "2016_CMS_Stag_Outdoor_3000.csv", 
  "2016_CMS_Stag_Outdoor_3000S.csv", "2016_CMS_Stag_Outdoor_400.csv", 
  "2016_CMS_Stag_Outdoor_400H.csv", "2016_CMS_Stag_Outdoor_5000.csv", 
  "2016_CMS_Stag_Outdoor_800.csv", "2016_CMS_Stag_Outdoor_Mile.csv", 
  "2017_CMS_Athena_Outdoor_10000.csv", "2017_CMS_Athena_Outdoor_1500.csv", 
  "2017_CMS_Athena_Outdoor_3000.csv", "2017_CMS_Athena_Outdoor_3000S.csv", 
  "2017_CMS_Athena_Outdoor_400.csv", "2017_CMS_Athena_Outdoor_400H.csv", 
  "2017_CMS_Athena_Outdoor_5000.csv", "2017_CMS_Athena_Outdoor_800.csv", 
  "2017_CMS_Stag_Outdoor_10000.csv", "2017_CMS_Stag_Outdoor_1500.csv", 
  "2017_CMS_Stag_Outdoor_3000.csv", "2017_CMS_Stag_Outdoor_3000S.csv", 
  "2017_CMS_Stag_Outdoor_400.csv", "2017_CMS_Stag_Outdoor_400H.csv", 
  "2017_CMS_Stag_Outdoor_5000.csv", "2017_CMS_Stag_Outdoor_800.csv", 
  "2018_CMS_Athena_Outdoor_10000.csv", "2018_CMS_Athena_Outdoor_1500.csv", 
  "2018_CMS_Athena_Outdoor_3000.csv", "2018_CMS_Athena_Outdoor_3000S.csv", 
  "2018_CMS_Athena_Outdoor_400.csv", "2018_CMS_Athena_Outdoor_400H.csv", 
  "2018_CMS_Athena_Outdoor_5000.csv", "2018_CMS_Athena_Outdoor_800.csv", 
  "2018_CMS_Athena_Outdoor_Hep.csv", "2018_CMS_Athena_Outdoor_Mile.csv", 
  "2018_CMS_Stag_Outdoor_10000.csv", "2018_CMS_Stag_Outdoor_1500.csv", 
  "2018_CMS_Stag_Outdoor_3000S.csv", "2018_CMS_Stag_Outdoor_400.csv", 
  "2018_CMS_Stag_Outdoor_400H.csv", "2018_CMS_Stag_Outdoor_5000.csv", 
  "2018_CMS_Stag_Outdoor_800.csv", "2018_CMS_Stag_Outdoor_Dec.csv", 
  "2019_CMS_Athena_Outdoor_10000.csv", "2019_CMS_Athena_Outdoor_1500.csv", 
  "2019_CMS_Athena_Outdoor_3000.csv", "2019_CMS_Athena_Outdoor_3000S.csv", 
  "2019_CMS_Athena_Outdoor_400.csv", "2019_CMS_Athena_Outdoor_400H.csv", 
  "2019_CMS_Athena_Outdoor_5000.csv", "2019_CMS_Athena_Outdoor_800.csv", 
  "2019_CMS_Athena_Outdoor_Hep.csv", "2019_CMS_Athena_Outdoor_Mile.csv", 
  "2019_CMS_Stag_Outdoor_10000.csv", "2019_CMS_Stag_Outdoor_1500.csv", 
  "2019_CMS_Stag_Outdoor_3000.csv", "2019_CMS_Stag_Outdoor_3000S.csv", 
  "2019_CMS_Stag_Outdoor_400.csv", "2019_CMS_Stag_Outdoor_400H.csv", 
  "2019_CMS_Stag_Outdoor_5000.csv", "2019_CMS_Stag_Outdoor_800.csv", 
  "2019_CMS_Stag_Outdoor_Dec.csv", "2019_CMS_Stag_Outdoor_Mile.csv", 
  "2020_CMS_Athena_Outdoor_1500.csv", "2020_CMS_Athena_Outdoor_3000.csv", 
  "2020_CMS_Athena_Outdoor_3000S.csv", "2020_CMS_Athena_Outdoor_400.csv", 
  "2020_CMS_Athena_Outdoor_400H.csv", "2020_CMS_Athena_Outdoor_5000.csv", 
  "2020_CMS_Athena_Outdoor_800.csv", "2020_CMS_Athena_Outdoor_Mile.csv", 
  "2020_CMS_Stag_Outdoor_1500.csv", "2020_CMS_Stag_Outdoor_3000.csv", 
  "2020_CMS_Stag_Outdoor_3000S.csv", "2020_CMS_Stag_Outdoor_400.csv", 
  "2020_CMS_Stag_Outdoor_400H.csv", "2020_CMS_Stag_Outdoor_5000.csv", 
  "2020_CMS_Stag_Outdoor_800.csv", "2020_CMS_Stag_Outdoor_Mile.csv", 
  "2022_CMS_Athena_Outdoor_10000.csv", "2022_CMS_Athena_Outdoor_1500.csv", 
  "2022_CMS_Athena_Outdoor_3000.csv", "2022_CMS_Athena_Outdoor_3000S.csv", 
  "2022_CMS_Athena_Outdoor_400.csv", "2022_CMS_Athena_Outdoor_400H.csv", 
  "2022_CMS_Athena_Outdoor_5000.csv", "2022_CMS_Athena_Outdoor_800.csv", 
  "2022_CMS_Athena_Outdoor_Mile.csv", "2022_CMS_Stag_Outdoor_10000.csv", 
  "2022_CMS_Stag_Outdoor_1500.csv", "2022_CMS_Stag_Outdoor_3000.csv", 
  "2022_CMS_Stag_Outdoor_3000S.csv", "2022_CMS_Stag_Outdoor_400.csv", 
  "2022_CMS_Stag_Outdoor_400H.csv", "2022_CMS_Stag_Outdoor_5000.csv", 
  "2022_CMS_Stag_Outdoor_800.csv", "2022_CMS_Stag_Outdoor_Mile.csv", 
  "2023_CMS_Athena_Outdoor_10000.csv", "2023_CMS_Athena_Outdoor_1500.csv", 
  "2023_CMS_Athena_Outdoor_3000.csv", "2023_CMS_Athena_Outdoor_3000S.csv", 
  "2023_CMS_Athena_Outdoor_400.csv", "2023_CMS_Athena_Outdoor_400H.csv", 
  "2023_CMS_Athena_Outdoor_5000.csv", "2023_CMS_Athena_Outdoor_800.csv", 
  "2023_CMS_Athena_Outdoor_Mile.csv", "2023_CMS_Stag_Outdoor_10000.csv", 
  "2023_CMS_Stag_Outdoor_1500.csv", "2023_CMS_Stag_Outdoor_3000.csv", 
  "2023_CMS_Stag_Outdoor_3000S.csv", "2023_CMS_Stag_Outdoor_400.csv", 
  "2023_CMS_Stag_Outdoor_400H.csv", "2023_CMS_Stag_Outdoor_5000.csv", 
  "2023_CMS_Stag_Outdoor_800.csv", "2023_CMS_Stag_Outdoor_Mile.csv"
)
events_with_10_cols <- c("2010_CMS_Athena_Outdoor_100.csv", "2010_CMS_Athena_Outdoor_100H.csv", 
  "2010_CMS_Athena_Outdoor_200.csv", "2010_CMS_Athena_Outdoor_DT.csv", 
  "2010_CMS_Athena_Outdoor_HJ.csv", "2010_CMS_Athena_Outdoor_HT.csv", 
  "2010_CMS_Athena_Outdoor_JT.csv", "2010_CMS_Athena_Outdoor_PV.csv", 
  "2010_CMS_Athena_Outdoor_SP.csv", "2010_CMS_Stag_Outdoor_100.csv", 
  "2010_CMS_Stag_Outdoor_110H.csv", "2010_CMS_Stag_Outdoor_200.csv", 
  "2010_CMS_Stag_Outdoor_DT.csv", "2010_CMS_Stag_Outdoor_HJ.csv", 
  "2010_CMS_Stag_Outdoor_HT.csv", "2010_CMS_Stag_Outdoor_JT.csv", 
  "2010_CMS_Stag_Outdoor_PV.csv", "2010_CMS_Stag_Outdoor_SP.csv", 
  "2011_CMS_Athena_Outdoor_100.csv", "2011_CMS_Athena_Outdoor_100H.csv", 
  "2011_CMS_Athena_Outdoor_200.csv", "2011_CMS_Athena_Outdoor_DT.csv", 
  "2011_CMS_Athena_Outdoor_HJ.csv", "2011_CMS_Athena_Outdoor_JT.csv", 
  "2011_CMS_Athena_Outdoor_PV.csv", "2011_CMS_Athena_Outdoor_SP.csv", 
  "2011_CMS_Stag_Outdoor_100.csv", "2011_CMS_Stag_Outdoor_110H.csv", 
  "2011_CMS_Stag_Outdoor_200.csv", "2011_CMS_Stag_Outdoor_DT.csv", 
  "2011_CMS_Stag_Outdoor_HJ.csv", "2011_CMS_Stag_Outdoor_HT.csv", 
  "2011_CMS_Stag_Outdoor_JT.csv", "2011_CMS_Stag_Outdoor_PV.csv", 
  "2011_CMS_Stag_Outdoor_SP.csv", "2012_CMS_Athena_Outdoor_100.csv", 
  "2012_CMS_Athena_Outdoor_100H.csv", "2012_CMS_Athena_Outdoor_200.csv", 
  "2012_CMS_Athena_Outdoor_DT.csv", "2012_CMS_Athena_Outdoor_HJ.csv", 
  "2012_CMS_Athena_Outdoor_HT.csv", "2012_CMS_Athena_Outdoor_JT.csv", 
  "2012_CMS_Athena_Outdoor_PV.csv", "2012_CMS_Athena_Outdoor_SP.csv", 
  "2012_CMS_Stag_Outdoor_100.csv", "2012_CMS_Stag_Outdoor_110H.csv", 
  "2012_CMS_Stag_Outdoor_200.csv", "2012_CMS_Stag_Outdoor_DT.csv", 
  "2012_CMS_Stag_Outdoor_HJ.csv", "2012_CMS_Stag_Outdoor_HT.csv", 
  "2012_CMS_Stag_Outdoor_JT.csv", "2012_CMS_Stag_Outdoor_PV.csv", 
  "2012_CMS_Stag_Outdoor_SP.csv", "2013_CMS_Athena_Outdoor_100.csv", 
  "2013_CMS_Athena_Outdoor_100H.csv", "2013_CMS_Athena_Outdoor_200.csv", 
  "2013_CMS_Athena_Outdoor_DT.csv", "2013_CMS_Athena_Outdoor_HJ.csv", 
  "2013_CMS_Athena_Outdoor_HT.csv", "2013_CMS_Athena_Outdoor_JT.csv", 
  "2013_CMS_Athena_Outdoor_PV.csv", "2013_CMS_Athena_Outdoor_SP.csv", 
  "2013_CMS_Stag_Outdoor_100.csv", "2013_CMS_Stag_Outdoor_110H.csv", 
  "2013_CMS_Stag_Outdoor_200.csv", "2013_CMS_Stag_Outdoor_DT.csv", 
  "2013_CMS_Stag_Outdoor_HJ.csv", "2013_CMS_Stag_Outdoor_HT.csv", 
  "2013_CMS_Stag_Outdoor_JT.csv", "2013_CMS_Stag_Outdoor_PV.csv", 
  "2013_CMS_Stag_Outdoor_SP.csv", "2014_CMS_Athena_Outdoor_100.csv", 
  "2014_CMS_Athena_Outdoor_100H.csv", "2014_CMS_Athena_Outdoor_200.csv", 
  "2014_CMS_Athena_Outdoor_DT.csv", "2014_CMS_Athena_Outdoor_HJ.csv", 
  "2014_CMS_Athena_Outdoor_HT.csv", "2014_CMS_Athena_Outdoor_JT.csv", 
  "2014_CMS_Athena_Outdoor_PV.csv", "2014_CMS_Athena_Outdoor_SP.csv", 
  "2014_CMS_Stag_Outdoor_100.csv", "2014_CMS_Stag_Outdoor_110H.csv", 
  "2014_CMS_Stag_Outdoor_200.csv", "2014_CMS_Stag_Outdoor_DT.csv", 
  "2014_CMS_Stag_Outdoor_HJ.csv", "2014_CMS_Stag_Outdoor_HT.csv", 
  "2014_CMS_Stag_Outdoor_JT.csv", "2014_CMS_Stag_Outdoor_PV.csv", 
  "2014_CMS_Stag_Outdoor_SP.csv", "2015_CMS_Athena_Outdoor_100.csv", 
  "2015_CMS_Athena_Outdoor_100H.csv", "2015_CMS_Athena_Outdoor_200.csv", 
  "2015_CMS_Athena_Outdoor_DT.csv", "2015_CMS_Athena_Outdoor_HJ.csv", 
  "2015_CMS_Athena_Outdoor_HT.csv", "2015_CMS_Athena_Outdoor_JT.csv", 
  "2015_CMS_Athena_Outdoor_PV.csv", "2015_CMS_Athena_Outdoor_SP.csv", 
  "2015_CMS_Stag_Outdoor_100.csv", "2015_CMS_Stag_Outdoor_110H.csv", 
  "2015_CMS_Stag_Outdoor_200.csv", "2015_CMS_Stag_Outdoor_DT.csv", 
  "2015_CMS_Stag_Outdoor_HJ.csv", "2015_CMS_Stag_Outdoor_HT.csv", 
  "2015_CMS_Stag_Outdoor_JT.csv", "2015_CMS_Stag_Outdoor_PV.csv", 
  "2015_CMS_Stag_Outdoor_SP.csv", "2016_CMS_Athena_Outdoor_100.csv", 
  "2016_CMS_Athena_Outdoor_100H.csv", "2016_CMS_Athena_Outdoor_200.csv", 
  "2016_CMS_Athena_Outdoor_DT.csv", "2016_CMS_Athena_Outdoor_HJ.csv", 
  "2016_CMS_Athena_Outdoor_HT.csv", "2016_CMS_Athena_Outdoor_JT.csv", 
  "2016_CMS_Athena_Outdoor_PV.csv", "2016_CMS_Athena_Outdoor_SP.csv", 
  "2016_CMS_Stag_Outdoor_100.csv", "2016_CMS_Stag_Outdoor_110H.csv", 
  "2016_CMS_Stag_Outdoor_200.csv", "2016_CMS_Stag_Outdoor_DT.csv", 
  "2016_CMS_Stag_Outdoor_HJ.csv", "2016_CMS_Stag_Outdoor_HT.csv", 
  "2016_CMS_Stag_Outdoor_JT.csv", "2016_CMS_Stag_Outdoor_PV.csv", 
  "2016_CMS_Stag_Outdoor_SP.csv", "2017_CMS_Athena_Outdoor_100.csv", 
  "2017_CMS_Athena_Outdoor_100H.csv", "2017_CMS_Athena_Outdoor_200.csv", 
  "2017_CMS_Athena_Outdoor_DT.csv", "2017_CMS_Athena_Outdoor_HJ.csv", 
  "2017_CMS_Athena_Outdoor_HT.csv", "2017_CMS_Athena_Outdoor_JT.csv", 
  "2017_CMS_Athena_Outdoor_PV.csv", "2017_CMS_Athena_Outdoor_SP.csv", 
  "2017_CMS_Stag_Outdoor_100.csv", "2017_CMS_Stag_Outdoor_110H.csv", 
  "2017_CMS_Stag_Outdoor_200.csv", "2017_CMS_Stag_Outdoor_DT.csv", 
  "2017_CMS_Stag_Outdoor_HJ.csv", "2017_CMS_Stag_Outdoor_HT.csv", 
  "2017_CMS_Stag_Outdoor_JT.csv", "2017_CMS_Stag_Outdoor_PV.csv", 
  "2017_CMS_Stag_Outdoor_SP.csv", "2018_CMS_Athena_Outdoor_100.csv", 
  "2018_CMS_Athena_Outdoor_100H.csv", "2018_CMS_Athena_Outdoor_200.csv", 
  "2018_CMS_Athena_Outdoor_DT.csv", "2018_CMS_Athena_Outdoor_HJ.csv", 
  "2018_CMS_Athena_Outdoor_HT.csv", "2018_CMS_Athena_Outdoor_JT.csv", 
  "2018_CMS_Athena_Outdoor_PV.csv", "2018_CMS_Athena_Outdoor_SP.csv", 
  "2018_CMS_Stag_Outdoor_100.csv", "2018_CMS_Stag_Outdoor_110H.csv", 
  "2018_CMS_Stag_Outdoor_200.csv", "2018_CMS_Stag_Outdoor_DT.csv", 
  "2018_CMS_Stag_Outdoor_HJ.csv", "2018_CMS_Stag_Outdoor_HT.csv", 
  "2018_CMS_Stag_Outdoor_JT.csv", "2018_CMS_Stag_Outdoor_PV.csv", 
  "2018_CMS_Stag_Outdoor_SP.csv", "2019_CMS_Athena_Outdoor_100.csv", 
  "2019_CMS_Athena_Outdoor_100H.csv", "2019_CMS_Athena_Outdoor_200.csv", 
  "2019_CMS_Athena_Outdoor_DT.csv", "2019_CMS_Athena_Outdoor_HJ.csv", 
  "2019_CMS_Athena_Outdoor_HT.csv", "2019_CMS_Athena_Outdoor_JT.csv", 
  "2019_CMS_Athena_Outdoor_PV.csv", "2019_CMS_Athena_Outdoor_SP.csv", 
  "2019_CMS_Stag_Outdoor_100.csv", "2019_CMS_Stag_Outdoor_110H.csv", 
  "2019_CMS_Stag_Outdoor_200.csv", "2019_CMS_Stag_Outdoor_DT.csv", 
  "2019_CMS_Stag_Outdoor_HJ.csv", "2019_CMS_Stag_Outdoor_HT.csv", 
  "2019_CMS_Stag_Outdoor_JT.csv", "2019_CMS_Stag_Outdoor_PV.csv", 
  "2019_CMS_Stag_Outdoor_SP.csv", "2020_CMS_Athena_Outdoor_100.csv", 
  "2020_CMS_Athena_Outdoor_100H.csv", "2020_CMS_Athena_Outdoor_200.csv", 
  "2020_CMS_Athena_Outdoor_DT.csv", "2020_CMS_Athena_Outdoor_HJ.csv", 
  "2020_CMS_Athena_Outdoor_HT.csv", "2020_CMS_Athena_Outdoor_JT.csv", 
  "2020_CMS_Athena_Outdoor_PV.csv", "2020_CMS_Athena_Outdoor_SP.csv", 
  "2020_CMS_Stag_Outdoor_100.csv", "2020_CMS_Stag_Outdoor_110H.csv", 
  "2020_CMS_Stag_Outdoor_200.csv", "2020_CMS_Stag_Outdoor_DT.csv", 
  "2020_CMS_Stag_Outdoor_HJ.csv", "2020_CMS_Stag_Outdoor_HT.csv", 
  "2020_CMS_Stag_Outdoor_JT.csv", "2020_CMS_Stag_Outdoor_PV.csv", 
  "2020_CMS_Stag_Outdoor_SP.csv", "2022_CMS_Athena_Outdoor_100.csv", 
  "2022_CMS_Athena_Outdoor_100H.csv", "2022_CMS_Athena_Outdoor_200.csv", 
  "2022_CMS_Athena_Outdoor_DT.csv", "2022_CMS_Athena_Outdoor_HJ.csv", 
  "2022_CMS_Athena_Outdoor_HT.csv", "2022_CMS_Athena_Outdoor_JT.csv", 
  "2022_CMS_Athena_Outdoor_PV.csv", "2022_CMS_Athena_Outdoor_SP.csv", 
  "2022_CMS_Stag_Outdoor_100.csv", "2022_CMS_Stag_Outdoor_110H.csv", 
  "2022_CMS_Stag_Outdoor_200.csv", "2022_CMS_Stag_Outdoor_DT.csv", 
  "2022_CMS_Stag_Outdoor_HT.csv", "2022_CMS_Stag_Outdoor_JT.csv", 
  "2022_CMS_Stag_Outdoor_PV.csv", "2022_CMS_Stag_Outdoor_SP.csv", 
  "2023_CMS_Athena_Outdoor_100.csv", "2023_CMS_Athena_Outdoor_100H.csv", 
  "2023_CMS_Athena_Outdoor_200.csv", "2023_CMS_Athena_Outdoor_DT.csv", 
  "2023_CMS_Athena_Outdoor_HJ.csv", "2023_CMS_Athena_Outdoor_HT.csv", 
  "2023_CMS_Athena_Outdoor_JT.csv", "2023_CMS_Athena_Outdoor_PV.csv", 
  "2023_CMS_Athena_Outdoor_SP.csv", "2023_CMS_Stag_100_Outdoor.csv", 
  "2023_CMS_Stag_Outdoor_100.csv", "2023_CMS_Stag_Outdoor_110H.csv", 
  "2023_CMS_Stag_Outdoor_200.csv", "2023_CMS_Stag_Outdoor_DT.csv", 
  "2023_CMS_Stag_Outdoor_HJ.csv", "2023_CMS_Stag_Outdoor_HT.csv", 
  "2023_CMS_Stag_Outdoor_JT.csv", "2023_CMS_Stag_Outdoor_PV.csv", 
  "2023_CMS_Stag_Outdoor_SP.csv")

events_with_11_cols <- c("2010_CMS_Athena_Outdoor_LJ.csv", "2010_CMS_Athena_Outdoor_TJ.csv", 
  "2010_CMS_Stag_Outdoor_LJ.csv", "2010_CMS_Stag_Outdoor_TJ.csv", 
  "2011_CMS_Athena_Outdoor_LJ.csv", "2011_CMS_Athena_Outdoor_TJ.csv", 
  "2011_CMS_Stag_Outdoor_LJ.csv", "2011_CMS_Stag_Outdoor_TJ.csv", 
  "2012_CMS_Athena_Outdoor_LJ.csv", "2012_CMS_Athena_Outdoor_TJ.csv", 
  "2012_CMS_Stag_Outdoor_LJ.csv", "2012_CMS_Stag_Outdoor_TJ.csv", 
  "2013_CMS_Athena_Outdoor_LJ.csv", "2013_CMS_Athena_Outdoor_TJ.csv", 
  "2013_CMS_Stag_Outdoor_LJ.csv", "2013_CMS_Stag_Outdoor_TJ.csv", 
  "2014_CMS_Athena_Outdoor_LJ.csv", "2014_CMS_Athena_Outdoor_TJ.csv", 
  "2014_CMS_Stag_Outdoor_LJ.csv", "2014_CMS_Stag_Outdoor_TJ.csv", 
  "2015_CMS_Athena_Outdoor_LJ.csv", "2015_CMS_Athena_Outdoor_TJ.csv", 
  "2015_CMS_Stag_Outdoor_LJ.csv", "2015_CMS_Stag_Outdoor_TJ.csv", 
  "2016_CMS_Athena_Outdoor_LJ.csv", "2016_CMS_Athena_Outdoor_TJ.csv", 
  "2016_CMS_Stag_Outdoor_LJ.csv", "2016_CMS_Stag_Outdoor_TJ.csv", 
  "2017_CMS_Athena_Outdoor_LJ.csv", "2017_CMS_Athena_Outdoor_TJ.csv", 
  "2017_CMS_Stag_Outdoor_LJ.csv", "2017_CMS_Stag_Outdoor_TJ.csv", 
  "2018_CMS_Athena_Outdoor_LJ.csv", "2018_CMS_Athena_Outdoor_TJ.csv", 
  "2018_CMS_Stag_Outdoor_LJ.csv", "2018_CMS_Stag_Outdoor_TJ.csv", 
  "2019_CMS_Athena_Outdoor_LJ.csv", "2019_CMS_Athena_Outdoor_TJ.csv", 
  "2019_CMS_Stag_Outdoor_LJ.csv", "2019_CMS_Stag_Outdoor_TJ.csv", 
  "2020_CMS_Athena_Outdoor_LJ.csv", "2020_CMS_Athena_Outdoor_TJ.csv", 
  "2020_CMS_Stag_Outdoor_LJ.csv", "2022_CMS_Athena_Outdoor_LJ.csv", 
  "2022_CMS_Athena_Outdoor_TJ.csv", "2022_CMS_Stag_Outdoor_LJ.csv", 
  "2022_CMS_Stag_Outdoor_TJ.csv", "2023_CMS_Athena_Outdoor_LJ.csv", 
  "2023_CMS_Athena_Outdoor_TJ.csv", "2023_CMS_Stag_Outdoor_LJ.csv", 
  "2023_CMS_Stag_Outdoor_TJ.csv")
