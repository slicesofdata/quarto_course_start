library(rvest)
library(tidyverse)
library(magrittr)

rvest::read_html("https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=600&season_hnd=131#event19")

#writeClipboard("https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=600&season_hnd=131#event19")
#     "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=4153&season_hnd=608#event6"
page <- "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=4153&season_hnd=608#event6"
page <- "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=4153&season_hnd=608"
#tb <- rvest::read_html("https://datatables.net/examples/basic_init/multiple_tables.html")
#rvest::read_html("https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=4153&season_hnd=608")
#page %>% html_nodes("td:nth-child(2) a") %>% html_attr("href")

scraplinks <- function(url){
  # Create an html document from the url
  webpage <- xml2::read_html(url)
  # Extract the URLs
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  # Extract the link text
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  return(tibble(link = link_, url = url_))
}

#### get the links to save
event_links <- scraplinks(page) %>%
  filter(., str_detect(url, "event")) %>%
  mutate(.,
         link = gsub(" ", "", as.character(link)),
         link = gsub(",", "", as.character(link)),
         )

# then create list and add names based on links
#event_list <- vector("list", length(event_links$link))
#names(event_list) <- event_links$link

# change the names
table_list <- page %>% rvest::read_html() %>% rvest::html_table()
names(table_list) <- event_links$link
#length(table_list)

# then remove missing cols
table_list_new <- lapply(table_list, function(x) {
  #print(names(x))
  dat = x %>% # keep names that are not missing
    select(., names(x)[nzchar(names(x))])
  
  names(dat) = gsub(" ", "_", names(dat))
  #print(dat)
  # add new variables
  dat = dat %>% 
    mutate(., 
           Team = "Stag",
           Event = "",
           Season = "",
           Location = "",
           )
  
})

for(x in 1:length(table_list_new)) {
  
  location = "Outdoor"
  dat$Location = location
  dat = table_list_new[[x]]
  dat$Season = stringr::str_split_fixed(dat$Meet_Date, " ", 3)[,3]
  dat$Event = names(table_list_new)[[x]]
  team = unique(dat$Team)
  event = unique(dat$Event)
  season = unique(dat$Season)
  
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





df <- data_frame(one = rep("hey", 10), two = seq(1:10), etc = "etc")

list_df <- list(df, df, df, df, df)
dfnames <- c("first", "second", "third", "fourth", "fifth")
list_df
list_df %>% map2_df(dfnames, ~mutate(.x, Event =.y))

purrr::map2_df(table_list_new, ~mutate(Event = names(table_list_new))) 

lapply(table_list_new, function(x) {
  
}

#map(table_list_new, )

for (elem in 1:length(table_list_new)) {
  dat = table_list_new[elem]
  #dat = dat[, 1:3]
  
  event = names(table_list_new)[elem]
  print(event)
  
  dat$Event = event 
  #table_list_new[[elem]] = dat
}
table_list_new  
view_html(table_list_new[1])



for (i in 1:length(table_list)) {
  
  dat = table_list[i]
  print(dat)
  #dat = dat[,-1]
  #table_list[i] = dat
    
}



year_code <- list(
  "2011" = "696",
  "2012" = "907",
  "2013" = "1130",
  "2014" = "1338",
  "2015" = "1513",
  "2016" = "1765",
  "2017" = "1974",
  "2018" = "2231",
  "2019" = "2604",
  "2020" = "3099",
  "2021" = "3429",
  "2022" = "4153" # verify
)

url_list_outdoor <- list(
#  "2023" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=4153&season_hnd=608",
#  "2022" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=3730&season_hnd=568",
#  "2021" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=3200&season_hnd=530",
#  "2020" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=2906&season_hnd=496",
#  "2019" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=2573&season_hnd=453",
#  "2018" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=2278&season_hnd=414",
  # NCAA?
#  "2017" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=1915&season_hnd=377",
  # NCAA?
#  "2016" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=1683&season_hnd=336",
#  "2015" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=1552&season_hnd=303",
#  "2014" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=1251&season_hnd=256",
#  "2013" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=1047&season_hnd=221",
#  "2012" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=863&season_hnd=191",
#  "2011" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=695&season_hnd=158",
  "2010" = "https://www.tfrrs.org/all_performances/CA_college_m_Claremont_Mudd_Scripps.html?list_hnd=600&season_hnd=131"
)



event_codes_men_track <- list(
#  "100" = "6",
#  "200" = "7",
#  "400" = "11",
#  "800" = "12",
#  "1500" = "13",
#  "Mile" = "15",
#  "3000" = "18",
#  "5000" = "21",
#  "10000" = "22",  # "100H" = "4",
#  "110H" = "5",
#  "400H" = "9",
  "3000S" = "19"
)

event_codes_men_track_relay <- list(
  "4x100" = "31",
  "4x400" = "33", 
  "SMR (800)" = "123",
  "DMR" = "38"
  )

event_codes_men_field <- list(
 # 'HJ' = "23",
  "PV" = "24",
  "LJ" = "25",
  "TJ" = "26",
  "SP" = "30",
  "DT" = "27",
  "HT" = "28",
  "JT" = "29",
  #  "Hep" = "40",
  "Dec" = "39"
)


event_codes_women_track <- list(
  "100" = "6",
  "200" = "7",
  "400" = "11",
  "800" = "12",
  "1500" = "13",
  "Mile" = "15",
  "3000" = "18",
  "5000" = "21",
  "10000" = "22",
  "100H" = "4",
#  "110H" = "5",
  "400H" = "9",
  "3000S" = "19"
)

event_codes_women_track_relay <- list(
  "4x100" = "31",
  "4x400" = "33", 
  "4x800" = "34", 
  "DMR"   = "38"
  )

event_codes_women_field <- list(
  'HJ' = "23",
  "PV" = "24",
  "LJ" = "25",
  "TJ" = "26",
  "SP" = "30",
  "DT" = "27",
  "HT" = "28",
  "JT" = "29"
  #  "Hep" = "40",
#  "Dec" = "39"
)

url_base         <- "https://www.tfrrs.org/"
url_perf         <- "all_performances/"
url_state        <- "CA_"
url_level        <- "college_"
url_gender       <- "m_"
url_college      <- "Claremont_Mudd_Scripps"
url_ext          <- ".html?"
url_list_query   <- "list_hnd="
url_year_code    <- "4153"
url_season_query <- "&season_hnd="
url_season_num   <- "608" 
url_event_code   <- "#event"
url_event_num    <- "6"

url_full <- paste(
  url_base, url_perf, url_state, url_level, url_gender, url_college, 
  url_ext, url_list_query, 
  url_year_code, 
  url_season_query, url_season_num,
  url_event_code, url_event_num, sep = ""
)

build_url <- function(
      url_base, url_perf, url_state, url_level, 
      url_gender, url_college, 
      url_ext, url_list_query, 
      url_year_code, 
      url_season_query, url_season_num,
      url_event_code, url_event_num
    ) {
  
  url_full = paste(
    url_base, url_perf, url_state, url_level, url_gender, url_college, 
    url_ext, url_list_query, 
    url_year_code, 
    url_season_query, url_season_num,
    url_event_code, url_event_num, 
    sep = ""
  )
  return(url_full)
}


get_html_data_from_url <- function(url) {
  html_data = url %>%
    rvest::read_html(.) %>%
    rvest::html_node("table") %>% 
    rvest::html_table()
  
  html_data = html_data[, -1] # clean up the data frame
  
  return(html_data)
}

add_new_variables <- function(
    data, 
    team = "CMS", 
    #year = "2022",
    #sex  = "m",
    event = "100"
    ) {
  
  #sex = ifelse(sex == "m", "Men", "Women")
  
  data = data %>%  # add key variables 
    dplyr::mutate(., 
                  team_var = team,
                  year_var = year,
                  sex_var = sex,
                  event_var = event
    )
  return(data)
}


write_data <- function(data, file) {
  message(paste("Saving:", file))
  write.csv(data, file = file, row.names = F, over)
}


###########################################
# Running the program
###########################################
###########################################


url_page <- build_url(
  url_base, url_perf, url_state, url_level, 
  url_gender, url_college, 
  url_ext, url_list_query, 
  url_year_code, 
  url_season_query, url_season_num,
  url_event_code, url_event_num
)

  

# loop through the urls and events
#code_list <- list()
get_tfrrs_data <- function(location, 
                     team_gender, 
                     url_list, 
                     event_codes
                     ) {

for (u in 1:length(url_list)) {
  
  url = url_list[u]
  message(paste("Pinging  :", url))

  # then build event page
  for (e in 1:length(event_codes)) {
    event_url = paste(url, "#event", event_codes[e], sep = '')
    
    #writeClipboard(event_url)
    
    message(paste("Accessing:", event_url))
    Sys.sleep(10)
    
    season = names(url_list)[e]
    #message(season)
    # event name and code
    event = names(event_codes)[e]
    #message(paste("Getting:", event))

    #code  = event_codes[e]
    #message(code)
    
    # then read the page
    data <- rvest::read_html(event_url)
    #Sys.sleep(10)
    data <- data %>% rvest::html_node("table") %>% 
      rvest::html_table()
    #print(data)
    data <- data %>% # keep names that are not missing
      select(.,names(data)[nzchar(names(data))])

    #Sys.sleep(10)
    
    if (all(class(data) %in% c("tbl_df", "tbl", "data.frame"))) {
      
      # then add variables
      data <- data %>%
        mutate(.,
             Season = season,
             Event = event,
             Location = location,
             Team = team_gender
             )
    
      # set file name
      file_name = here::here("data", "tfrrs", paste(
         paste(season, "CMS", team_gender, event, location, sep = "_"), #names(url_list)[u], 
         ".csv", sep = ""
          )
        )
    
      #message(file_name)
      # write data to disk
    
      if (file.exists(file_name)) { file.remove(file_name) }
        Sys.sleep(5)
        
        write_data(data = data, file = file_name)
        
        # delay timer
        #Sys.sleep(30) 
        } else {
          message(paste("NOTE: Table is empty or not a data frame.\n", sep = ""))
        }
    
  }}
 } # end func
  
  
############################################################################
# Get data
############################################################################

# track short
# Stag
get_tfrrs_data(
  location = "Outdoor",
  team_gender = "Stag",
  url_list = url_list_outdoor, 
  event_codes = event_codes_men_track
  ) 

# Athena
get_tfrrs_data(location = "Outdoor",
         team_gender = "Athena",
         url_list = gsub("_m_", "_f_", url_list_outdoor), 
         event_codes = event_codes_women_track
         )

# track long
# Stag
# Athena

# relay 
# Stag
# Athena

# field
# Stag
# Athena


############################################################################
# end get data
############################################################################  



# read the data
data <- get_html_data_from_url(url_page) #%>% add_new_variables(data) # add new variables

data <- add_new_variables(data) # add new variables

view_html(add_new_variables(data))

# set file name
file_name <- build_file_name()

# write data to disk
write_data(data = data, file = file_name)

###########################################
# End running the program
###########################################


#url_list <- "/SCIAC_Outdoor_Performance_List"
#gender_list <- c("?gender=m", "?gender=f")

#page <- read_html("https://www.tfrrs.org/results_search.html")
#page <- rvest::read_html("https://tf.tfrrs.org/lists/3781/SCIAC_Performance_List?gender=m")

#url_base <- "https://www.tfrrs.org/lists/"
#url_list <- "/SCIAC_Outdoor_Performance_List"
#gender_list <- c("?gender=m", "?gender=f")

#build_url_list 
#https://www.tfrrs.org/lists/3429/SCIAC_Outdoor_Performance_List?gender=f#event6




# codes for tesing
tfrrs_year_code <- list("2011" = "696")
event_codes_men <- list("100" = "6")
  
# get data for men
for (year in 1:length(year_code)) {
  
  the_url = paste(url_base, 
                  year_code[year], 
                  url_list, sep = "")
  
  year = names(year_code)[year]
  
  # update the url
  the_url = paste(the_url, "?gender=m", sep = "")
  
  # then append the event
  for (e in 1:length(event_codes_men)) {
    
    # the url up with gender 
    in_url = the_url
    
    # get event 
    event = event_codes_men[e]
    event_name = names(event_codes_men)[e]
    # update the url
    out_url = paste(in_url, "#event", event, sep = "")
    
    message(out_url)
    
    # get the data
    the_page = rvest::read_html(out_url)
    
    dat = the_page %>% 
      #rvest::html_node("table") %>% 
      rvest::html_table() 
    
    #dat = dat[,-1]
     
    #print(names(dat))
    
    # add new variables for easy reading
    dat = dat %>% 
      dplyr::mutate(., 
                    year_name = year,
                    sex_name = "men",
                    event_name = event_name
                    )
    # clean names
    names(dat) = gsub(" ", "_", names(dat))
    
    #print(names(dat))
    # set the file name
    file_name = paste0(paste(year, "tfrrs", "men", event_name, sep = "_"), ".csv")
    
    #message(file_name)
    
    # write the data
    #write.csv(dat, 
    #          here::here("data", "tfrrs", file_name),
    #          row.names = FALSE
    #          )
    }
message(paste("File:", file_name, "saved."))
}


#https://www.tfrrs.org/lists/696/SCIAC_Outdoor_Performance_List
#"https://www.tfrrs.org/lists/696/SCIAC_Outdoor_Performance_List"

