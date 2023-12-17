dataviz167_packages <- c(
  
  # DATA FRAME MANIPULATION
  "tidyverse", "dplyr", "dtplyr", "data.table",
  "car",  "magrittr", "magrittr", "readr", 

  # DATES AND TIMES 
  "lubridate", "zoo", "xts",

  # DEVELOPMENT
  "devtools", "remotes",

  # COLORS
  "colorblindr", "monochromeR", "viridis", "viridisLite", 
  
  # FONTS and TEXT
  "extrafont", "glue", "showtext", "utf8", "systemfonts",

  # GEOSPATIAL
  "zipcode", 
  
  # IMAGES AND GRAPHICS 
  "ragg", "Cairo", "magick", 
  "colourpicker", "RColorBrewer",

  # MODELS AND DATA
  "see", "boot", "easystats", "see", "broom", 

  # PLOTTING
  "dendextend", "dygraphs", 
  "ggplot2", "GGally", "ggiraph", "ggpubr", "ggvis", 
  "ggtext", "geomtextpath", "ggthemes", 
  "gghighlight", "ggformula", "ggAssist", "ggThemeAssist",
  "gt", "gtsummary",
  "plotly", "cowplot",  

  # PROJECTS AND MANAGEMENT
  "gitr", "here", "usethis", "renv",
  
  # READING AND WRITING DATA FILES 
  "foreign", "haven", "vroom", "xlsx", "readr",

  # REFERENCES AND REPORTING 
  "bibext", "knitr", "pandoc", "rstudioapi", "rmarkdown", "quarto", 
  "shiny", "shinydashboard", "shinydashboardPlus", 

  # STRING MANIPULATION
  "stringr",

  # TABLES
  "DT", "htmlwidgets", "htmlTable", "kable", "kableExtra",
  
  # OTHER
  "coin", "fs", "zoom"
)

install.packages(dataviz167_packages, dep = T)

main.packages <- c(
  "boot",  "bibext",
  "Cairo", "car", "coin", "colorblindr", 
  "dendextend", "devtools", "data.table", "DT", "dygraphs",
  "easystats", "foreign",
  "ggplot2", "GGally", "ggiraph", "ggpubr", "ggvis", 
  "haven", "here", "htmlwidgets", "htmlTable", 
  "kableExtra", "knitr", 
  "lubridate",
  "magrittr", "monochromeR",
  "pandoc", "plotly",
  "ragg", "rstudioapi", "renv",
  "see",
  "sjPlot", "sqldf",
  "tidyverse", 
  "usethis",
  "quarto", "xlsx",
  "zipcode", "zoom"
)

install.packages(main.packages, dep = T)
