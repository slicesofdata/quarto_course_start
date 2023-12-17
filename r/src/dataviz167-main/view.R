# viewing data frame with html

view <- function(object, rows = F, show = 100,...) { 

  if (!require(DT)) {
  
    stop("DT library not installed")

  } else {
  
  if (is_tibble(object)) {
    object = as.data.frame(object)
    #smessage("converted tibble to dataframe for viewing")
  } 

  if (is.null(dim(object)) & class(object) == "list") {
    message("Object is a list. Viewer displays last list element. Consider passing each element to view().")

    lapply(object, function(x) {
	      DT::datatable(x, rownames = rows, options = list(pageLength = show))
      }) 
  } else {
    DT::datatable(object, rownames = rows, options = list(pageLength = show)) 
  }

  }
}

# same func, diff name to address conflict with tibble::view()
view_html <- view

gview <- function(df, rows = F, show = 100, ...) { 
  DT::datatable(df, rownames = rows, options = list(pageLength = show)) 
  }

message("done")