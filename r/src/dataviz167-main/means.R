means <- function(data, y = formula()) {
  # note: if summarizing a single variable, use var ~ var, or formula will fail 
  agg = aggregate(y, data = data, 
          FUN = function(x) c(mean = round(mean(na.omit(as.numeric(x))), 4), 
                              se   = round(sd(as.numeric(x))/sqrt(sum(!is.na(as.numeric(x)))), 4),
#                              se   = round(plotrix::std.error(na.omit(as.numeric(x))), 4),
                              mdn  = round(median(na.omit(as.numeric(x))), 4),
                              min  = round(min(na.omit(as.numeric(x))), 4), 
                              max  = round(max(na.omit(as.numeric(x))), 4),
                              n    = length(na.omit(as.numeric(x))),
                              skew = round(e1071::skewness(na.omit(as.numeric(x))), 4)
                              )
  )
  sd(x)/sqrt(sum(!is.na(x)))
  df <- data.frame(mean = agg[[as.character(y)[2]]][,1],  
                   se   = agg[[as.character(y)[2]]][,2],
                   mdn  = agg[[as.character(y)[2]]][,3], 
                   min  = agg[[as.character(y)[2]]][,4], 
                   max  = agg[[as.character(y)[2]]][,5], 
                   n    = agg[[as.character(y)[2]]][,6],
                   skew = agg[[as.character(y)[2]]][,7] 
                )
  
  if (as.character(y)[[2]] != as.character(y)[[3]]) { # if there are > 1 factors in formula

      df[[as.character(y)[2]]] <- df[["mean"]]
      v.names <- names(df)
      
      # then populate with vars from formula
      fy = gsub(" ", "", as.character(y)[3])
      vars = stringr::str_split_fixed(fy, "\\+", stringr::str_count(fy, "\\+")+1)
      
      # then populate the data frame with vars from formula
      for (i in 1:dim(vars)[2]) {   df[[ vars[1,i] ]] <- agg[,i]   }
           df <- df[, c(vars[1,], v.names)] # rearrange columns
      df[[ paste0(as.character(y)[2],".z") ]] <- scale(df[[ "mean" ]])
      #print(df); 
      }
      if (as.character(y)[[2]] == as.character(y)[[3]]) {  #if there is only 1 factor in formula
          row.names(df) <- as.character(y)[[2]]  
      }
  return(df)
} #example call: means(OGT, rt ~ cond + wordtype)

#multiple means
mmeans <- function(data, cols, vars = c("") ) {
  data.t <- data.table::data.table(data) #convert to data table
  return(as.data.frame(data.t[, lapply(.SD, mean, na.rm = T), by = vars, .SDcols = cols ] ))
}
