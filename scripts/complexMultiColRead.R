library(pdftools)
src <- ""
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

QTD_COLUMNS <- 2
read_text <- function(text) {
  result <- ''
  #Get all index of " " from page.
  lstops <- gregexpr(pattern =" ",text)
  #Puts the index of the most frequents ' ' in a vector.
  stops <- as.integer(names(sort(table(unlist(lstops)),decreasing=TRUE)[1:2]))
  #Slice based in the specified number of colums (this can be improved)
  for(i in seq(1, QTD_COLUMNS, by=1))
  {
    temp_result <- sapply(text, function(x){
      start <- 1
      stop <-stops[i] 
      if(i > 1)            
        start <- stops[i-1] + 1
      if(i == QTD_COLUMNS)#last column, read until end.
        stop <- nchar(x)+1
      substr(x, start=start, stop=stop)
    }, USE.NAMES=FALSE)
    temp_result <- trim(temp_result)
    result <- append(result, temp_result)
  }
  result
}

txt <- pdf_text('~/Downloads/DJMT/outfile_p8-9.pdf')
result <- ''
for (i in 1:length(txt)) { 
  page <- txt[i]
  t1 <- unlist(strsplit(page, "\n"))      
  maxSize <- max(nchar(t1))
  t1 <- paste0(t1,strrep(" ", maxSize-nchar(t1)))
  result = append(result,read_text(t1))
}
result
