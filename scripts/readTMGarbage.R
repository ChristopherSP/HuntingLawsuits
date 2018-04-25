library(data.table)
library(stringi)
library(pdftools)
library(tm)
library(parallel)
library(doParallel)
library(tidytext)
library(dplyr)

claroGarb = fread("~/Downloads/claroGarbage.txt",sep='\t',quote="\"",header = F)
names(claroGarb) = c("author", "datetimestamp", "description", "heading", "doc_id", "language", "origin", "totalChars", "CNJ", "digitsCNJ", "text")
gc()

corpus = VCorpus(DataframeSource(claroGarb))

i <- 0
corpus = tm_map(corpus, function(x) {
  i <<- i +1
  meta(x, "author") <- claroGarb$author[i]
  meta(x, "datetimestamp") <- claroGarb$datetimestamp[i]
  meta(x, "description") <- claroGarb$description[i]
  meta(x, "heading") <- claroGarb$heading[i]
  meta(x, "id") <- claroGarb$doc_id[i]
  meta(x, "language") <- claroGarb$language[i]
  meta(x, "origin") <- claroGarb$origin[i]
  meta(x, "totalChars") <- claroGarb$totalChars[i]
  meta(x, "CNJ") <- claroGarb$CNJ[i]
  meta(x, "digitsCNJ") <- claroGarb$digitsCNJ[i]
  x
})

rm(list = "claroGarb")
gc()

claro = tm_filter(corpus, FUN = function(x) any(grepl("claro(\\n|\\s)+(s|sa|tv|celular|americel|pré)(\\s|$)", x)))

data = tidy(claro)

claroDT = tidy(claro) %>% 
  select(origin, CNJ, digitsCNJ) %>% 
  as.data.table

claroDT[digitsCNJ>=12, formatedCNJ := stri_pad_left(CNJ,20,'0')]
claroDT[digitsCNJ>=12, formatedCNJ := gsub('(\\d{7})(\\d{2})(\\d{4})(\\d)(\\d{2})(\\d{4})','\\1-\\2\\.\\3\\.\\4\\.\\5\\.\\6',formatedCNJ)]
claroDT[digitsCNJ<=4, formatedCNJ := stri_pad_left(CNJ,5,'0')]
claroDT[digitsCNJ<12, formatedCNJ := paste0(substring(CNJ,1,nchar(CNJ)-4),"/",substring(CNJ,nchar(CNJ)-3,nchar(CNJ)))]

write.table(claroDT,"~/Downloads/claroResults.txt",row.names = F,quote=T, sep='\t')

