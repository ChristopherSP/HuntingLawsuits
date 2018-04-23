library(data.table)
library(stringi)
library(tm)

claroGarb = fread("~/Downloads/claroGarbage/claroGarbage.txt",sep='\t',quote="\"",header = F, col.names = c("author", "datetimestamp", "description", "heading", "doc_id", "language", "origin", "totalChars", "CNJ", "digitsCNJ", "text"))
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
