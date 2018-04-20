library(rvest)
library(parallel)

getEditions = function(page = '1'){
  baseURL = "http://www.tjmt.jus.br/Djes?page="
  
  editionInfo = read_html(paste0(baseURL,page)) %>% 
    html_nodes("td.tamanho14.cinza.family") %>% 
    html_nodes("b") %>% 
    html_text()
  
  edition = editionInfo[seq(1,length(editionInfo),by=2)]
  date = editionInfo[seq(2,length(editionInfo),by=2)]
  year = sapply(strsplit(date,"/",fixed = T),function(x)x[3])
  return(paste0(edition,'-',year))  
}

downloadDiarie = function(edition, complementURL){
  baseURL = "http://sistemadje.tjmt.jus.br/publicacoes/"
  tryCatch({
    download.file(paste0(baseURL,edition,complementURL),paste0("~/Downloads/DJ/",edition,".pdf"))
  },
  error = function(cond){
    cat("Sem Link: ",edition,"\n")
    return(NA)
  }) 
}

ncores = detectCores() - 1
editions = mclapply(1:267, getEditions, mc.cores = ncores)
editionsVec = unlist(editions)

idxChangeDiarieName = which(grepl("9892",editionsVec,fixed = T))

selectPattern = ifelse(seq_along(editionsVec)< idxChangeDiarieName,T,F)

newerEditions = editionsVec[selectPattern]
olderEditions = editionsVec[!selectPattern]

mclapply(newerEditions, downloadDiarie, complementURL = " C1 Tribunal de Justiça.pdf", mc.cores = ncores)
mclapply(olderEditions, downloadDiarie, complementURL = ".pdf", mc.cores = ncores)


