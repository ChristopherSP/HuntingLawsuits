library(rvest)
library(parallel)
library(pbmcapply)

args = commandArgs(trailingOnly = TRUE)

#################################
# Check Arguments
#################################
if (length(args) != 3) {
  stop(
    "Error: Three arguments must be provided:\n\t1. Initial page number\n\t2. Final page number\n\t3. Output folder to save pdf documents",
    call. = FALSE
  )
} else if (any(is.na(as.numeric(args[1:2])))) {
  stop("Error: Arguments 1 and 2 must be numeric", call. = FALSE)
} else if (as.numeric(args[2]) < as.numeric(args[1])) {
  cat(
    "Passed final page is smaller than initial page. The order was changed to attend the program requisites"
  )
  aux = args[1]
  args[1] = args[2]
  args[2] = aux
}

#################################
# Initialize Arguments
#################################
initialPage = as.integer(args[1])
finalPage = as.integer(args[2])
outputFolder = args[3]

#################################
# Get all diaries editions number to download
#################################
getEditions = function(page = '1') {
  baseURL = "http://www.tjmt.jus.br/Djes?page="
  
  editionInfo = read_html(paste0(baseURL, page)) %>%
    html_nodes("td.tamanho14.cinza.family") %>%
    html_nodes("b") %>%
    html_text()
  
  edition = editionInfo[seq(1, length(editionInfo), by = 2)]
  date = editionInfo[seq(2, length(editionInfo), by = 2)]
  year = sapply(strsplit(date, "/", fixed = T), function(x)
    x[3])
  return(paste0(edition, '-', year))
}

#################################
# Download pdf into specified folder
#################################
downloadDiarie = function(edition, complementURL) {
  baseURL = "http://sistemadje.tjmt.jus.br/publicacoes/"
  tryCatch({
    download.file(
      paste0(baseURL, edition, complementURL),
      paste0(outputFolder, edition, complementURL)
    )
  },
  error = function(cond) {
    return(NA)
  })
}

#################################
# Main
#################################
ncores = detectCores() - 1

# get editions numbers in all pages
editions = pbmclapply(
  initialPage:finalPage,
  getEditions,
  mc.cores = ncores,
  ignore.interactive = T
)
editionsVec = unlist(editions)

# get when jornal changes page format
idxChangeDiarieName = which(grepl("9892", editionsVec, fixed = T))
if (length(idxChangeDiarieName) == 0) {
  idxChangeDiarieName = length(editionsVec)
}
# separates editions in the new and old format
selectPattern = ifelse(seq_along(editionsVec) < idxChangeDiarieName, T, F)

newerEditions = editionsVec[selectPattern]
olderEditions = editionsVec[!selectPattern]

if (length(newerEditions) > 0) {
  pbmclapply(
    newerEditions,
    downloadDiarie,
    complementURL = " C1 Tribunal de Justiça.pdf",
    mc.cores = ncores,
    ignore.interactive = T
  )
  pbmclapply(
    newerEditions,
    downloadDiarie,
    complementURL = " C2 Comarcas - Entrância Especial.pdf",
    mc.cores = ncores,
    ignore.interactive = T
  )
  pbmclapply(
    newerEditions,
    downloadDiarie,
    complementURL = " C7 Comarcas - 1ª 2ª e 3ª Entrância.pdf",
    mc.cores = ncores,
    ignore.interactive = T
  )
  pbmclapply(
    newerEditions,
    downloadDiarie,
    complementURL = " C6 Foro Extrajudicial.pdf",
    mc.cores = ncores,
    ignore.interactive = T
  )
}

if (length(olderEditions) > 0) {
  pbmclapply(
    olderEditions,
    downloadDiarie,
    complementURL = ".pdf",
    mc.cores = ncores,
    ignore.interactive = T
  )
}
