library(data.table)
library(stringi)
library(pdftools)
library(tm)
library(parallel)
library(doParallel)
library(tidytext)
library(dplyr)

# Get current script path
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

# Split multicolumn text
structPage = function(page){
  col1 = stri_replace_all_regex(page,"(   +).*\\n","")
  col2 = stri_extract_all_regex(page,"(   +).*\\n")[[1]]
  col2 = paste(stri_replace_all_regex(col2,"(   +)"," "),collapse = '')
  text = paste(col1,col2,collapse = '\n\n')  
  return(text)
}

# Read justice diarie pdf
readDocuments = function(file){
  # Read as plain text
  doc = pdf_text(file)
  # Columns and pages in order
  doc = unlist(lapply(doc,structPage))
  # Paste all paragraphs
  doc = paste(doc, collapse ="\n")
  # Split document by lawsuit
  doc = strsplit(doc,"(Processo )?Número( Único)?: |Protocolo( Número\\/Ano)?: ", perl = T)[[1]]
  # Replace word used to split the documents
  if(length(doc) > 1){
    doc[2:length(doc)] = paste0("CNJ: ",doc[2:length(doc)])
  }
  return(doc)
}

######################################################################################
# Read Files
######################################################################################
# path = "../DJ/"
path = "~/Downloads/DJ_Newer/"
# Get all files names
filesName = list.files(path)
files = paste0(path,filesName)

# Apply read function
ncores = detectCores() - 1
diaries = mclapply(files, readDocuments, mc.cores = ncores)

# Create a corpus from documents. It's a proper structure to do data mining.
diaries = lapply(diaries, function(x) VCorpus(VectorSource(x)))

######################################################################################
# Creating Corpus and Initial Meta
######################################################################################
# Create metadata defining which document came from which diarie
invisible(
  lapply(seq_along(diaries), function(doc){
    lapply(seq_along(diaries[[doc]]), function(idx){
      meta(diaries[[doc]][[idx]], tag = "language") <<- "pt"
      meta(diaries[[doc]][[idx]], tag = "origin") <<- filesName[doc]
    })}))

# Put all corpus together
doc.corpus = diaries[[1]]
invisible(
  sapply(2:length(diaries), function(idx){
    doc.corpus <<- c(doc.corpus,diaries[[idx]])
  }))

# Remove structure to free memory
rm(diaries)
invisible(gc())

######################################################################################
# Cleaning Corpus
######################################################################################
# Remove multiple blank space
doc.corpus = tm_map(doc.corpus, content_transformer(stri_replace_all_regex)," +"," ")
doc.corpus = tm_map(doc.corpus, removePunctuation)
doc.corpus = tm_map(doc.corpus, content_transformer(tolower))

# Filter only lawsuits that contains the word CLARO S.A.
claro.corpus = tm_filter(doc.corpus, FUN = function(x) any(grepl("claro s[ ]?a|claro ltda", x)))
claro.corpus.garbage = tm_filter(doc.corpus, FUN = function(x) any(grepl("claro", x)))

# Extract CNJ and number of numerical digits. The number of digits is present so we can kwon if the process is old or new (20 digits from 2010)
invisible(
  sapply(seq_along(claro.corpus), function(idx){
    meta(claro.corpus[[idx]], tag = "CNJ") <<- stri_replace_all_regex(stri_replace_all_fixed(stri_extract_first_regex(content(claro.corpus[[idx]]),"cnj [ 0-9.\\/-]+"),"cnj ","")," +","")
    meta(claro.corpus[[idx]], tag = "digitsCNJ") <<- nchar(stri_replace_all_regex(meta(claro.corpus[[idx]], tag = "CNJ"),"[[:punct:]]",""))
    }))

# Extracts metadata in a tabular format
data = tidy(claro.corpus)

# Selects only the important columns, without the whole text content
teste = data %>% 
  select(origin, CNJ, digitsCNJ) %>% 
  as.data.table


claroGarbage = tidy(claro.corpus.garbage)

write.table(claroGarbage,"~/Downloads/claroGarbage.txt",sep='\t',quote=T,row.names = F)
