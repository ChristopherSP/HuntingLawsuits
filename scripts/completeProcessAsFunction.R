library(data.table)
library(stringi)
library(pdftools)
library(tm)
library(parallel)
library(tidytext)
library(dplyr)
library(pbmcapply)

args = commandArgs(trailingOnly = TRUE)

#################################
# Check Arguments
#################################
if (length(args) != 2) {
  stop(
    "Error: Two arguments must be provided:\n\t1. PDF diaries folder path\n\t2. Full output txt file path to save the to result",
    call. = FALSE
  )
}

inputFolder = args[1]
outputFile = args[2]

# Split multicolumn text
structPage = function(page) {
  col1 = stri_extract_all_regex(page, ".*(   +)")[[1]]
  col1 = paste(col1, collapse = '\n')
  col2 = stri_extract_all_regex(page, "(   +).*\\n")[[1]]
  col2 = paste(stri_replace_all_regex(col2, "(   +)", " "), collapse = '')
  text = paste(col1, col2, collapse = '\n\n')
  return(text)
}

# Gets CNJ number
getCNJOnReading = function(page) {
  cnjs = c("First Document")
  cnjs = c(
    cnjs,
    stri_extract_all_regex(
      page,
      " \\d+[ ]?[-\\/][ ]?\\d{4}  +|\\d{1,7}-\\d{2}.\\d{4}.\\d[\\.]?\\d{2}.\\d{4}  +|\\d{14,20}  +| \\d+[ ]?[-\\/][ ]?\\d{4}\\n|\\d{1,7}-\\d{2}.\\d{4}.\\d[\\.]?\\d{2}.\\d{4}\\n|\\d{14,20}\\n",
      simplify = T
    )
  )
  cnjs = stri_replace_all_regex(cnjs,
                                "\\n| ",
                                "")
  return(cnjs[!is.na(cnjs)])
}

# Read justice diarie pdf
readDocuments = function(file) {
  # Read as plain text
  doc = pdf_text(file)
  # Columns and pages in order
  doc = unlist(lapply(doc, structPage))
  # Paste all paragraphs
  doc = paste(doc, collapse = "\n")
  # doc = tolower(doc)
  cnjs = getCNJOnReading(doc)
  # Split document by lawsuit
  doc = strsplit(
    doc,
    " \\d+[ ]?[-\\/][ ]?\\d{4}  +|\\d{1,7}-\\d{2}.\\d{4}.\\d[\\.]?\\d{2}.\\d{4}  +|\\d{14,20}  +| \\d+[ ]?[-\\/][ ]?\\d{4}\\n|\\d{1,7}-\\d{2}.\\d{4}.\\d[\\.]?\\d{2}.\\d{4}\\n|\\d{14,20}\\n",
    perl = T
  )[[1]]
  # Replace word used to split the documents
  if (length(doc) > 1) {
    doc[2:length(doc)] = paste0("smtxdocumentseparationmarker ",
                                cnjs[2:length(doc)],
                                doc[2:length(doc)])
  }
  return(list(cnj = cnjs, doc = doc))
}

huntingLawsuits = function(fileName) {
  ######################################################################################
  # Read Files
  ######################################################################################
  file = paste0(path, fileName)
  splitedDocs = readDocuments(file)
  cnjs = splitedDocs$cnj
  digitsCNJ = nchar(stri_replace_all_regex(cnjs,
                                           "[[:punct:]]",
                                           ""))
  docs = splitedDocs$doc
  doc.corpus = VCorpus(VectorSource(docs))
  
  # remove older struct to optimize memory use
  rm(docs)
  invisible(gc())
  
  # Create metadata defining which document came from which diarie
  doc.corpus = tm_map(doc.corpus, function(x) {
    meta(x, tag = "origin") <- fileName
    meta(x, tag = "totalChars") <- nchar(content(x))
    x
  })
  
  # Create metadata defining CNJ number and how many numerical digits it has
  idx = 0
  doc.corpus = tm_map(doc.corpus, function(x) {
    idx <<- idx + 1
    meta(x, tag = "CNJ") <- cnjs[idx]
    meta(x, tag = "digitsCNJ") <- digitsCNJ[idx]
    x
  })
  ######################################################################################
  # Cleaning Corpus
  ######################################################################################
  # Remove multiple blank space
  doc.corpus = tm_map(doc.corpus,
                      content_transformer(stri_replace_all_regex),
                      " +",
                      " ")
  # Remove special characters
  doc.corpus = tm_map(doc.corpus, removePunctuation)
  # Transform string to lower case
  doc.corpus = tm_map(doc.corpus, content_transformer(tolower))
  
  #Filter Claro documents only
  claro.corpus = tm_filter(
    doc.corpus,
    FUN = function(x)
      any(
        grepl(
          "claro(\\n|\\s)+(s|sa|tv|celular|americel|pré)(\\s|$|\\n)",
          x
        )
      )
  )
  
  # remove older struct to optimize memory use
  rm(list = "doc.corpus")
  invisible(gc())
  
  # Saves claro's important metadata to a file
  if (length(claro.corpus) > 0) {
    claro = tidy(claro.corpus) %>%
      select(id, origin, totalChars, CNJ, digitsCNJ) %>%
      as.data.table
    
    claro[, formatedCNJ := ifelse(
      digitsCNJ >= 12  &
        CNJ != "First Document",
      gsub(
        '(\\d{7})(\\d{2})(\\d{4})(\\d)(\\d{2})(\\d{4})',
        '\\1-\\2\\.\\3\\.\\4\\.\\5\\.\\6',
        stri_pad_left(stri_replace_all_regex(CNJ, "[[:punct:]]", ""), 20, '0')
      ),
      CNJ
    )]
    
    write.table(
      claro,
      outputFile,
      sep = "\t",
      quote = T,
      row.names = F,
      col.names = F,
      append = T
    )
  }
  invisible(gc())
}

path = inputFolder

# Get all files names
filesName = list.files(path)
filesName = sort(filesName, decreasing = T)

# Apply read function
ncores = detectCores() - 1
pbmclapply(filesName,
           huntingLawsuits,
           mc.cores = ncores,
           ignore.interactive = T)
