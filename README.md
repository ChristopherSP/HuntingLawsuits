# Hunting Lawsuits Challenge

The solution was made on R and there are only two scripts needed in this folder related to the challenge submition: downloadDJ.R and completeProcessAsFunction.R

# Prerequisites

To run this script it's needed to have R installed in the computer, which can be achieved by the following line of code on the Ubuntu terminal

    sudo apt-get install r-base-dev

Some scripts demand a list of packages to be installed. To install a package on R one must execute the following command on R

    install.package("packageName")

# Running the scripts

To execute a R script one must run the following command on the terminal
  
    Rscript --vanilla pathToScript/scriptName.R arg1 arg2 ... argN 

# Usefull Scripts
## downloadDJ.R Script

This script downloads the pdf diaries to an existing folder. 

There are three packages that need to be installed 

 - library(rvest)
 - library(parallel)
 - library(pbmcapply)

### Arguments

All parameters are mandatory

 - __arg1__: The initial page of the court website to download 
 - __arg2__: The final page of the court website to download 
 - __arg3__: The folder path to download the pdf files to

Ex.:

    Rscript --vanilla ./downloadDJ.R 1 267 "~/Download/DJ/"

It's of the most importance that the specified path in arg3 contains the last "/" or else will be an error when tring to paste the file name to the output path.

### Output layout
The output of the script is all pdf diaries documents downloaded in the designeted folder.

For more details see the coments in the scripts

## completeProcessAsFunction.R Script

This script reads the pdf diaries in an existing folder, extracts its contents, breaks each lawsuit in a corpus and filter the ones that are related to the Claro SA company name. 

There are some packages that need to be installed 

 - library(data.table)
 - library(stringi)
 - library(pdftools)
 - library(tm)
 - library(parallel)
 - library(tidytext)
 - library(dplyr)
 - library(pbmcapply)

### Arguments

All parameters are mandatory

 - __arg1__: The input folder path that contains the pdf documents to be read
 - __arg2__: The output txt file path to export the results to

Ex.:

    Rscript --vanilla ./completeProcessAsFunction.R "~/Download/DJ/" "~/Download/Output/output.txt"

It's of the most importance that the specified path in arg1 contains the last "/" or else will be an error when tring to paste the file name to the output path.

### Output layout

The output is a txt file containg the information below:

 - __doc_id__: Internal id of the diarie corpus
 - __diario__: Diaire name
 - __content_nchars__: Total amount of character in the corpus content
 - __cnj_protocolo__: CNJ or Protocol number
 - __digitosCNJ__: Number of numerical digits in the CNJ
 - __formatedCNJ_Protocolo__: Formated CNJ number
 
|doc_id|diario|content_nchars|cnj_protocolo|digitosCNJ|formatedCNJ_Protocolo|
|-|-|-|-|-|-|
|"1011"|"10246-2018 C1 Tribunal de Justiça.pdf"|1309|"1001414-22.2017.8.11.0007"|20|"1001414-22.2017.8.11.0007"|
|"1018"|"10246-2018 C1 Tribunal de Justiça.pd"|1135|"1001090-32.2017.8.11.0007"|20|"1001090-32.2017.8.11.0007"|
|"7"|"10245-2018 C2 Comarcas - Entrância Especial.pdf"|1882|"58448-69.2014.811.0041"|18|"58448-69.2014.811.0041"|
