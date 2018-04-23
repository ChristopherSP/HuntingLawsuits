library(data.table)
library(stringi)

amostra = fread("~/Downloads/amostra-claro-mato-grosso.csv",head=F,col.names = c("cnj","rsocial"))
amostra[, ano := substr(cnj,12,15)]
amostra[,.N*5,by=ano][order(ano)]
