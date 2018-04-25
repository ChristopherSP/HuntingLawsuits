# checking matches from bash: egrep -oh  "claro(\s|\n)+(s|sa|tv|celular|americel|pré)(\s|$)" claroGarbage.txt
# clean_matches <- read.csv("./clean_matches.csv")
# 
# cm1 <- table(clean_matches)
# cm1[order(cm1, decreasing = T)]
# df1 <- data.frame(word = names(m1[order(m1, decreasing = T)]), freq = m1[order(m1, decreasing = T)])
# write.csv(df1, row.names = F, file = "df1.csv")
# 
# # checking matches from bash: egrep -oh "(\w+ +\w+ +)claro$" claroGarbage.txt
# matches2 <- read.csv("./end_matches.csv")
# 
# m2 <- table(matches2)
# m2[order(m2, decreasing = T)]
# df2 <- data.frame(word = names(m2[order(m2, decreasing = T)]), freq = m2[order(m2, decreasing = T)])
# write.csv(df2, row.names = F, file = "df2.csv")

#### Go for it
#### claro(\s|\n)+(s|sa|tv|celular|americel|pré)(\s|$)
f <- read.csv("./claroGarbage.txt", sep = "\t", stringsAsFactors = FALSE)
l <- gregexpr("claro(\\n|\\s)+(s|sa|tv|celular|americel|pré)(\\s|$)", f$text)
df <- do.call(rbind, lapply(l, function(x) cbind(x[1], attr(x, "match.length")[1])))

# get all rows that match to our criteria
indices <- which(df[,1] != -1)
# get only CNJs
matches_cnj <- regmatches(f$text[indices], gregexpr("^cnj(\\s+\\d+)+", f$text[indices]))
df_matches_cnj <- do.call(rbind, matches_cnj)
df_matches_cnj <- regmatches(df_matches_cnj, gregexpr("(\\d+\\s*)+", df_matches_cnj))
# adds a bar to the protocol numbers
df_matches_cnj <- sub("\\s+", "/", df_matches_cnj)

# write a CSV
write.table(as.array(df_matches_cnj), file = "cnjs.csv", col.names = F, row.names = F, quote = F)
