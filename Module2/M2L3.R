## lexical density, see Savoy p. 30
## POS counts needed! 

library(XML)
filenames <-  "Apology.xml"

## mean sent. length and POS 

url = paste("./", filenames, sep = "")
doc <- xmlTreeParse(url, useInternalNodes = TRUE, isURL = F)
rootnode  <- xmlRoot(doc)
  
POS <- xpathSApply(rootnode, "//TEI.2/text/body/sentence/word/lemma", xmlGetAttr, 'POS')
POS <- unlist(POS)
POS.df <- as.data.frame(table(POS))
tokens <- sum(POS.df$Freq)

## subset functional and content-bearing words
content <- c("noun", "adjective", "verb", "adverb")
Content.df <- POS.df[POS.df$POS %in% content, ]
sum.content <- sum(Content.df$Freq)

LD <- sum.content/tokens
## the LD seems a bit too high, but that is because of tagging (see file)


