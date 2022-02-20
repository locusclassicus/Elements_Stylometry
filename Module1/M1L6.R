## download
## notice the raw element!
url <- "https://raw.githubusercontent.com/PerseusDL/canonical-greekLit/master/data/tlg0059/tlg010/tlg0059.tlg010.perseus-grc2.xml"
file_name <- "Philebus.xml"
download.file(url, file_name)

## get div node (= Stephanus pages)
doc <- xmlTreeParse(file_name, useInternalNodes = T, isURL = FALSE)
## this command uses XPath Syntax
my_divs <- getNodeSet(doc, "/tei:TEI//tei:text//tei:body//tei:div//tei:div",
                      namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))
## and сould get the same result simply by indexing the xmlRoot, but we won't
my_divs = xmlRoot(doc)[["text"]][["body"]][["div"]]["div"] 

## each page contains several paragraphs, 
## and each paragraph contains information about speakers, Stephanus pages 
## and Stephanus sections; there are also labels for speakers and the text itself
names(xmlChildren(my_divs[[5]]))
names(xmlChildren(my_divs[[5]][["p"]]))
names(xmlChildren(my_divs[[5]][["p"]][["said"]]))

## xmlValue() can retrieve the text, but not the attributes
## to this end, we need xmlGetAttr()
xmlGetAttr(my_divs[[5]][[1]][["said"]], "who")
xmlGetAttr(my_divs[[5]][[1]][["said"]][["milestone"]], "n")

##  use the XPath Syntax to retrieve the speakers
replies <- getNodeSet(doc, 
                      "/tei:TEI//tei:text//tei:body//tei:div//tei:div//tei:p//tei:said",
                       namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))

who <- sapply(replies, xmlGetAttr, "who")

## let us now complicate things a bit and select text from specific speakers
## who are those unique speakers?
unique(who)

## condition itself 
Philebus <- c()
for (i in 1:length(replies)) {
  who <- xmlGetAttr(replies[[i]], "who")
  if (who == "#Φίληβος"){
    text <- xmlValue(replies[[i]][["text"]])
    Philebus <- c(Philebus, text)
  }
}

## it remains to split the text into words
library(stylo)
Philebus.parsed <- parse.corpus(Philebus)[[1]] ## we might need to unlist this
