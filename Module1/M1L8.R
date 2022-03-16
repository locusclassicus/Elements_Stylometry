## the beginning is familiar
library(XML)
url <- "https://raw.githubusercontent.com/OpenGreekAndLatin/csel-dev/master/data/stoa0040/stoa001/stoa0040.stoa001.opp-lat1.xml"
file_name <- "Confessiones.xml"
download.file(url, file_name)
doc <- xmlTreeParse(file_name, useInternalNodes = T, isURL = FALSE)

## explore
para <- getNodeSet(doc, "/tei:TEI//tei:text//tei:body//tei:div//tei:div//tei:p",
                       namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))
## stores chapter, not book number

books <- getNodeSet(doc, "/tei:TEI//tei:text//tei:body//tei:div[@subtype='book']",
                    namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))
sapply(books, xmlGetAttr, "n")

## retrieve text with book number
df <- data.frame()
for (i in 1:length(books)) {
  nr <- sapply(books[i], xmlGetAttr, "n")
  chld <- xmlChildren(books[[i]])
  divs <- chld[names(chld) == "div"]
  for (j in 1:length(divs)) {
    text <- sapply(divs[[j]][["p"]]["text"], xmlValue)
    b.nr <- rep(nr, length(text))
    result <- cbind(b.nr, text)
    df <- rbind(df, result)
  }
}

## check 
 


