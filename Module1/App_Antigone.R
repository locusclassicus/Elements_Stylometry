## we know now how to extract text by specific speaker and the location 
## what we might further need is to bind this into one dataframe

## the beginning is familiar
url <- "https://raw.githubusercontent.com/PerseusDL/canonical-greekLit/master/data/tlg0011/tlg002/tlg0011.tlg002.perseus-grc2.xml"
file_name <- "SophoclesAnt.xml"
download.file(url, file_name)
doc <- xmlTreeParse(file_name, useInternalNodes = T, isURL = FALSE)

## check unique speakers
sp <- getNodeSet(doc, "/tei:TEI//tei:text//tei:body//tei:div//tei:div//tei:sp",
                       namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))

## examine one node
xmlElementsByTagName(sp[[3]], "speaker")
xmlValue(xmlElementsByTagName(sp[[3]], "speaker"))
unname(xmlValue(xmlElementsByTagName(sp[[3]], "speaker")))

## retrieve speakers
speakers <- c()
for(i in 1:length(sp)) {
  speaker <- unname(xmlValue(xmlElementsByTagName(sp[[i]], "speaker")))
  speakers <- c(speakers, speaker)
}

head(speakers)
unique(speakers)

## retrieve text, first one node
xmlElementsByTagName(sp[[1]], "l")
xmlValue(xmlElementsByTagName(sp[[1]], "l"))
unname(xmlValue(xmlElementsByTagName(sp[[1]], "l")))
## we might want to collapse lines into one 
paste(unname(xmlValue(xmlElementsByTagName(sp[[1]], "l"))), collapse = " ")

## now write a loop 
all_text <- c()
for(i in 1:length(sp)) {
  text <- paste(unname(xmlValue(xmlElementsByTagName(sp[[i]], "l"))), collapse = " ")
  all_text <- c(all_text, text)
}

### compare length
length(all_text) == length(speakers)

## bind
df <- data.frame(speakers = speakers, text = all_text)

## explore
unique(df$speakers)

## subset
antigone <- df[df$speakers == "Ἀντιγόνη",]
antigone_text <- strsplit(tolower(antigone$text), "\\W")
antigone_text <- unlist(antigone_text, use.names = FALSE)
## delete empty spaces
antigone_text <- antigone_text[antigone_text != ""]

## calculate frequencies
library(stylo)
antigone_freq <- make.frequency.list(antigone_text, value = TRUE, relative = FALSE)
## but probably you'll want to use lemmatized corpus to this end
