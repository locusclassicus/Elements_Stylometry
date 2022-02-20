## download 
url <- "https://raw.githubusercontent.com/gcelano/LemmatizedAncientGreekXML/master/texts/tlg0593.1st1K001.1st1K-grc1.xml"
file_name <- "Gorgias_Helen.xml"
download.file(url, file_name)

## get rootnode
library(XML)
doc <- xmlTreeParse(file_name, useInternalNodes = T)
rootnode <- xmlRoot(doc)

## explore the structure
names(xmlChildren(rootnode)) ## s = sentence
length(xmlChildren(rootnode)) ## 85 sentences

names(xmlChildren(rootnode[[2]])) ## t = token
length(xmlChildren(rootnode[[2]])) ## 21 tokens in sent 1, etc.

## get some token 
rootnode[[2]][1]
names(xmlChildren(rootnode[[2]][[1]])) ## form and lemma

## xPath syntax
my_tokens <- getNodeSet(doc, "/text/s/t")

class(my_tokens)
length(my_tokens)
my_tokens[[1]]["f"]

## extract wordforms
my_wf <- getNodeSet(doc, "/text/s/t/f", 
                    namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))
my_wf[1]
xmlValue(my_wf[1])
my_text <- sapply(my_wf, xmlValue)

## inspect output
head(my_text, 21) ## remember we had 21 tokens in sent 1? 
my_text <- tolower(my_text)



