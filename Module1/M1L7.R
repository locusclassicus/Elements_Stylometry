## another handy command
library(XML)
file_name <- "Philebus.xml"
doc <- xmlTreeParse(file_name, useInternalNodes = T, isURL = FALSE)
sections <- getNodeSet(doc, "//tei:milestone[@unit='section']",
                         namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))
replies <- getNodeSet(doc, "//tei:said",
                      namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))
## no text in the "sections" nodes!

## an easy way to retrieve section number
xmlGetAttr(sections[[1]], "n")

## now let us try to save replies ("//div//p//said") specifying section 
## for each text, but sections are not specified for all replies
## compare
names(xmlChildren(replies[[1]]))
names(xmlChildren(replies[[2]]))

## so we want to check, for each reply, if there is a "milestone" element within it
## there's a handy function to do it 
elem <- xmlElementsByTagName(replies[[1]], "milestone")
elem$milestone ## will be null if there is none

## but as we remember there are two types of milestones, 
## and we just need the section type

elem.type <- sapply(elem, xmlGetAttr, "unit")
elem.type == "section"

elem.nr <- sapply(elem, xmlGetAttr, "n")
sect.elem <- elem.nr[elem.type == "section"]

## let's write a loop assigning sections to all replies

sect.nr <- c()
for (i in 1:length(replies)){ ## for each "said" node
  ## check if there is a "milestone" element in it
  elem <- xmlElementsByTagName(replies[[i]], "milestone")
  
  ## if there is none
  if (is.null(elem$milestone)) {
    ## then use previous milestone
    elem <- xmlElementsByTagName(replies[[i - 1]], "milestone")
    elem.type <- sapply(elem, xmlGetAttr, "unit")
    elem.nr <- sapply(elem, xmlGetAttr, "n")
    sect.elem <- elem.nr[elem.type == "section"]
    sect.nr <- c(sect.nr, sect.elem)
  }
  ## otherwise
  else {
    elem.type <- sapply(elem, xmlGetAttr, "unit")
    elem.nr <- sapply(elem, xmlGetAttr, "n")
    sect.elem <- elem.nr[elem.type == "section"]
    sect.nr <- c(sect.nr, sect.elem)
  }
}

## the problem now is that said[[2]] which has no milestones got both sect.nr 
## from said[[1]] which has two milestones! so we tweak our code a bit
## specifying that we only want the last milestone unit

sect.nr <- c()
for (i in 1:length(replies)){ 
  elem <- xmlElementsByTagName(replies[[i]], "milestone")
  
  
  if (length(elem) == 0) { ## here's something new, same result
    elem <- xmlElementsByTagName(replies[[i - 1]], "milestone")
    elem.type <- sapply(elem, xmlGetAttr, "unit")
    elem.nr <- sapply(elem, xmlGetAttr, "n")
    sect.elem <- elem.nr[elem.type == "section"]
    sect.nr <- c(sect.nr, tail(sect.elem,1)) ## here is smth new 
  }
  ## otherwise
  else {
    elem.type <- sapply(elem, xmlGetAttr, "unit")
    elem.nr <- sapply(elem, xmlGetAttr, "n")
    sect.elem <- elem.nr[elem.type == "section"]
    sect.nr <- c(sect.nr, sect.elem)
  }
}

## now note that though sections only has 282 elements, sect.nr has 518 elements
## this happens because sect.nr repeats a section if there is another reply in it
## however, replies contain 1142 elements, and we only have 518 :(

## this happens because two or more consecutive "saids" can lack milestones, so 
## we tweak the code again and use "while" loop

sect.nr <- c()
for (i in 1:length(replies)){ 
  elem <- xmlElementsByTagName(replies[[i]], "milestone")
  
  if (length(elem) == 0) {
    x = i 
    while (length(elem) == 0) { ## here's something new
      x <- (x-1)
      elem <- xmlElementsByTagName(replies[[x]], "milestone")
      elem.type <- sapply(elem, xmlGetAttr, "unit")
      elem.nr <- sapply(elem, xmlGetAttr, "n")
      sect.elem <- elem.nr[elem.type == "section"]
      sect.nr <- c(sect.nr, tail(sect.elem,1))}
  }
  else {
    elem.type <- sapply(elem, xmlGetAttr, "unit")
    elem.nr <- sapply(elem, xmlGetAttr, "n")
    sect.elem <- elem.nr[elem.type == "section"]
    sect.nr <- c(sect.nr, head(sect.elem, 1)) 
    ## small tweak: some replies contain two milestones
  }
}

## we now extract all speakers as we did last time 
speakers <- sapply(replies, xmlGetAttr, "who")

## and all text 
text <- c()
for (i in 1:length(replies)) {
    t <- xmlValue(replies[[i]][["text"]])
    text <- c(text, t)
}

## bind it all together
df <- cbind(sect.nr, speakers, text)
rownames(df) <- c(1:length(sect.nr))


