## what if we don't want wordforms? 

## download 
url <- "https://raw.githubusercontent.com/gcelano/LemmatizedAncientGreekXML/master/texts/tlg0593.1st1K001.1st1K-grc1.xml"
file_name <- "Gorgias_Helen.xml"
download.file(url, file_name)

## get tokens nodes
library(XML)
doc <- xmlTreeParse(file_name, useInternalNodes = T)
my_tokens <- getNodeSet(doc, "/text/s/t") ## no need to add namespaces 

## inspect tokens
my_tokens[1] ## l1
my_tokens[22] ## l2
my_tokens[23] ## only form

## check l1
my_l1 <- getNodeSet(doc, "/text/s/t/l/l1")

## how do we know that l1 or l2 are missing?
xmlValue(my_tokens[[1]][["l"]][["l1"]])

is.na(xmlValue(my_tokens[[1]][["l"]][["l1"]]))
!is.na(xmlValue(my_tokens[[1]][["l"]][["l1"]]))

xmlValue(my_tokens[[23]][["l"]][["l1"]])
!is.na(xmlValue(my_tokens[[23]][["l"]][["l1"]]))

## initiate empty vector
lemmata <- character()

## loops and conditions
### simple condition
x <- 1
if (x < 10) {
  x <- x+1
}

## for loop 
y <- c()
x <- list(a = c(1:5), b = c(6:10), d = c(11:15))
for (i in 1:length(x)) {
  y <- c(y, x[[i]][1])
}

## if-else condition
x <- 4
if (x >= 5) {x <- x - 1} else {x <- x + 1}
ifelse(x >= 5, (x <- x-1), (x <- (x +1)))

## write a condition
for (i in 1:length(my_tokens)){
  if (!is.na(xmlValue(my_tokens[[i]][["l"]][["l1"]]))) {
    lemmata <- c(lemmata, xmlValue(my_tokens[[i]][["l"]][["l1"]]))
    
  } else if (!is.na(xmlValue(my_tokens[[i]][["l"]][["l2"]]))) {
    lemmata <- c(lemmata, xmlValue(my_tokens[[i]][["l"]][["l2"]]))                 
    
  } else { lemmata <- c(lemmata, xmlValue(my_tokens[[i]][["f"]]))
  }
}

## inspect output
lemmata[1:21]
class(lemmata)

## delete punctuation
lemmata.w <- gsub("\\W", "", lemmata)
lemmata.t <- lemmata.w[which(lemmata.w != "")]


