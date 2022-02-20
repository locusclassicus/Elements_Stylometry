## download the Corpus from 
## https://figshare.com/articles/dataset/The_Diorisis_Ancient_Greek_Corpus/6187256
diorisis_list <- unzip("Diorisis.zip", files = NULL, list = TRUE) 
head(diorisis_list)

## Unzip Selected
xenophon_list <- grep("(0032)", diorisis_list$Name, ignore.case=TRUE, value=TRUE) 
xenophon_1 <- grep("(006)", xenophon_list, ignore.case=TRUE, value=TRUE)
xenophon_2 <- grep("(001)", xenophon_list, ignore.case=TRUE, value=TRUE)
unzip("Diorisis.zip", files = c(xenophon_1, xenophon_2))

## rename files
library(filesstrings)
remove_filename_spaces(".")
file_names_old <- tail(list.files(pattern = ".xml"), 2)
file_names_new <- c("Xenophon_Anab.xml", "Xenophon_Hell.xml")
file.rename(from=file_names_old, to=file_names_new)
list.files(pattern = ".xml")

library(XML)
filenames = file_names_new

for (i in filenames){
  url = paste("./", i, sep = "")
  doc <- xmlTreeParse(url, useInternalNodes = TRUE, isURL = F)
  rootnode  <- xmlRoot(doc)
  text <- xpathSApply(rootnode, "//TEI.2/text/body/sentence/word/lemma", xmlGetAttr, 'entry')
  text <- as.character(text)
  y <-gsub(".xml","", i)
  outname <-  paste(y, ".txt", sep= "") 
  write.table(text, outname, row.names = FALSE, col.names = FALSE, quote = FALSE)
}
## some lemmas are NULL, we might want to remove them later

dir.create("Unicode")
files_unicode <- list.files(pattern = ".txt")
file.copy(files_unicode, "Unicode")
file.remove(files_unicode)

## now let's write a function...


