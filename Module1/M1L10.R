## write a function to parse files in Diorisis archive

xml_to_txt <- function(x) {
  ## extract files 
  diorisis_list <- unzip("Diorisis.zip", files = NULL, list = TRUE) 
  new_list <- grep(x, diorisis_list$Name, ignore.case=TRUE, value=TRUE) 
  unzip("Diorisis.zip", files = new_list)
}

## let's source it and try x = 0086 (Aristotle)
## add new commands

xml_to_txt <- function(x) {
  ## extract files 
  diorisis_list <- unzip("Diorisis.zip", files = NULL, list = TRUE) 
  new_list <- grep(x, diorisis_list$Name, ignore.case=TRUE, value=TRUE) 
  unzip("Diorisis.zip", files = new_list)
  ## remove spaces
  library(filesstrings)
  remove_filename_spaces(".")
  ## parse
  filenames <- list.files(pattern = ".xml")
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
}