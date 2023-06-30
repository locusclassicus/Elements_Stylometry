# load libraries
library(XML)
library(tidyverse)
library(tidytext)

# # Cicero's letters to Atticus
# url = "https://raw.githubusercontent.com/PerseusDL/canonical-latinLit/master/data/phi0474/phi057/phi0474.phi057.perseus-lat1.xml"
setwd("/Users/olga/R_Workflow/Elements_Stylometry/Module5")
filename = "Atticus.xml"
# download.file(url, filename)
# greek manually edited in xml!!!

# Parse XML
doc <- xmlTreeParse(filename, useInternalNodes = T)
rootnode <- xmlRoot(doc)

# get book number
books <- getNodeSet(doc, 
                    "/tei:TEI//tei:text//tei:body//tei:div//tei:div[@subtype='book']",
                      namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))

# initialize empty data frame
books_df <- data.frame(matrix(ncol = 4, nrow = 0))
names <- c("book_nr", "letter_nr", "date", "text")
colnames(books_df) <- names

# extract data from xml
for (i in 1:length(books)){
  book <- books[[i]]
  book_nr <- xmlGetAttr(book, "n")
  
  letters <-  xmlChildren(book)
  
  for (j in 1:length(letters)) {
    letter <- letters[[j]]
    letter_nr <- xmlGetAttr(letter, "n")
    letter_div <- letter[names(xmlChildren(letter)) == "div"]
    letter_text <- sapply(letter_div, xmlValue)
    letter_text <- paste(letter_text, sep="", collapse="")
    letter_label <- letter[["label"]]["seg"][1]
    letter_date <- xmlValue(letter_label)
  
    result_letter <- c(book_nr, letter_nr, letter_date, letter_text)
    books_df <- rbind(books_df, result_letter)
  }
  colnames(books_df) <- names
}

# extract year from dateline
cicero_att <- books_df %>% as_tibble() %>% 
  filter(grepl("\\(\\d*?\\)", date))

cicero_att$date <- gsub(".*\\(", "", cicero_att$date)
cicero_att$date <- gsub(").*$", "", cicero_att$date)

cicero_att <- cicero_att %>% 
  mutate(date = as.numeric(date)*(-1))

# how many letters per year?
cicero_att %>% group_by(date) %>% summarise(n=n()) %>% 
  ggplot(aes(date, n, fill = "salmon")) + 
  geom_bar(stat = "identity", show.legend = F)

save(cicero_att, file = "CiceroLetters.Rdata")

# # Cicero's Letters to Quintus
# url = "https://raw.githubusercontent.com/PerseusDL/canonical-latinLit/master/data/phi0474/phi058/phi0474.phi058.perseus-lat1.xml"
# filename = "Quintus.xml"
# download.file(url, filename)
# 
# # Parse XML
# doc <- xmlTreeParse(filename, useInternalNodes = T)
# rootnode <- xmlRoot(doc)
# 
# # get book number
# books <- getNodeSet(doc, 
#                     "/tei:TEI//tei:text//tei:body//tei:div//tei:div[@subtype='Book']",
#                     namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))
# 
# # initialize empty data frame
# books_df <- data.frame(matrix(ncol = 4, nrow = 0))
# names <- c("book_nr", "letter_nr", "date", "text")
# colnames(books_df) <- names
# 
# # extract data from xml
# for (i in 1:length(books)){
#   book <- books[[i]]
#   book_nr <- xmlGetAttr(book, "n")
#   
#   letters <-  xmlChildren(book)[-1]
#   
#   for (j in 1:length(letters)) {
#     letter <- letters[[j]]
#     letter_nr <- xmlGetAttr(letter, "n")
#     letter_div <- letter[names(xmlChildren(letter)) == "div"]
#     letter_text <- sapply(letter_div, xmlValue)
#     letter_text <- paste(letter_text, sep="", collapse="")
#     letter_label <- letter[["label"]]["seg"][1]
#     letter_date <- xmlValue(letter_label)
#     
#     result_letter <- c(book_nr, letter_nr, letter_date, letter_text)
#     books_df <- rbind(books_df, result_letter)
#   }
#   colnames(books_df) <- names
# }
# 
# # extract year from dateline
# cicero_quint <- books_df %>% as_tibble()
# cicero_quint <- cicero_quint %>% filter(grepl("\\(\\d*?\\)", date))
# 
# cicero_quint$date <- gsub(".*\\(", "", cicero_quint$date)
# cicero_quint$date <- gsub(").*$", "", cicero_quint$date)
# 
# cicero_quint <- cicero_quint %>% mutate(date = as.numeric(date)) %>% 
#  mutate(dest = "Quintus")
# 
# # merge Atticus and Quintus
# cicero_letters <- cicero_att %>% bind_rows(cicero_quint)
# 
# # Letters to friends
# url = "https://raw.githubusercontent.com/PerseusDL/canonical-latinLit/master/data/phi0474/phi056/phi0474.phi056.perseus-lat1.xml"
# filename = "Familiares.xml"
# download.file(url, filename)
# 
# # Parse XML
# doc <- xmlTreeParse(filename, useInternalNodes = T)
# rootnode <- xmlRoot(doc)
# 
# # get book number
# books <- getNodeSet(doc, 
#                     "/tei:TEI//tei:text//tei:body//tei:div//tei:div[@subtype='Book']",
#                     namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))
# 
# # initialize empty data frame
# books_df <- data.frame(matrix(ncol = 3, nrow = 0))
# names <- c("book_nr", "letter_nr", "greek")
# colnames(books_df) <- names
# 
# # extract data from xml
# for (i in 1:length(books)){
#   book <- books[[i]]
#   book_nr <- xmlGetAttr(book, "n")
#   
#   letters <-  xmlChildren(book)[-1]
#   
#   for (j in 1:length(letters)) {
#     letter <- letters[[j]]
#     letter_nr <- xmlGetAttr(letter, "n")
#     letter_div <- letter[names(xmlChildren(letter)) == "div"]
#     letter_text <- sapply(letter_div, xmlValue)
#     letter_text <- paste(letter_text, sep="", collapse="")
#     letter_label <- letter[["label"]]["seg"][1]
#     letter_date <- xmlValue(letter_label)
#     
#     result_letter <- c(book_nr, letter_nr, letter_date, letter_text)
#     books_df <- rbind(books_df, result_letter)
#   }
#   colnames(books_df) <- names
# }
# 
# # extract year from dateline
# cicero_fam <- books_df %>% as_tibble()
# cicero_fam <- cicero_fam %>% filter(grepl("\\(\\d*?\\)", date))
# 
# cicero_fam$date <- gsub(".*\\(", "", cicero_fam$date)
# cicero_fam$date <- gsub(").*$", "", cicero_fam$date)
# 
# cicero_fam <- cicero_fam %>% mutate(date = as.numeric(date)) %>% 
#   mutate(dest = "Fam")
# 
# # merge Atticus, Quintus and Familiares
# cicero_letters <- cicero_letters %>% bind_rows(cicero_fam) %>% 
#   mutate(date = date*(-1))
