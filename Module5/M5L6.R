# exploring the use of Greek in cicero's letters
library(XML)
library(tidyverse)
library(tidytext)

# load the corrected file from the home directory
filename = "Atticus.xml"

# <quote xml:lang="greek">
# <foreign xml:lang="greek">

# Parse XML
doc <- xmlTreeParse(filename, useInternalNodes = T)
rootnode <- xmlRoot(doc)
# Get node set
books <- getNodeSet(doc, 
                    "/tei:TEI//tei:text//tei:body//tei:div//tei:div[@subtype='book']",
                    namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))

# initialize empty data frame
books_df <- c()

# extract data from xml
for (i in 1:length(books)){
  book <- books[[i]]
  book_nr <- xmlGetAttr(book, "n")
  
  letters <-  xmlChildren(book)
  
  for (j in 1:length(letters)) {
    letter <- letters[[j]]
    letter_nr <- xmlGetAttr(letter, "n")
    greek_words <- xmlValue(xmlElementsByTagName(letter, 
                                                 "foreign",
                                                 recursive = T))
    all_quotes <- xmlElementsByTagName(letter, "quote",
                                       recursive = T)
    idx <- sapply(all_quotes, xmlGetAttr, "xml:lang")
    greek_quotes <- xmlValue(all_quotes[idx == "greek"])
    
    if(length(greek_words) > 0){
    for(k in 1:length(greek_words)){
      line_w <- c(book_nr, letter_nr, greek_words[k])
      books_df <- rbind(books_df, line_w)
    }}
    
    if(length(greek_quotes) > 0){
    for (l in 1:length(greek_quotes)){
      line_q <- c(book_nr, letter_nr, greek_quotes[l])
      books_df <- rbind(books_df, line_q)
    }}
  }
}
names <- c("book_nr", "letter_nr", "greek")
colnames(books_df) <- names

# tokenized greek words
greek_att <- books_df %>% as_tibble() %>% 
  unnest_tokens(greek, greek, token = "words") 

# total greek per letter
greek_att_sum <- greek_att %>% group_by(book_nr, letter_nr) %>% 
  summarize(total_greek = n()) %>% 
  ungroup() 


# delete chr from letter name
greek_att_sum$letter_nr <- gsub("A", ".1", greek_att_sum$letter_nr)
greek_att_sum$letter_nr <- gsub("B", ".2", greek_att_sum$letter_nr)
greek_att_sum$letter_nr <- gsub("C", ".3", greek_att_sum$letter_nr)
greek_att_sum$letter_nr <- gsub("D", ".4", greek_att_sum$letter_nr)


greek_att_sum <- greek_att_sum %>% mutate(book_nr = as.numeric(book_nr),
                                          letter_nr = as.numeric(letter_nr))

# join with sentiment data
load("/Users/olga/R_Workflow/Elements_Stylometry/data/CiceroSent.Rdata")
greek_sentiment <- cic_sent_grouped %>% 
  left_join(greek_att_sum, by = c("book_nr", "letter_nr")) 

# replace NAs
idx <- is.na(greek_sentiment$total_greek)
greek_sentiment$total_greek[idx] <- 0

# ratio_greek = greek/total
load("/Users/olga/R_Workflow/Elements_Stylometry/data/CiceroLetterLength.Rdata")
cicero_letter_length <- cicero_letter_length %>% 
  separate(letter_id, into = c("book_nr", "letter_nr"), sep= "_")

cicero_letter_length$letter_nr <- gsub("A", ".1", cicero_letter_length$letter_nr)
cicero_letter_length$letter_nr <- gsub("B", ".2", cicero_letter_length$letter_nr)
cicero_letter_length$letter_nr <- gsub("C", ".3", cicero_letter_length$letter_nr)
cicero_letter_length$letter_nr <- gsub("D", ".4", cicero_letter_length$letter_nr)


cicero_letter_length <- cicero_letter_length %>% 
  mutate(book_nr = as.numeric(book_nr), letter_nr = as.numeric(letter_nr))

# joint
greek_sentiment <- greek_sentiment %>% 
  left_join(cicero_letter_length, by = c("book_nr", "letter_nr"))

greek_sentiment_ratio <- greek_sentiment %>% 
   mutate(ratio = total_greek/n)

# plot greek per letter
greek_sentiment_ratio %>% 
  ggplot(aes(index, ratio, color = as.factor(date))) + 
           geom_point(alpha = 0.5)

# plot greek per year
greek_sentiment_ratio %>% group_by(date) %>% 
  summarise(gr_year = mean(ratio)) %>% 
  ggplot(aes(date, gr_year, fill = as.factor(gr_year))) +
  geom_col(show.legend = F)
