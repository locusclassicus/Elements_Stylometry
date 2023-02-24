# exploring the use of Greek in cicero's letters

load("/Users/olga/R_Workflow/Elements_Stylometry/data/CiceroLetters.Rdata")

# Cicero's letters to Atticus
url = "https://raw.githubusercontent.com/PerseusDL/canonical-latinLit/master/data/phi0474/phi057/phi0474.phi057.perseus-lat1.xml"
filename = "Atticus.xml"
download.file(url, filename)

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
books_df <- data.frame(matrix(ncol = 3, nrow = 0))
names <- c("book_nr", "letter_nr", "greek")
colnames(books_df) <- names

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
  colnames(books_df) <- names
}

greek_att <- books_df %>% mutate(dest = "Atticus") %>% 
  unnest_tokens(greek, greek, token = "regex", pattern = "\\s+") 

greek_att_sum <- greek_att %>% group_by(book_nr, letter_nr, dest) %>% 
  summarize(total_greek = n())

# join
cicero_att <- cicero_letters %>% filter(dest == "Atticus") 
greek_att_joined <- cicero_att %>% left_join(greek_att_sum, by = c("book_nr", "letter_nr", "dest"))

# NAs to 0
greek_att_joined$total_greek[is.na(greek_att_joined$total_greek)] <- 0
## fixed: betacode tokenization

# tokenize text in cicero_att

# count the number of words in each letter

# ratio = greek/total
load("/Users/olga/R_Workflow/Elements_Stylometry/data/CiceroLetterLength.Rdata")
att_letter_length <- cicero_letter_length %>% filter(dest == "Atticus")

att_greek_ratio <- greek_att_joined %>% select(-text, -dest) %>% 
  left_join(att_letter_length, by = c("book_nr", "letter_nr", "date")) %>% 
  select(-dest) %>% mutate(ratio = total_greek/letter_length)

# plot ratio
mean_greek_att <- att_greek_ratio %>% filter(!is.na(ratio)) %>% group_by(date) %>% 
  summarise(mean=mean(ratio)) 

mean_greek_att %>% ggplot(aes(date, mean)) +
  geom_line()
  
# total greek words per year
greek_att_joined %>% group_by(date) %>% summarise(sum = sum(total_greek))
# 50 & 49 peaks

att_greek_ratio %>% group_by(date) %>% summarise(mean = mean(ratio))

# by book 
greek_att_joined %>% group_by(book_nr) %>% 
  summarise(sum = sum(total_greek))

