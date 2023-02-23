## hiatus count

## do not use lemmatized text
sample <- c("ὅτι μὲν ὑμεῖς, ὦ ἄνδρες Ἀθηναῖοι, πεπόνθατε ὑπὸ τῶν ἐμῶν κατηγόρων, οὐκ οἶδα· ἐγὼ
 δʼ οὖν καὶ αὐτὸς ὑπʼ αὐτῶν ὀλίγου ἐμαυτοῦ ἐπελαθόμην, οὕτω πιθανῶς ἔλεγον. καίτοι 
ἀληθές γε ὡς ἔπος εἰπεῖν οὐδὲν εἰρήκασιν. μάλιστα δὲ αὐτῶν ἓν ἐθαύμασα τῶν πολλῶν
 ὧν ἐψεύσαντο, τοῦτο ἐν ᾧ ἔλεγον ὡς χρῆν ὑμᾶς εὐλαβεῖσθαι μὴ ὑπʼ ἐμοῦ ἐξαπατηθῆτε 
ὡς δεινοῦ ὄντος λέγειν. ")

## transform as tibble
library(tidytext)
library(tidyr)
sample <- as_tibble(sample) %>% mutate(title = "Apology", .before = value) %>% rename(text = value)

## unnest: spaces and punctuation lost
ngrams <- sample %>% unnest_tokens(ngram, text, token = "character_shingles", n=3)

## tokenize: punctuation and spaces kept, but \n interfere
library(tokenizers)
ngrams <- sample$text %>% tokenize_character_shingles(n=3L, strip_non_alphanum = F)

## clean
library(stringr)
sample$text <- str_replace_all(sample$text, "\n", " ")
## str_squish() also reduces repeated whitespace inside a string
sample$text <- str_squish(sample$text)

## repeat tokenize and tidy
ngrams <- sample$text %>% tokenize_character_shingles(n=3L, strip_non_alphanum = F)
ngrams[[1]]

ngram.tbl <- as_tibble(ngrams[[1]]) %>% 
  mutate(title = "Apology", .before = value) %>% 
  rename(text = value)

## separate characters
ngrams.sep <- ngram.tbl %>% 
  separate(col = text, into = c("chr1", "chr2"), sep=1) %>%
  separate(col = chr2, into = c("chr2", "chr3"), sep=1)

## filter ngrams with middle spaces
ngrams.filtered <- ngrams.sep %>% filter(chr2 == " ")

## define vowels
## https://en.wikipedia.org/wiki/Greek_diacritics 
vowels <- c("α  ε  η  ι  ο  υ  ω  ᾳ  ῃ  ῳ   ά  έ  ή  ί  ό  ύ  ώ  ᾴ  ῄ  ῴ    ὰ  ὲ  ὴ  ὶ  ὸ  ὺ  ὼ  ᾲ  ῂ  ῲ    ᾶ     ῆ  ῖ     ῦ  ῶ  ᾷ  ῇ  ῷ    ἀ  ἐ  ἠ  ἰ  ὀ  ὐ  ὠ  ᾀ  ᾐ  ᾠ   ἄ  ἔ  ἤ  ἴ  ὄ  ὔ  ὤ  ᾄ  ᾔ  ᾤ    ἂ  ἒ  ἢ  ἲ  ὂ  ὒ  ὢ  ᾂ  ᾒ  ᾢ    ἆ     ἦ  ἶ     ὖ  ὦ  ᾆ  ᾖ  ᾦ    ἁ  ἑ  ἡ  ἱ  ὁ  ὑ  ὡ  ᾁ  ᾑ  ᾡ  ἅ  ἕ  ἥ  ἵ  ὅ  ὕ  ὥ  ᾅ  ᾕ  ᾥ    ἃ  ἓ  ἣ  ἳ  ὃ  ὓ  ὣ  ᾃ  ᾓ  ᾣ    ἇ     ἧ  ἷ     ὗ  ὧ  ᾇ  ᾗ  ᾧ    ϊ   ϋ       ΐ   ΰ       ῒ   ῢ       ῗ   ῧ       ᾱ    ῑ   ῡ       ᾰ    ῐ   ῠ   ")  
vowels <- str_squish(vowels)
vowels.sep <- str_split(vowels, pattern = " ")

## filter ngrams
hiatus <- ngrams.filtered %>% filter(chr1 %in% vowels.sep[[1]]) %>% 
  filter(chr3 %in% vowels.sep[[1]])

## on objectionable and permissible types of hiatus see Brandwood ch. 17

## let's now compare two of Plato's dialogues
library(XML)
Chrm <- xmlTreeParse("./PlatoPerseusXML/Charmides.xml", useInternalNodes = T, isURL = FALSE)
Soph <- xmlTreeParse("./PlatoPerseusXML/Sophist.xml", useInternalNodes = T, isURL = FALSE)

said_Chrm <- getNodeSet(Chrm, 
                        "/tei:TEI//tei:text//tei:body//tei:div//tei:div//tei:p//tei:said",
                        namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))
said_Soph <- getNodeSet(Soph, 
                        "/tei:TEI//tei:text//tei:body//tei:div//tei:div//tei:p//tei:said",
                        namespaces = c(tei = "http://www.tei-c.org/ns/1.0"))

text_Chrm <- sapply(said_Chrm, xmlValue, "text")
text_Soph <- sapply(said_Soph, xmlValue, "text")

## clean
library(stringr)
text_Chrm <- str_replace_all(text_Chrm, "\n", " ")
text_Soph <- str_replace_all(text_Soph, "\n", " ")
text_Chrm <- str_replace_all(text_Chrm, "\t", " ")
text_Soph <- str_replace_all(text_Soph, "\t", " ")
text_Chrm <- str_squish(text_Chrm)
text_Soph <- str_squish(text_Soph)

## tokenize
ngrams_Chrm <- text_Chrm %>% tokenize_character_shingles(n=3L, strip_non_alphanum = F)
ngrams_Soph <- text_Soph %>% tokenize_character_shingles(n=3L, strip_non_alphanum = F)

## transform
Chrm.text <- as_tibble(unlist(ngrams_Chrm, use.names = F)) %>% 
  mutate(title = "Charmides", .before = value) %>% rename(text = value)
Soph.text <- as_tibble(unlist(ngrams_Soph, use.names = F)) %>% 
  mutate(title = "Sophist", .before = value) %>% rename(text = value)

tbl <- bind_rows(Chrm.text, Soph.text)

## separate chr
ngrams.sep <- tbl %>% 
  separate(col = text, into = c("chr1", "chr2"), sep=1) %>%
  separate(col = chr2, into = c("chr2", "chr3"), sep=1)

## filter ngrams with middle spaces
ngrams.filtered <- ngrams.sep %>% filter(chr2 == " ")

## total number of occassions for hiatus
total <- ngrams.filtered %>% group_by(title) %>% summarise(total = n())

## find hiatus
hiatus <- ngrams.filtered %>% filter(chr1 %in% vowels.sep[[1]]) %>% 
  filter(chr3 %in% vowels.sep[[1]])

## total hiatus
count_hiatus <- hiatus %>% group_by(title) %>% summarize(count = n())
count_hiatus <- count_hiatus %>% left_join(total, by = "title") %>% 
  mutate(freq = count/total)
