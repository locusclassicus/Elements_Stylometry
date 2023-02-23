## regular expressions for your own tokenizer
Apol <- c("τί δὲ δή; οἱ δὲ ἀκροαταὶ βελτίους ποιοῦσιν  ἢ οὔ; καὶ οὗτοι. τί δέ, οἱ βουλευταί;καὶ οἱ βουλευταί. ἀλλʼ ἄρα, ὦ Μέλητε, μὴ οἱ ἐν τῇ ἐκκλησίᾳ, οἱ ἐκκλησιασταί, διαφθείρουσι τοὺς νεωτέρους; ἢ κἀκεῖνοι βελτίους ποιοῦσιν ἅπαντες; κἀκεῖνοι.")
Metam <- c( "In nova fert animus mutatas dicere formas
           corpora; di, coeptis (nam vos mutastis et illas)
           adspirate meis primaque ab origine mundi
           ad mea perpetuum deducite tempora carmen!
             Ante mare et terras et quod tegit omnia caelum               5
           unus erat toto naturae vultus in orbe,
           quem dixere chaos: rudis indigestaque moles
           nec quicquam nisi pondus iners congestaque eodem
           non bene iunctarum discordia semina rerum.")

## split sentences in Latin
Metam.sent <- tokenize_sentences(Metam, lowercase = FALSE, strip_punct = FALSE,
                   simplify = FALSE)

## error if split sent in Greek 
Apol.sent <- tokenize_sentences(Apol, lowercase = FALSE, strip_punct = FALSE,
                                simplify = FALSE)

## not much better with uppercase
Apol <- c("τί δὲ δή; οἱ δὲ ἀκροαταὶ βελτίους ποιοῦσιν  ἢ οὔ;  Καὶ οὗτοι. Τί δέ, οἱ βουλευταί;καὶ οἱ βουλευταί. ἀλλʼ ἄρα, ὦ Μέλητε, μὴ οἱ ἐν τῇ ἐκκλησίᾳ, οἱ ἐκκλησιασταί, διαφθείρουσι τοὺς νεωτέρους; ἢ κἀκεῖνοι βελτίους ποιοῦσιν ἅπαντες; κἀκεῖνοι.")

## so we should write our own regex that splits a text at . or ; 


## If “.” matches any character, how do you match a literal “.”? 
## You need to use an “escape” to tell the regular expression you want 
## to match it exactly, not use its special behaviour. Like strings, 
## regexps use the backslash, \, to escape special behaviour. 
## So to match an ., you need the regexp \.. Unfortunately this creates 
## a problem. We use strings to represent regular expressions, and \ 
## is also used as an escape symbol in strings. So to create the regular
## expression \. we need the string "\\.".
## https://stringr.tidyverse.org/articles/regular-expressions.html


## this will only include dots
tokenize_regex(Apol, pattern = "\\.", simplify = FALSE)

## how do we include ; ?
tokenize_regex(Apol, pattern = ";", simplify = FALSE)

## now let's create a class
my_punct <- "[\\.;]"
tokenize_regex(Apol, pattern = my_punct, simplify = FALSE)

## as an option, you can use sentence tags in XML, if any
url <- "https://raw.githubusercontent.com/locusclassicus/LemmatizedAncientGreekXML/master/texts/tlg0059.tlg002.perseus-grc2.xml"
file.name = "Apology.xml"
download.file(url, file.name)
doc <- xmlTreeParse("./Apology.xml", useInternalNodes = T)
rootnode <- xmlRoot(doc)
sent <- getNodeSet(doc, "/text/s")
sent[[1]]

sent.length <- c()
for (i in 1:length(sent)) {
  l <- length(xmlChildren(sent[[i]]))
  sent.length <- c(sent.length, l)
}

mean(sent.length)
sd(sent.length)
library(MASS)
truehist(sent.length)
ggplot(data = as_tibble(sent.length), 
       mapping = aes(x = value)) + geom_histogram(fill = "lightblue") +
  geom_vline(aes(xintercept=mean(sent.length)), color = "darkblue") +
  geom_text(aes(x=mean(sent.length)+2.6, label="16.4", y=60), colour="blue", angle = 90)

## explore 
which.max(sent.length)

longest <- c()
for (i in 1:length(xmlChildren(sent[[376]]))) {
  token <- xmlValue(sent[[376]][[i]][["f"]])
  longest <- c(longest, token)
}
