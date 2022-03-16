## set working directory
getwd()
setwd("~/R_Workflow/Elements_Stylometry")
list.files()

## save url
url <- "https://www.thelatinlibrary.com/cicero/off1.shtml"

## a wrong way to read the text! 
text.raw <- scan(url, what = "character", sep = "\n")

## load package
install.packages("stylo")
library(stylo)

## download text
download.file(url, destfile = "CiceroOff.txt")

## read text 
my_text <- load.corpus.and.parse(files = "CiceroOff.txt", corpus.dir = getwd(), 
                                 markup.type = "html", encoding = "UTF-8")

## save as character vector
my_text <- unlist(my_text, use.names = FALSE)

## save text 
save(my_text, file = "./Cicero/CiceroOff.Rdata")
write.table(my_text, quote = FALSE, row.names = FALSE, 
            col.names = FALSE, file = "./Cicero/CiceroOff.txt")

## remove text
rm(my_text)

## load text
load("CiceroOff.Rdata")

## remove text
rm(my_text)

## read text
my_text <- unlist((load.corpus.and.parse(files = "CiceroOff.txt", corpus.dir = getwd(), 
                                 markup.type = "plain", encoding = "UTF-8")), use.names = FALSE)

## make frequency table
freqABS <- make.frequency.list(my_text, value = TRUE, relative = FALSE)
freqREL <- make.frequency.list(my_text, value = TRUE, relative = TRUE)

## view the result
head(freqREL)
tail(freqABS)
