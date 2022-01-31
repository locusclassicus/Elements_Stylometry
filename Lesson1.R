## set working directory
getwd()
setwd("~/R_Workflow/AntibarbariDigital")
list.files()

## save url
url <- "https://www.thelatinlibrary.com/cicero/off1.shtml"

## a wrong way to read the text! 
text.raw <- scan(url, what = "character", sep = "\n")

## load package
install.packages("stylo")
library(stylo)

## download text
download.file(url, file = "CiceroOff.txt")

## read text 
my_text <- load.corpus.and.parse(files = "all", corpus.dir = getwd(), 
                                 markup.type = "html", encoding = "UTF-8")

## save as character vector
my_text <- unlist(my_text, use.names = FALSE)

## save text 
save(my_text, file = "CiceroOff.Rdata")

## remove text
rm(my_text)

## upload text
load("CiceroOff.Rdata")

## make frequency table
freqABS <- make.frequency.list(my_text, value = TRUE, relative = FALSE)
freqREL <- make.frequency.list(my_text, value = TRUE, relative = TRUE)

## view the result
head(freqREL)
tail(freqABS)
