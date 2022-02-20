library(stylo)
## method one
my_text<- load.corpus.and.parse(files = "Xenophon_Hell.txt", corpus.dir = getwd(), 
                                markup.type= "plain", 
                                corpus.lang = "Other", 
                                splitting.rule = NULL,
                                sample.size = 1000, 
                                sampling = "normal.sampling",
                                sample.overlap = 0, 
                                sampling.with.replacement = FALSE, 
                                features = "w", 
                                ngram.size = 1, 
                                preserve.case = FALSE,
                                encoding = "UTF-8")

## method two (if your text is already loaded)
make.samples(tokenized.text, sample.size = 10000, 
             sampling = "no.sampling", sample.overlap = 0,
             number.of.samples = 1, sampling.with.replacement = FALSE)

## drop samples
