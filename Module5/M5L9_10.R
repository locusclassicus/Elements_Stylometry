# As a last part on our project on Cicero's letters to Atticus, 
# let us make a map of all the locations from which Cicero 
# writes his letters. To do so, we need our xml. Again.
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
locations <- c()

# extract data from xml
for (i in 1:length(books)){
  book <- books[[i]]
  letters <-  xmlChildren(book)
  
  for (j in 1:length(letters)) {
    letter <- letters[[j]]
    letter_label <- letter[["label"]]["seg"][1]
    letter_place <- xmlValue(letter_label)
    
    locations <- rbind(locations, letter_place)
  }
}

# villas names
## "Tusculano", "Pompeiano", "Formiano", "Puteolano",
## "Cumano", "Anti", "Asturae", "Arpinati" + "Romae"

# mutate
library(stringr)
locations <- as_tibble(locations) 
locations <- locations %>% 
  mutate(place = case_when(grepl("Tuscul", locations$seg) ~ "Tusculum",
                           grepl("Pompei", locations$seg) ~ "Pompeianum",
                           grepl("Formia", locations$seg) ~ "Formianum",
                           grepl("Puteol", locations$seg) ~ "Puteolanum",
                           grepl("Cuman", locations$seg) ~ "Cumanum",
                           grepl("Anti", locations$seg) ~ "Antium",
                           grepl("Astur", locations$seg) ~ "Astura",
                           grepl("Arpin", locations$seg) ~ "Arpinas",
                           grepl("Rom", locations$seg) ~ "Roma",
                           .default = "other")) 

stat_loc <- locations %>% group_by(place) %>% 
  summarise(n = n()) %>% filter(!place == "other")

stat_loc

# now we should find latitude and longitude for these places

# Antium: 41.44962477043274, 12.616437099751108 
#(perhaps swallowed up by the sea)

# Arpinas 41.65165181135183, 13.620123572773 (place not known)

# Astura: 41.42262637834914, 12.78322981727089 (place not known)

# Cumanum: 40.834775290584076, 14.087219678261587 (monte Nuovo)

# Formianum: where Cicero was murdered
# 41.25277376392804, 13.578596255010044

# Pompeianum: Street of the Tombs, Via delle Tombe
# 40.75283927219468, 14.479769053899993

# Puteolanum: 40.82934865595719, 14.126633874383538 (near Naples)

# Rome 41.890648715205764, 12.48712284015621 (Palatine hill)

# Tusculum: probably near Badia di Grottaferrata
# 41.786506316890744, 12.66631665709489 


lat <- as_tibble(c(41.44962477043274, 41.65165181135183,
                   41.42262637834914, 40.834775290584076,
                   41.2527737639280, 40.75283927219468,
                   40.82934865595719, 41.890648715205764,
                   41.786506316890744
                   ))
colnames(lat) <- "lat"

lng <- as_tibble(c(12.616437099751108, 13.620123572773,
                   12.78322981727089, 14.087219678261587,
                   13.578596255010044, 14.479769053899993,
                   14.126633874383538, 12.48712284015621,
                   12.66631665709489))
colnames(lng) <- "lng"

loc_data <- stat_loc %>% bind_cols(lat, lng)

library(leaflet)
loc_data %>% 
  leaflet() %>%
  addTiles() %>% 
  addCircleMarkers(lng = ~lng, lat = ~lat, weight = 1,
                   radius = ~sqrt(n)*5, 
                   popup = ~paste0(
                     "<b>", place,":</b>",  
                     "</br>", 
                     n),
                   color = 'black', fillColor = 'red',
                   ) 

# to publish
# Media library WP: add new, copy link
# http://antibarbari.ru/wp-content/uploads/2023/03/CiceroVillas.html
# in the post:
# <iframe style="width: 100%; height: 120px;" 
# src="http://antibarbari.ru/wp-content/uploads/2023/03/CiceroVillas.html">
# </iframe>
# 
