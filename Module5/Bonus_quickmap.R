italy <- map_data("italy") 
italy %>% ggplot() + 
  geom_polygon(fill = "white", color = "black",
               mapping = aes(long, lat, group = group)) +
  coord_quickmap() + 
  geom_point(data = loc_data, aes(lng, lat, size = n), 
             color = "red", alpha = 0.5)


italy <- map_data("italy") %>% filter(lat < 44 & lat > 38, 
                                      long > 9 & long < 19)
italy %>% ggplot() + 
  geom_polygon(fill = "white", color = "grey40",
               mapping = aes(long, lat, group = group)) +
  coord_quickmap() + 
  geom_point(data = loc_data, aes(lng, lat, color = place), 
            alpha = 0.8, size = 3, show.legend = T)
  


