# 3. faza: Vizualizacija podatkov

# Uvozimo zemljevid.
data('World')


svet_stevilo_igralcev <- left_join(x = World, y = stevilo_igralcev, by = c('sovereignt' = 'COUNTRY'))#združimo tabelo z številom igralcev iz posamezne države s tabelo world


svet_stevilo_igralcev[is.na(svet_stevilo_igralcev)] <- 0 #spremenim na v 0

tmap_mode('view')
zemljevid_stevila_igralcev <- tm_shape(svet_stevilo_igralcev) + tm_polygons("stevilo", breaks = c(0, 1, 5, 10, 15, 20, 30, 40, Inf))

