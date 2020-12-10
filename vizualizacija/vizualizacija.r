# 3. faza: Vizualizacija podatkov

# Uvozimo zemljevid.
data('World')

#združimo tabelo z številom igralcev iz posamezne države s tabelo world
svet_stevilo_igralcev <- left_join(x = World, y = stevilo_igralcev, by = c('name' = 'COUNTRY'))
zemljevid_stevila_igralcev <- tm_shape(svet_stevilo_igralcev) + tm_polygons("stevilo")
