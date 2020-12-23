# 4. faza: Analiza podatkov


x <- povprecje_osebni_podatki
x$COUNTRY[x$COUNTRY != 'United States'] <- 'druga'
x <- x[!is.na(x$COUNTRY),]  #odstranimo vrstice z NA

risba <- x %>% 
  ggplot(aes(x=PTS, y=REB, col=COUNTRY)) + 
  geom_point() +
  geom_text(
    data=x %>% mutate(n = row_number()), 
    aes(label=n),
    size=2,
    color="black",
    nudge_x = 0.03,
    nudge_y = 0.03
  ) + theme_minimal()

risba + ggsave("risba.pdf", device="pdf")