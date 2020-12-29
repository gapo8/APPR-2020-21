# 4. faza: Analiza podatkov




x <- povprecje_osebni_podatki
x$COUNTRY[x$COUNTRY != 'United States'] <- 'druga'
x <- x[!is.na(x$COUNTRY),]  #odstranimo vrstice z NA
x$PLAYER <- NULL

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


data <- x %>% 
  select(REB, PTS) %>%
  as.matrix() %>%
  scale()

data %>% View

D <- dist(data) # matrika različnosti
model <- hclust(D) # model

model$labels

### Vizualizacija rezultata - dendrogram
plot(model, hang=-1,  cex=0.3, main="I")

require(ggdendro)

ggdendrogram(model, labels=TRUE) 

ggdendrogram(model, rotate=TRUE, labels=TRUE) + 
  theme(
    axis.text.y = element_text(
      size = rel(0.2), 
      margin = margin(r=-20),
      color=x$COUNTRY[model$order]
    )
  ) +
  ggsave("dendro.pdf", device="pdf") 




