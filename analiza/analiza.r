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


### Vizualizacija rezultata - dendrogram
plot(model, hang=-1,  cex=0.3, main="I")

require(ggdendro)

ggdendrogram(model, labels=TRUE) 

ggdendrogram(model, rotate=TRUE, labels=TRUE) + 
  theme(
    axis.text.y = element_text(
      size = rel(0.2), 
      margin = margin(r=-20),
      color=iris$Species[model$order]
    )
  ) +
  ggsave("dendro.pdf", device="pdf") 


# Pridobitev podatkov za izris z ggplot
ddata <- dendro_data(model, type = "rectangle")

names(ddata)

segment(ddata) %>% View

ggplot(segment(ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) + 
  coord_flip() + 
  scale_y_reverse(expand = c(0.2, 0)) + 
  theme_dendro()

ddata$labels$label
iris$Species[model$order]

ddata$labels$label <- paste(iris$Species[model$order], ddata$labels$label %>% as.character)
ddata$labels$Species <- iris$Species[model$order]

dendrogg <- ggplot(segment(ddata)) + 
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend), size=0.1) + 
  coord_flip()  + 
  scale_y_reverse(expand = c(0.2, 0)) + 
  geom_text(
    data=ddata$labels, 
    aes(x=x, y=y, label=label, colour=Species), hjust=0, size=1, nudge_y = 0.03) +
  theme(axis.line.y=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.y=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill="white"),
        panel.grid=element_blank())

dendrogg + ggsave("dendrogg.pdf", device="pdf")

