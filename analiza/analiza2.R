

y  <- osebni_podatki2019_2020
                                 
                                

y$PTS <- parse_double(y$PTS) ##spremeni caracter v double
y$REB <- parse_double(y$REB)
y$AST <- parse_double(y$AST)

yp <- y %>% 
  group_by(PLAYER, COUNTRY) %>% 
  summarise(PTS = mean(PTS), REB = mean(REB), AST = mean(AST))  ##združil po igralcih in državah ter naredil povprečje za tiste, ki so igrali v več sezonah

yp$COUNTRY[yp$COUNTRY != 'United States'] <- 'druga'
yp <- yp[!is.na(yp$COUNTRY),]  #odstranimo vrstice z NA
yp$PLAYER <- NULL


risba <- yp %>% 
  ggplot(aes(x=PTS, y=REB, col=COUNTRY)) + 
  geom_point() +
  geom_text(
    data=yp %>% mutate(n = row_number()), 
    aes(label=n),
    size=2,
    color="black",
    nudge_x = 0.03,
    nudge_y = 0.03
  ) + theme_minimal()

risba + ggsave("risba.pdf", device="pdf")


row.names(yp) <- yp$COUNTRY

data <- yp %>% 
  select(PTS, REB, AST) %>%
  as.matrix() %>%
  scale()

tt <- dist(data)
gg <- hclust(tt)


plot(gg, hang=-1,  cex=0.3, main="yp") #dendogram


p <- cutree(gg, k=2)



n <- 2
skupine <- hclust(tt) %>% cutree(n)

data.frame(lipa=names(skupine), drevo=factor(skupine))


ggplot(inner_join(obcine, data.frame(obcina=names(skupine),
                                     skupina=factor(skupine)), by="obcina")
       , aes(x=povrsina, y=naselja, color=skupina, size=prebivalci/1000)) + geom_point() +
  ggtitle("Število naselij glede na površino občine") +
  xlab(expression("Površina (km"^2 * ")")) + ylab("Št. naselij") +
  guides(color=guide_legend(title="Skupina"),
         size=guide_legend(title="Prebivalci (* 1000)"))


