

y  <- osebni_podatki2019_2020
                                 
                                

y$PTS <- parse_double(y$PTS) ##spremeni caracter v double
y$REB <- parse_double(y$REB)
y$AST <- parse_double(y$AST)

yp <- y %>% 
  group_by(PLAYER, COUNTRY) %>% 
  summarise(PTS = mean(PTS), REB = mean(REB), AST = mean(AST))  ##združil po igralcih in državah ter naredil povprečje za tiste, ki so igrali v več sezonah

yp$COUNTRY[yp$COUNTRY == 'USA'] <- 'United States'
yp$COUNTRY[yp$COUNTRY != 'United States'] <- 'druga'
yp <- yp[!is.na(yp$COUNTRY),]  #odstranimo vrstice z NA




ypc <- yp
ypc$PLAYER <- NULL
ypc$COUNTRY <- NULL
row.names(ypc) <- yp$PLAYER

#poiščimo število skupin
fviz_nbclust(ypc, FUN = hcut, method = "wss")
fviz_nbclust(ypc, FUN = hcut, method = "silhouette")
fviz_nbclust(ypc, FUN = hcut, method = "gap_stat", nstart=25, nboot=300)


n <- 3
skupine <- hclust(dist(scale(ypc))) %>% cutree(n)

b <- data.frame(lipa=names(skupine), skupina=factor(skupine))
c <- inner_join(yp, data.frame(PLAYER=names(skupine),
                               skupina=factor(skupine)), by="PLAYER")

ggplot(inner_join(yp, data.frame(PLAYER=names(skupine),
                                     skupina=factor(skupine)), by="PLAYER")
       , aes(x=PTS, y=AST, color=skupina, shape=COUNTRY))  + geom_point()  +
  ggtitle("Število asistenc glede na število točk") +
  xlab("Povprečno število točk") + ylab("Povprečno število asistenc") +
  guides(color=guide_legend(title="Skupina"),
         shape=guide_legend(title="Država")) 
