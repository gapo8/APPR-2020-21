

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
row.names(c) <- c$PLAYER

ggplot(inner_join(yp, data.frame(PLAYER=names(skupine),
                                     skupina=factor(skupine)), by="PLAYER")
       , aes(x=PTS, y=AST, color=skupina, shape=COUNTRY))  + geom_point()  + geom_point(data = c[c('Luka Doncic','Goran Dragic'),], size=5) +
  ggtitle("Število asistenc glede na število točk") +
  xlab("Povprečno število točk") + ylab("Povprečno število asistenc") + 
  guides(color=guide_legend(title="Skupina"),
         shape=guide_legend(title="Država")) 



#države
p <- povprecje_drzave
p <- p[!is.na(p$COUNTRY),]  #odstranimo vrstice z NA
povprecje_drzave <- povprecje_drzave[!is.na(povprecje_drzave$COUNTRY),]  #odstranimo vrstice z NA


p$COUNTRY <- NULL
row.names(p) <- povprecje_drzave$COUNTRY


fviz_nbclust(p, FUN = hcut, method = "wss")
fviz_nbclust(p, FUN = hcut, method = "silhouette")
fviz_nbclust(p, FUN = hcut, method = "gap_stat", nstart=25, nboot=300)

k <- 3
grupe <- hclust(dist(scale(p))) %>% cutree(k)
pp <- inner_join(povprecje_drzave, data.frame(COUNTRY=names(grupe),grupa=factor(grupe)), by='COUNTRY')
row.names(pp) <- pp$COUNTRY

ggplot(pp, aes(x=PTS, y=AST, color=REB, shape=grupa))  + geom_point(size= 3)  + geom_point(data = pp['Slovenia',], size=7) +
  ggtitle("Število asistenc glede na število točk") +
  xlab("Povprečno število točk") + ylab("Povprečno število asistenc") + 
  guides(color=guide_legend(title="Povprečno število skokov"),
         shape=guide_legend(title="Grupa")) 
