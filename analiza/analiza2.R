

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


n <- 5
skupine <- hclust(dist(scale(ypc))) %>% cutree(n)

b <- data.frame(lipa=names(skupine), skupina=factor(skupine))
c <- inner_join(yp, data.frame(PLAYER=names(skupine),
                               skupina=factor(skupine)), by="PLAYER")

ggplot(inner_join(yp, data.frame(PLAYER=names(skupine),
                                     skupina=factor(skupine)), by="PLAYER")
       , aes(x=PTS, y=AST, color=skupina, shape=COUNTRY)) + geom_point() +
  ggtitle("Število naselij glede na površino občine") +
  xlab(expression("Površina (km"^2 * ")")) + ylab("Št. naselij") +
  guides(color=guide_legend(title="Skupina"),
         size=guide_legend(title="Prebivalci (* 1000)"))
