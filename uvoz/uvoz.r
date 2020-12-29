# 2. faza: Uvoz podatkov

library(readxl)
osebni_podatki2000_2001 <- read_excel("podatki/osebni_podatki2000_2001.xlsx")
osebni_podatki2001_2002 <- read_excel("podatki/osebni_podatki2001_2002.xlsx")
osebni_podatki2002_2003 <- read_excel("podatki/osebni_podatki2002_2003.xlsx")
osebni_podatki2003_2004 <- read_excel("podatki/osebni_podatki2003_2004.xlsx")
osebni_podatki2004_2005 <- read_excel("podatki/osebni_podatki2004_2005.xlsx")
osebni_podatki2005_2006 <- read_excel("podatki/osebni_podatki2005_2006.xlsx")
osebni_podatki2006_2007 <- read_excel("podatki/osebni_podatki2006_2007.xlsx")
osebni_podatki2007_2008 <- read_excel("podatki/osebni_podatki2007_2008.xlsx")
osebni_podatki2008_2009 <- read_excel("podatki/osebni_podatki2008_2009.xlsx")
osebni_podatki2009_2010 <- read_excel("podatki/osebni_podatki2009_2010.xlsx")
osebni_podatki2010_2011 <- read_excel("podatki/osebni_podatki2010_2011.xlsx")
osebni_podatki2011_2012 <- read_excel("podatki/osebni_podatki2011_2012.xlsx")
osebni_podatki2012_2013 <- read_excel("podatki/osebni_podatki2012_2013.xlsx")
osebni_podatki2013_2014 <- read_excel("podatki/osebni_podatki2013_2014.xlsx")
osebni_podatki2014_2015 <- read_excel("podatki/osebni_podatki2014_2015.xlsx")
osebni_podatki2015_2016 <- read_excel("podatki/osebni_podatki2015_2016.xlsx")
osebni_podatki2016_2017 <- read_excel("podatki/osebni_podatki2016_2017.xlsx")
osebni_podatki2017_2018 <- read_excel("podatki/osebni_podatki2017_2018.xlsx")
osebni_podatki2018_2019 <- read_excel("podatki/osebni_podatki2018_2019.xlsx")
osebni_podatki2019_2020 <- read_excel("podatki/osebni_podatki2019_2020.xlsx")


# Urejanje

osebni_podatki_skupni <- rbind(osebni_podatki2009_2010, osebni_podatki2010_2011,
                               osebni_podatki2011_2012, osebni_podatki2012_2013,
                               osebni_podatki2013_2014, osebni_podatki2014_2015,
                               osebni_podatki2015_2016, osebni_podatki2016_2017,
                               osebni_podatki2017_2018, osebni_podatki2018_2019,
                               osebni_podatki2019_2020,
                               osebni_podatki2000_2001, osebni_podatki2001_2002,
                               osebni_podatki2002_2003, osebni_podatki2003_2004,
                               osebni_podatki2004_2005, osebni_podatki2005_2006,
                               osebni_podatki2006_2007, osebni_podatki2007_2008) 


osebni_podatki_skupni$PTS <- parse_double(osebni_podatki_skupni$PTS) ##spremeni caracter v double
osebni_podatki_skupni$REB <- parse_double(osebni_podatki_skupni$REB)
osebni_podatki_skupni$AST <- parse_double(osebni_podatki_skupni$AST)


povprecje_osebni_podatki <- osebni_podatki_skupni %>% 
  group_by(PLAYER, COUNTRY) %>% 
  summarise(PTS = mean(PTS), REB = mean(REB), AST = mean(AST))  ##združil po igralcih in državah ter naredil povprečje za tiste, ki so igrali v več sezonah


povprecje_osebni_podatki$COUNTRY[povprecje_osebni_podatki$COUNTRY == 'USA'] <- 'United States'
povprecje_osebni_podatki$COUNTRY[povprecje_osebni_podatki$COUNTRY == 'Czech Republi...'] <- 'Czech Rep.'
povprecje_osebni_podatki$COUNTRY[povprecje_osebni_podatki$COUNTRY == 'Dominican Rep...'] <- 'Dominican Rep.'
povprecje_osebni_podatki$COUNTRY[povprecje_osebni_podatki$COUNTRY == 'Serbia and Mo...'] <- 'Serbia'
povprecje_osebni_podatki$COUNTRY[povprecje_osebni_podatki$COUNTRY == 'Bosnia and He...'] <- 'Bosnia and Herz.'
povprecje_osebni_podatki$COUNTRY[povprecje_osebni_podatki$COUNTRY == 'Democratic Re...'] <- 'Dem. Rep. Congo'
povprecje_osebni_podatki$COUNTRY[povprecje_osebni_podatki$COUNTRY == 'DRC'] <- 'Dem. Rep. Congo'
povprecje_osebni_podatki$COUNTRY[povprecje_osebni_podatki$COUNTRY == 'United Kingdo...'] <- 'United Kingdom'


povprecje_drzave <- osebni_podatki_skupni %>% group_by(COUNTRY) %>% summarise(PTS = mean(PTS), REB = mean(REB), AST = mean(AST))


stevilo_igralcev <- povprecje_osebni_podatki %>% group_by(COUNTRY) %>% summarise(stevilo = n()) ##število igralcev iz vsake džave
stevilo_igralcev <- stevilo_igralcev[!is.na(stevilo_igralcev$COUNTRY),]  #odstranimo vrstice z NA
rr <- stevilo_igralcev
stevilo_igralcev$COUNTRY <- NULL
row.names(stevilo_igralcev) <- rr$COUNTRY


vsi_igralci <- sum(stevilo_igralcev$stevilo)
igralci_amerika <- unlist(stevilo_igralcev["United States", ])
igralci_ostali <- vsi_igralci - igralci_amerika


#tortni diagram
slices <- c(igralci_amerika, igralci_ostali)
lbls <- c('ZDA', 'Ostali')
pct <- round(slices/sum(slices)*100)
lbls <- paste(lbls, pct) # add percents to labels
lbls <- paste(lbls,"%",sep="") # ad % to labels



stevilo_igralcev_brez_amerike <- subset(rr, stevilo <= 1000)
top10_drzav_igralci <- stevilo_igralcev_brez_amerike %>% top_n(stevilo, n = 10)

top10_drzav_tocke <- povprecje_drzave %>% top_n(PTS, n =10)



