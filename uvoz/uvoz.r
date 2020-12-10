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


povprecje_drzave <- osebni_podatki_skupni %>% group_by(COUNTRY) %>% summarise(PTS = mean(PTS), REB = mean(REB), AST = mean(AST))


stevilo_igralcev <- povprecje_osebni_podatki %>% group_by(COUNTRY) %>% summarise(stevilo = n()) ##število igralcev iz vsake džave

povprecje_drzave$COUNTRY[povprecje_drzave$COUNTRY == 'USA'] <- 'United States'
povprecje_drzave$COUNTRY[povprecje_drzave$COUNTRY == 'Russia'] <- 'Russian Federation'
stevilo_igralcev$COUNTRY[stevilo_igralcev$COUNTRY == 'USA'] <- 'United States'
stevilo_igralcev$COUNTRY[stevilo_igralcev$COUNTRY == 'Russia'] <- 'Russian Federation'
stevilo_igralcev$COUNTRY[stevilo_igralcev$COUNTRY == 'South Korea'] <- 'Republic of Korea'

vsi_igralci <- sum(stevilo_igralcev$stevilo)
igralci_amerika <- unlist(stevilo_igralcev[69, 2])
igralci_ostali <- vsi_igralci - igralci_amerika

stevilo <- c(vsi_igralci, igralci_amerika, igralci_ostali)
ime <- c('vsi igralci', 'igralci amerika', 'igralci ostali')
vsi_amerika_svet <- data.frame(stevilo, row.names = ime)
amerika_svet <- data.frame(stevilo[-1], row.names = ime[-1])


stevilo_igralcev_brez_amerike_in_NA <- stevilo_igralcev[-c(69, 73),]
top10_drzav_igralci <- stevilo_igralcev_brez_amerike_in_NA %>% top_n(stevilo, n = 10)

top10_drzav_tocke <- povprecje_drzave %>% top_n(PTS, n =10)


povprecje_osebni_podatki$COUNTRY <- as.factor(povprecje_osebni_podatki$COUNTRY) 
svet <- left_join(x = World, y = povprecje_osebni_podatki, by = c('name' = 'COUNTRY'))
