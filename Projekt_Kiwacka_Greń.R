#PROJEKT GRUPOWY - CZÊŒÆ II
#Kiwacka Gabriella
#Greñ PIotrek

#Temat: Forbes Miliarderzy

setwd("C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy")
list.files()

#potrzebne biblioteki
library(stringi)
library(dplyr) #do arrange
library(writexl)
library(rgl)
library(RColorBrewer)
library(colorspace)
library(rvest)

#projektowa ramka danych
billionaires <- read.csv("Forbes_Billionaires_Projekt.csv", sep=",", dec=".")
billionaires <- billionaires[, -1]





#25 najbogatszych miliarderów wed³ug œredniej ze wszystkich lat
names <- billionaires$name #pobranie nazwisk miliarderów do wektora
#ramka ktora posluzy do stworzenia ramki z wartosciami miliarderó jako wartoœci numeryczne
kasa <- data.frame("2022" = billionaires$X2022, "2021" = billionaires$X2021, "2020" = billionaires$X2020,
                   "2019" = billionaires$X2019, "2018" = billionaires$X2018, "2017" = billionaires$X2017,
                   "2016" = billionaires$X2016, "2015" = billionaires$X2015 ) 
for (i in 1:2600){ #pêtla usuwaj¹ca znaki inne jak liczbowe z ramek wartoœci maj¹tku miliarderów
  for (j in 1:8){
    kasa[i, j] <- gsub("\\$", "", kasa[i, j])
    kasa[i, j] <- gsub(" B", "", kasa[i, j])
  }
}
#zamiana wartoœci ju¿ bez znaków nie liczbowych na typ numeryczny
kasa$X2022 <- as.numeric(kasa$X2022)
kasa$X2021 <- as.numeric(kasa$X2021)
kasa$X2020 <- as.numeric(kasa$X2020)
kasa$X2019 <- as.numeric(kasa$X2019)
kasa$X2018 <- as.numeric(kasa$X2018)
kasa$X2017 <- as.numeric(kasa$X2017)
kasa$X2016 <- as.numeric(kasa$X2016)
kasa$X2015 <- as.numeric(kasa$X2015)
#swtorzenie ramki z nazwiskami miliarderów i ich maj¹tkami jako numerycznymi liczbami
networth <- data.frame("names" = names, kasa)
#ramka która bêdzie zawieraæ œrednie wartoœci miliarderów na przestrzeni lat
avg_netw <- data.frame("names" = 1:2600, "AVG" = 1:2600)

#wype³nienie ramki avg_netw
for (i in 1:2600){
  suma <- 0
  for (j in 2:9){
    if(is.na(networth[i, j])){
      suma <- suma
    }
    else{
      suma <- suma + networth[i, j]
    }
  }
  avg_netw[i,1] <- names[i]
  avg_netw[i,2] <- round(suma/8, 2)
}
#posortowanie ramki avg_netw wed³ug œredniej maj¹tku malej¹co
avg_sorted <- arrange(avg_netw, desc(AVG))
bill <- arrange(billionaires, desc(billionaires$age))
#wyszukanie reszty informacji na temat miliarderów
?write
AVG_names <- avg_sorted[1:25,1]
AVG_wiek <- 1:25
AVG_kraj <- 1:25
AVG_inc <- 1:25
AVG_anw <- avg_sorted[1:25, 2]
AVG_act <- 1:25
for (i in 1:25){
  for (j in 1:2600){
    if(avg_sorted[i, 1] == billionaires[j, 2]){
      AVG_wiek[i] <- billionaires[j, 3]
      AVG_kraj[i] <- billionaires[j, 4]
      AVG_inc[i] <- billionaires[j, 6]
      AVG_act[i] <- billionaires[j, 7]
    }
  }
}

#sklejenie wszystkich informacji do jednej ramki danych
AVG_anw <- stri_paste("$", AVG_anw, sep="")
AVG_anw <- stri_paste(AVG_anw, "B", sep=" ")
Najbogatsi_wg_avg <- data.frame("Nazwisko" = AVG_names, "Wiek" = AVG_wiek, "Kraj" = AVG_kraj,
                                "Branza" = AVG_inc, "Srednia wartosc na przestrzeni lat" = AVG_anw, "Aktualna wartosc" = AVG_act)
#wysy³anie ramki do pliku csv i doc
write.table(Najbogatsi_wg_avg, "C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Najbogatsi wed³ug œredniej/Najbogatsi_wed³ug_œredniej.doc", quote = FALSE)
write.csv(Najbogatsi_wg_avg, "C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Najbogatsi wed³ug œredniej/Najbogatsi_wed³ug_œredniej.csv")

year <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)#wektor zawieraj¹cy lata, potrzebny do stworzenia wykresu

#tworzenie wykresu z maj¹tkami 5 najbogatszych na przestrzeni lat i wys³anie go do pdf i png
pdf("C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Najbogatsi wed³ug œredniej/5 Najbogatszych.pdf", width = 14, height = 9)
plot(year, networth[which(networth$names == Najbogatsi_wg_avg$Nazwisko[1]),9:2], type = "b", pch = 16, col = "brown3", xlab = "Rok",
     ylab = "Wrtoœæ maj¹tku [MLD]", bty = "n", main = c("Majatek 5 najbogatszych wed³ug œredniej", "na przestrzeni lat"), font.main = 2, las = 0, lty = 1, lwd = 1)
lines(year, networth[which(networth$names == Najbogatsi_wg_avg$Nazwisko[2]), 9:2], type = 'b', pch = 16, col = "chartreuse4", lwd = 1)
lines(year, networth[which(networth$names == Najbogatsi_wg_avg$Nazwisko[3]), 9:2], type = 'b', pch = 16, col = "cyan3", lwd = 1)
lines(year, networth[which(networth$names == Najbogatsi_wg_avg$Nazwisko[4]), 9:2], type = 'b', pch = 16, col = "darkgoldenrod2", lwd = 1)
lines(year, networth[which(networth$names == Najbogatsi_wg_avg$Nazwisko[5]), 9:2], type = 'b', pch = 16, col = "azure4", lwd = 1)
legend("topleft", networth$names[1:5], col = c("brown3", "chartreuse4", "cyan3", "darkgoldenrod2", "azure4"), pch = rep(16, 5), bty = "n")
dev.off()

png("C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Najbogatsi wed³ug œredniej/5 Najbogatszych.png", width = 1920, height = 1080)
plot(year, networth[which(networth$names == Najbogatsi_wg_avg$Nazwisko[1]),9:2], type = "b", pch = 16, col = "brown3", xlab = "Rok",
     ylab = "Wrtoœæ maj¹tku [MLD]", bty = "n", main = c("Majatek 5 najbogatszych wed³ug œredniej", "na przestrzeni lat"), font.main = 2, las = 0, lty = 1, lwd = 2)
lines(year, networth[which(networth$names == Najbogatsi_wg_avg$Nazwisko[2]), 9:2], type = 'b', pch = 16, col = "chartreuse4", lwd = 2)
lines(year, networth[which(networth$names == Najbogatsi_wg_avg$Nazwisko[3]), 9:2], type = 'b', pch = 16, col = "cyan3", lwd = 2)
lines(year, networth[which(networth$names == Najbogatsi_wg_avg$Nazwisko[4]), 9:2], type = 'b', pch = 16, col = "darkgoldenrod2", lwd = 2)
lines(year, networth[which(networth$names == Najbogatsi_wg_avg$Nazwisko[5]), 9:2], type = 'b', pch = 16, col = "azure4", lwd = 2)
legend("topleft", networth$names[1:5], col = c("brown3", "chartreuse4", "cyan3", "darkgoldenrod2", "azure4"), pch = rep(16, 5), bty = "n")
dev.off()






#Najwiêksze progresy na przestrzneni lat
#wektory pomocnicze do zawarcia w nich informacji na temat progresu/regresu i lat w których by³y zrobione
progres <- 1:2600
regres <- 1:2600
rokpr <- 1:2600
rokrg <- 1:2600

#wyszukanie najwiêkszych progresów i regresów maj¹tków ka¿dego miliardera oraz lat na przestrzeni których by³y zrobione
for(i in 1:2600){
  minn <- 300
  maxx <- 0
  a <- 0
  for(j in 9:3){
    if(!is.na(networth[i,j - 1]) & !is.na(networth[i,j])){
      a <- networth[i, j - 1] - networth[i, j]
    }
    else{
      a <- a
    }
    if(a > maxx){
      maxx <- a
      rokpr[i] <- stri_paste(colnames(networth)[j], " - ", colnames(networth)[j - 1])
    }
    else if(a < minn){
      minn <- a
      rokrg[i] <- stri_paste(colnames(networth)[j], " - ", colnames(networth)[j - 1])
    }
  }
  progres[i] <- maxx
  regres[i] <- minn
}
#wyrzucenie znaku  "X" z wektorów lat ¿eby otrzymaæ czysty wynik np 2021-2022 a nie X2021-X2022
rokpr <- gsub("X", "", rokpr)
rokrg <- gsub("X", "", rokrg)
#stworzenie ramek zawieraj¹cej informacje o nazwisko milionerów i ich najwiêkszy progres/regres i na przestrzeni jakich lat by³
Progres <- data.frame("Name" = billionaires$name, "MaxProgres" = progres, "Lata progresu" = rokpr)
Regres <- data.frame("Name" = billionaires$name, "MaxRegres" = regres, "Lata regresu" = rokrg)

#stworzenie spójnej ramki danych zawieraj¹cej szczegó³owe informacje na temat miliarderów i scalaj¹ca ich progresy i regresy w jednym miejscy (poprzednie ramki danych)
ZestawieniePr <- data.frame("Nazwisko" = Progres$Name, "Wiek" = billionaires$age[which(billionaires$name == Progres$Name)],
                      "Branza" = billionaires$industry[which(billionaires$name == Progres$Name)], "Najwiekszy progres majatku" = Progres$MaxProgres,
                      "Lata miedzy ktorymi najbardziej zwiekszyl/a majatek" = Progres$Lata.progresu, "Najwiekszy regres majatku" = Regres$MaxRegres[which(Regres$Name == Progres$Name)],
                      "Lata miedzy ktorymi najbardziej zmniejszyl/a majatek" = Regres$Lata.regresu[which(Regres$Name == Progres$Name)])

#ramkê posortowaliœmy wed³ug najwiêkszych progresów maj¹tku malej¹co i wys³aliœmy j¹ do pliku
ZestawieniePr <- arrange(ZestawieniePr, desc(Najwiekszy.progres.majatku))
write.csv(ZestawieniePr, "C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Zestawienie progresu maj¹tkowego/Zestawienie_progresu_maj¹tkowego.csv")

#nastêpnie stworzyliœmy wykres przedstawiaj¹cy maj¹tek na przestrzeni lat z zaznaczonym najelepszym progresem i regresem
#dla 10 miliarderów z najlepszymi progresami na przestrzeni lat (pos³uzy³a do tego ramka danych stworzona parê linijek wy¿ej ZestawieniePr)
TOP_10_PR <- ZestawieniePr[1:10,]
year <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)
pdf("C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Zestawienie progresu maj¹tkowego/Zestawienie progresu maj¹tkowego.pdf", width = 8, height = 5)
#w pêtli dla ka¿dego miliardera zamianiliœmy progres na wartoœæ liczbow¹ oraz pobraliœmy lata na przestrzni których by³ zrobiony
#analogicznie zrobiliœmy z regresem
for(i in 1:10){
  worth <- networth[which(networth$names == ZestawieniePr[i, 1]), 9:2]
  
  pr <- TOP_10_PR$Lata.miedzy.ktorymi.najbardziej.zwiekszyl.a.majatek[i]
  pr1 <- substr(pr, 1, 4)
  pr1 <- as.numeric(pr1)
  pr2 <- substr(pr, 8, 11)
  pr2 <- as.numeric(pr2)
  pr <- networth[which(networth$names == TOP_10_PR$Nazwisko[i]), 9:2]
  #mini ramka danych z 1 wiersza posiadaj¹ca dane maj¹tku tylko w latach w których wyst¹pi³ progres
  #potrzebne do dodania lini z progresem do wykresu
  for(j in 9:2){
    if(j == pr1 - 2013 | j == pr2 - 2013){
      pr[j - 1] <- pr[j - 1]
    }
    else{
      pr[j - 1] <- NA
    }
  }
  
  rg <- TOP_10_PR$Lata.miedzy.ktorymi.najbardziej.zmniejszyl.a.majatek[i]
  rg1 <- substr(rg, 1, 4)
  rg1 <- as.numeric(rg1)
  rg2 <- substr(rg, 8, 11)
  rg2 <- as.numeric(rg2)
  rg <- networth[which(networth$names == TOP_10_PR$Nazwisko[i]), 9:2]
  #mini ramka danych z 1 wiersza posiadaj¹ca dane maj¹tku tylko w latach w których wyst¹pi³ regres
  #potrzebne do dodania lini z regresem do wykresu
  for(j in 9:2){
    if(j == rg1 - 2013 | j == rg2 - 2013){
      rg[j - 1] <- rg[j - 1]
    }
    else{
      rg[j - 1] <- NA
    }
  }
  
  #tworzymy wykres maj¹tku na przestrzeni lat i dodajemy dwie linie progresu i regresu miêdzy latami w których wyst¹pi³y
  plot(year, worth, type = "b", pch = 16, col = "black", xlab = "Rok", ylab = "Wrtoœæ maj¹tku [MLD]", bty = "n", main = c(ZestawieniePr[i, 1]),
       font.main = 2, las = 0, lty = 1, lwd = 3)
  lines(year, pr, type = 'b', pch = 16, col = "green", lwd = 3)
  lines(year, rg, type = "b", pch = 16, col = "red", lwd = 3)
  legend("topleft", c("Wartoœæ maj¹tku na przestrzeni lat", "Najwiêkszy wzrost wartoœci maj¹tku", "Najwiêkszy spadek wartoœci maj¹tku"),
         col = c("black", "green", "red"), bty = "n", pch = c(16, 16, 16))
}
dev.off()





#Najbogatsi z ka¿dego roku
#pomocnicza ramka danych do których wpiszemy po 10 najbogatszych z ka¿dego roku 
Najbogatsi <- data.frame("2022"=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), "2021"=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),  "2020"=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                         "2019"=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), "2018"=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), "2017"=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                         "2016"=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), "2015"=c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))

#dla ka¿dego roku szukamy 10 najbogatszych porównuj¹c maj¹tki a nastêpnie usuwaj¹c maj¹tek najbogatszej osoby po ka¿dym przejœciu pêtli
#aby jej nie zduplikowa³o
for(i in 2:9){
  networth_pomoc <- networth
  j <- 1
  while(j <= 10){
    maxx <- 0
    id <- NA
    for(k in 1:2600){
      if(!is.na(networth_pomoc[k, i]) & networth_pomoc[k, i] > maxx){
        maxx <- networth_pomoc[k, i]
        id <- k
      }
      else{
        maxx <- maxx
      }
    }
    Najbogatsi[j, i-1] <- id
    j <- j + 1
    networth_pomoc[id, i] <- NA
  }
}

#pomocnicza ramka danych do której wpiszemy szczegó³owe informaje o 10 najbogatszych w ka¿dym roku 
Najbogatsi_rok <- data.frame("Ranking aktualnie" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), "Nazwisko" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), "Wiek" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                             "Kraj" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), "Zrodlo dochodww" =c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0), "Branza" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                             "Wartosc majatku w tym roku" = c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0))
YOR <- c(brewer.pal(10, "RdYlGn"))
YOR
YOR2 <- YOR[c(1:10)]

#wszystkie wykresy stworzone w pêtli poni¿ej s¹ wys³ane do jednego pdfa
pdf("C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Najbogatsi w roku/Wykresy zestawieñ.pdf", width = 8, height = 5)

#w pêtli odpowiednio wiersz po wierszu w zale¿noœci który akurat rok jest analizowany, wypisujemy dane o miliarderach z g³ównej tabeli billionaires
for(i in 1:8){
  for(j in 1:10){
    Najbogatsi_rok[j,] <- billionaires[which(billionaires$rank == Najbogatsi[j, i]), c(1:6, i+6)]
  }
  #nastêpnie w zale¿noœci, który rok jest analizowany, ramka jest wysy³ana do odpowiedniego pliku, a wykresy zestawienia 10 najbogatszych
  #oraz ich udzia³u w ogólnym maj¹tku tego roku s¹ wysy³ane do pdf
  if(i == 1){
    write.csv(Najbogatsi_rok, "C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Najbogatsi w roku/Najbogatsi_2022.csv")
    
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- gsub("\\$", "", Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- gsub(" B", "", Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- as.numeric(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    d <- max(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    barplot(Najbogatsi_rok$Wartosc.majatku.w.tym.roku, beside = TRUE, las = 2, col = YOR2, ylab = "Wartoœæ majatku [MLD]", ylim = c(0,d +20),
            main = "NAJBOGATSI ROK 2022", names.arg = Najbogatsi_rok$Nazwisko, fon.lab = 3)
    
    all <- networth[,2]
    suma <- 0
    #obliczamy sumê ogóln¹ maj¹tków w tym roku aby stworzyæ statystykê udzia³y 10 najbogatszych w danym roku 
    for(k in 1:2600){
      if(!is.na(all[k])){
        suma <- suma + all[k]
      }
    }
    suma
    #liczymy procentowy udzia³ miliarderów w maj¹tku tego roku
    sum <- sum(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    percentage <- sum/suma*100
    percentage <- round(percentage, 2)
    percentage <- stri_paste("Ich udzia³ w ogólnym maj¹tku to ", percentage, '%')
    #wykres ko³owy pokazuj¹cy obliczone wy¿ej dane
    pie(c(suma, sum), labels = c("Reszta miliarderów", "10 Najbogatszych w tym roku"), edges = 400, las = 2, col = c("aquamarine1", "azure2"),
        init.angle = 0, main = c("Zestawienie sumy maj¹tków 10 najbogatszych ludzi", "w roku 2022 z reszt¹ miliarderów"),
        xlab = percentage)
  }
  #ka¿dy kolejny if dzia³a identycznie do pierwszego tylko liczy dane dla innych lat i wysy³a do innych plików
  else if(i == 2){
    write.csv(Najbogatsi_rok, "C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Najbogatsi w roku/Najbogatsi_2021.csv")
    
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- gsub("\\$", "", Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- gsub(" B", "", Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- as.numeric(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    d <- max(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    barplot(Najbogatsi_rok$Wartosc.majatku.w.tym.roku, beside = TRUE, las = 2, col = YOR2, ylab = "Wartoœæ majatku [MLD]", ylim = c(0,d +20),
            main = "NAJBOGATSI ROK 2021", names.arg = Najbogatsi_rok$Nazwisko, fon.lab = 3)
    
    all <- networth[,3]
    suma <- 0
    for(k in 1:2600){
      if(!is.na(all[k])){
        suma <- suma + all[k]
      }
    }
    suma
    sum <- sum(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    percentage <- sum/suma*100
    percentage <- round(percentage, 2)
    percentage <- stri_paste("Ich udzia³ w ogólnym maj¹tku to ", percentage, '%')
    pie(c(suma, sum), labels = c("Reszta miliarderów", "10 Najbogatszych w tym roku"), edges = 400, las = 2, col = c("aquamarine1", "azure2"),
        init.angle = 0, main = c("Zestawienie sumy maj¹tków 10 najbogatszych ludzi", "w roku 2021 z reszt¹ miliarderów"),
        xlab = percentage)
  }
  else if(i == 3){
    write.csv(Najbogatsi_rok, "C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Najbogatsi w roku/Najbogatsi_2020.csv")
    
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- gsub("\\$", "", Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- gsub(" B", "", Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- as.numeric(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    d <- max(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    barplot(Najbogatsi_rok$Wartosc.majatku.w.tym.roku, beside = TRUE, las = 2, col = YOR2, ylab = "Wartoœæ majatku [MLD]", ylim = c(0,d +20),
            main = "NAJBOGATSI ROK 2020", names.arg = Najbogatsi_rok$Nazwisko, fon.lab = 3)
    
    all <- networth[,4]
    suma <- 0
    for(k in 1:2600){
      if(!is.na(all[k])){
        suma <- suma + all[k]
      }
    }
    suma
    sum <- sum(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    percentage <- sum/suma*100
    percentage <- round(percentage, 2)
    percentage <- stri_paste("Ich udzia³ w ogólnym maj¹tku to ", percentage, '%')
    pie(c(suma, sum), labels = c("Reszta miliarderów", "10 Najbogatszych w tym roku"), edges = 400, las = 2, col = c("aquamarine1", "azure2"),
        init.angle = 0, main = c("Zestawienie sumy maj¹tków 10 najbogatszych ludzi", "w roku 2020 z reszt¹ miliarderów"),
        xlab = percentage)
  }
  else if(i == 4){
    write.csv(Najbogatsi_rok, "C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Najbogatsi w roku/Najbogatsi_2019.csv")
    
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- gsub("\\$", "", Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- gsub(" B", "", Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- as.numeric(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    d <- max(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    barplot(Najbogatsi_rok$Wartosc.majatku.w.tym.roku, beside = TRUE, las = 2, col = YOR2, ylab = "Wartoœæ majatku [MLD]", ylim = c(0,d +20),
            main = "NAJBOGATSI ROK 2019", names.arg = Najbogatsi_rok$Nazwisko, fon.lab = 3)
    
    all <- networth[,5]
    suma <- 0
    for(k in 1:2600){
      if(!is.na(all[k])){
        suma <- suma + all[k]
      }
    }
    suma
    sum <- sum(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    percentage <- sum/suma*100
    percentage <- round(percentage, 2)
    percentage <- stri_paste("Ich udzia³ w ogólnym maj¹tku to ", percentage, '%')
    pie(c(suma, sum), labels = c("Reszta miliarderów", "10 Najbogatszych w tym roku"), edges = 400, las = 2, col = c("aquamarine1", "azure2"),
        init.angle = 0, main = c("Zestawienie sumy maj¹tków 10 najbogatszych ludzi", "w roku 2019 z reszt¹ miliarderów"),
        xlab = percentage)
  }
  else if(i == 5){
    write.csv(Najbogatsi_rok, "C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Najbogatsi w roku/Najbogatsi_2018.csv")
    
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- gsub("\\$", "", Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- gsub(" B", "", Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- as.numeric(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    d <- max(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    barplot(Najbogatsi_rok$Wartosc.majatku.w.tym.roku, beside = TRUE, las = 2, col = YOR2, ylab = "Wartoœæ majatku [MLD]", ylim = c(0,d +20),
            main = "NAJBOGATSI ROK 2018", names.arg = Najbogatsi_rok$Nazwisko, fon.lab = 3)
    
    all <- networth[,6]
    suma <- 0
    for(k in 1:2600){
      if(!is.na(all[k])){
        suma <- suma + all[k]
      }
    }
    suma
    sum <- sum(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    percentage <- sum/suma*100
    percentage <- round(percentage, 2)
    percentage <- stri_paste("Ich udzia³ w ogólnym maj¹tku to ", percentage, '%')
    pie(c(suma, sum), labels = c("Reszta miliarderów", "10 Najbogatszych w tym roku"), edges = 400, las = 2, col = c("aquamarine1", "azure2"),
        init.angle = 0, main = c("Zestawienie sumy maj¹tków 10 najbogatszych ludzi", "w roku 2018 z reszt¹ miliarderów"),
        xlab = percentage)
  }
  else if(i == 6){
    write.csv(Najbogatsi_rok, "C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Najbogatsi w roku/Najbogatsi_2017.csv")
    
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- gsub("\\$", "", Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- gsub(" B", "", Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- as.numeric(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    d <- max(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    barplot(Najbogatsi_rok$Wartosc.majatku.w.tym.roku, beside = TRUE, las = 2, col = YOR2, ylab = "Wartoœæ majatku [MLD]", ylim = c(0,d +20),
            main = "NAJBOGATSI ROK 2017", names.arg = Najbogatsi_rok$Nazwisko, fon.lab = 3)
    
    all <- networth[,7]
    suma <- 0
    for(k in 1:2600){
      if(!is.na(all[k])){
        suma <- suma + all[k]
      }
    }
    suma
    sum <- sum(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    percentage <- sum/suma*100
    percentage <- round(percentage, 2)
    percentage <- stri_paste("Ich udzia³ w ogólnym maj¹tku to ", percentage, '%')
    pie(c(suma, sum), labels = c("Reszta miliarderów", "10 Najbogatszych w tym roku"), edges = 400, las = 2, col = c("aquamarine1", "azure2"),
        init.angle = 0, main = c("Zestawienie sumy maj¹tków 10 najbogatszych ludzi", "w roku 2017 z reszt¹ miliarderów"),
        xlab = percentage)
  }
  else if(i == 7){
    write.csv(Najbogatsi_rok, "C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Najbogatsi w roku/Najbogatsi_2016.csv")
    
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- gsub("\\$", "", Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- gsub(" B", "", Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- as.numeric(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    d <- max(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    barplot(Najbogatsi_rok$Wartosc.majatku.w.tym.roku, beside = TRUE, las = 2, col = YOR2, ylab = "Wartoœæ majatku [MLD]", ylim = c(0,d +20),
            main = "NAJBOGATSI ROK 2016", names.arg = Najbogatsi_rok$Nazwisko, fon.lab = 3)
    
    all <- networth[,8]
    suma <- 0
    for(k in 1:2600){
      if(!is.na(all[k])){
        suma <- suma + all[k]
      }
    }
    suma
    sum <- sum(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    percentage <- sum/suma*100
    percentage <- round(percentage, 2)
    percentage <- stri_paste("Ich udzia³ w ogólnym maj¹tku to ", percentage, '%')
    pie(c(suma, sum), labels = c("Reszta miliarderów", "10 Najbogatszych w tym roku"), edges = 400, las = 2, col = c("aquamarine1", "azure2"),
        init.angle = 0, main = c("Zestawienie sumy maj¹tków 10 najbogatszych ludzi", "w roku 2016 z reszt¹ miliarderów"),
        xlab = percentage)
  }
  else if(i == 8){
    print(Najbogatsi_rok)
    write.csv(Najbogatsi_rok, "C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Najbogatsi w roku/Najbogatsi_2015.csv")
    
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- gsub("\\$", "", Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- gsub(" B", "", Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    Najbogatsi_rok$Wartosc.majatku.w.tym.roku <- as.numeric(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    d <- max(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    barplot(Najbogatsi_rok$Wartosc.majatku.w.tym.roku, beside = TRUE, las = 2, col = YOR2, ylab = "Wartoœæ majatku [MLD]", ylim = c(0,d +20),
            main = "NAJBOGATSI ROK 2015", names.arg = Najbogatsi_rok$Nazwisko, fon.lab = 3)
    
    all <- networth[,9]
    suma <- 0
    for(k in 1:2600){
      if(!is.na(all[k])){
        suma <- suma + all[k]
      }
    }
    suma
    sum <- sum(Najbogatsi_rok$Wartosc.majatku.w.tym.roku)
    percentage <- sum/suma*100
    percentage <- round(percentage, 2)
    percentage <- stri_paste("Ich udzia³ w ogólnym maj¹tku to ", percentage, '%')
    pie(c(suma, sum), labels = c("Reszta miliarderów", "10 Najbogatszych w tym roku"), edges = 400, las = 2, col = c("aquamarine1", "azure2"),
        init.angle = 0, main = c("Zestawienie sumy maj¹tków 10 najbogatszych ludzi", "w roku 2015 z reszt¹ miliarderów"),
        xlab = percentage)
  }
}
dev.off()





#Zestawienie krajów z najwiêksz¹ iloœci¹ miliarderów
country <- as.factor(billionaires$country) #pobranie krajów z tabelki 
kraj <- levels(country) #pobranie krajów jako unikatowe wartoœci
country <- table(country) #pobranie liczby miliarderów z ka¿dego kraju 
liczba_w_kraju <- as.numeric(country) #przedstawienie tej liczby jako wartoœæ numeryczna
#wektor pomocniczy
najbogatszy <- 1:75
najbogatszy

#tworzymy pomocnicz¹ ramkê z aktualnymi maj¹tkami miliarderów i szukamy najbogatszego w ka¿dym kraju aktualnie
for(j in 1:75){
  ct <- kraj[j]
  x <- data.frame("names" = networth$names, "X2022" = networth$X2022)
  i <- max(x$X2022[which(billionaires$country == ct)])
  najbogatszy[j] <- x$names[which(x$X2022 == i)]
}

#tworzenie ramki danych z informacj¹ o kraju, ile ma miliarderów i który miliarder z tego kraju jest aktualnie najbogatszy
Countries <- data.frame("Kraj" = kraj, "Liczba miliarderow w kraju" = liczba_w_kraju, "Aktualnie najbogatszy w kraju" = najbogatszy)
Countries <- arrange(Countries, desc(Liczba.miliarderow.w.kraju)) #sortujemy ramkê wzglêdem liczby miliarderów malej¹co

#tworzymy pomocnicze wektory w celu stworzenia czytelnego wykresu
kraje1 <- Countries$Liczba.miliarderow.w.kraju[1:12]
kraje2 <- sum(Countries$Liczba.miliarderow.w.kraju[13:23])
kraje3 <- sum(Countries$Liczba.miliarderow.w.kraju[23:75])

#wysy³amy ramkê danych do pliku csv
write.csv(Countries, "C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Zestawienie krajów/Kraje z najwiêksz¹ iloœci¹ miliarderów.csv")

#sumujemy liczbê miliarderów z niektórych krajów
suma_20_40 <- sum(Countries$Liczba.miliarderow.w.kraju[13:23])
suma_1_20 <- sum(Countries$Liczba.miliarderow.w.kraju[24:75])

#liczmy procentowy udzia³ pierwszych 12 krajów oraz reszty w liczbie miliarderów aby utworzyæ czytelny wykres
piepercent <- c(round(Countries$Liczba.miliarderow.w.kraju[1]/2600*100, 1), round(Countries$Liczba.miliarderow.w.kraju[2]/2600*100, 1), round(Countries$Liczba.miliarderow.w.kraju[3]/2600*100, 1),
                round(Countries$Liczba.miliarderow.w.kraju[4]/2600*100, 1), round(Countries$Liczba.miliarderow.w.kraju[5]/2600*100, 1), round(Countries$Liczba.miliarderow.w.kraju[6]/2600*100, 1),
                round(Countries$Liczba.miliarderow.w.kraju[7]/2600*100, 1), round(Countries$Liczba.miliarderow.w.kraju[8]/2600*100, 1), round(Countries$Liczba.miliarderow.w.kraju[9]/2600*100, 1),
                round(Countries$Liczba.miliarderow.w.kraju[10]/2600*100, 1), round(Countries$Liczba.miliarderow.w.kraju[11]/2600*100, 1), round(Countries$Liczba.miliarderow.w.kraju[12]/2600*100, 1),
                round(suma_20_40/2600 * 100, 1), round(suma_1_20/2600*100, 1))

#wybieramy paletê kolorów
paleta <- choose_palette()
View(paleta)
pa <- paleta(14)

#tworzymy wykres z zestawionymi zebranymi informacjami i wysy³amy go do pdfa oraz png
pdf("C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Zestawienie krajów/Zestawienie krajów.pdf", height = 8, width = 14)
pie(c(kraje1, kraje2, kraje3), labels = stri_paste(piepercent, "%"), edges = 600, col = pa, main = c("Zestawienie krajów wedlug liczby", "miliarderów pochodz¹cych z kraju"))
legend("bottomleft", c(Countries$Kraj[1:12], "Kraje z l. miliarderów miêdzy 20 - 40", "Kraje z l. miliraderów miêdzy 1 - 20"),
       col = pa, bty = "n", pch = 15)
dev.off()

png("C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Zestawienie krajów/Zestawienie krajów.png", height = 800, width = 800)
pie(c(kraje1, kraje2, kraje3), labels = stri_paste(piepercent, "%"), edges = 600, col = pa, main = c("Zestawienie krajów wedlug liczby", "miliarderów pochodz¹cych z kraju"))
legend("bottomleft", c(Countries$Kraj[1:12], "Kraje z l. miliarderów miêdzy 20 - 40", "Kraje z l. miliraderów miêdzy 1 - 20"),
       col = pa, bty = "n", pch = 15)
dev.off()





#Funkcja zestawiaj¹ca dane konkretnej osoby
zestawienie <- function(imie){
  #pobieramy konretne dane osoby która nas interesuje
  rank <- billionaires[which(billionaires$name == imie), 1]
  wiek <- billionaires[which(billionaires$name == imie), 3]
  count <- billionaires[which(billionaires$name == imie), 4]
  zrodelko <- billionaires[which(billionaires$name == imie), 5]
  branza <- billionaires[which(billionaires$name == imie), 6]
  p_worth <- billionaires[which(billionaires$name == imie), 7]
  p_progres <- ZestawieniePr[which(ZestawieniePr$Nazwisko== imie), 4]
  progres_lata <- ZestawieniePr[which(ZestawieniePr$Nazwisko== imie), 5]
  p_regres <- ZestawieniePr[which(ZestawieniePr$Nazwisko== imie), 6]
  regres_lata <- ZestawieniePr[which(ZestawieniePr$Nazwisko== imie), 7]
  
  #pobieramy z internetu aktualny kurs dolara
  sciezka <- paste("https://www.bankier.pl/waluty")
  path <- "/html/body/div[3]/div[1]/div[2]/div[1]/div[1]/div[2]/div/div[4]/div[2]/table"
  nodes <- html_nodes(read_html(sciezka), xpath=(path))
  kurs <- html_table(nodes)
  kurs <- kurs[[1]]
  #po pobraniu kursu pobieramy czas pobrania kursu
  czas <- Sys.time()
  #pobran¹ tabelê zmieniamy w ramkê danych
  kurs <- as.data.frame(kurs)
  #wybieramy z niej kurs dolara
  dolar <- as.vector(kurs[1,2])
  #zamieniamy przecinek na kropkê a nastêpnie sam dolar na wartoœæ numeryczn¹ aby mo¿na by³o przeliczyæ maj¹tek na PLN
  dolar <- gsub("\\,", '\\.', dolar)
  dolar <- as.numeric(dolar)
  
  #przeliczamy maj¹tek na z³otówki z zaokr¹gleniem do 2 miejsc po przecinku
  networth_PLN <- round(networth$X2022[which(networth$names == imie)] * dolar, 2)
  
  #tworzymy skrót od kraju pochodzenia miliardera
  words <- stri_extract_all_words(count) #wyci¹gamy s³owa z kraju pochodzenia poniewa¿ od tego zale¿y jak bêdzie wygl¹da³ skrót
  skrot <- ""
  #je¿eli nazwa kraju jest jednocz³onowa to skrót to pierwsze 3 litery zamienione na wielkie
  if(length(words[[1]]) == 1){
    for(i in words){
      skrot <- paste(skrot, toupper(substr(i, 1, 3)),sep = "", collapse = "")
    }
  }
  #je¿eli nazwa jest dwucz³onowa to skrót to dwie pierwsze litery z ka¿dego s³owa zamienione na wielkie
  else if(length(words[[1]]) == 2){
    for(i in words){
      skrot <- paste(skrot, toupper(substr(i, 1, 1)), sep = "", collapse = "")
    }
  }
  #je¿eli jest trzycz³onowa to podobnie jak w dwucz³onowej
  else if(length(words[[1]]) == 3){
      skrot <- paste(skrot, toupper(substr(i, 1, 1)), sep = "", collapse = "")
  }
  #pojawia siê jeden kraj którego nazwa jest skomplikowana i 4 cz³onowa, na dodatek skrót od pierwszych liter nic by nie mówi³
  #wiêc jest po prostu stworzony osobno
  else{
      skrot <- "St. KAN"
  }
  
  #nastêpnie funkcja wypisuje wszystkie zgromadzonne informacje, niektóre w odpowiednim formacie
    print(sprintf("Imiê i Nazwisko: %s", imie))
    print(sprintf("Kraj pochodzenia: %s", skrot))
    print(sprintf("Aktualne miejsce w rankingu: %s", rank))
    print(sprintf("Aktualna wartoœæ maj¹tku: %s", p_worth))
    print(paste(sprintf("Aktualna wartoœæ w PLN: %s", networth_PLN), sprintf("MLD z³")))
    print(sprintf("Aktualny kurs dolara: %g", dolar))
    print(sprintf("Data pobrania kursu: %s", format(czas, format = "%d %B %Y %H:%M")))
    print(sprintf("G³ówne Ÿród³o dochodu: %s", zrodelko))
    print(paste(sprintf("Najwiêkszy progres maj¹tku na przestrzeni lat: %s", "$"), sprintf("%s", p_progres), sprintf("%s", "B")))
    print(sprintf("Lata miêdzy, którymi nast¹pi³ progres: %s", progres_lata))
    print(paste(sprintf("Najwiêkszy regres maj¹tku na przestrzeni lat: %s", "$"), sprintf("%s", p_regres), sprintf("%s", "B")))
    print(sprintf("Lata miêdzy, którymi nast¹pi³ regres: %s", regres_lata))
  
    #nastêpnie dla wyszukanego miliardera tworzymy wykres maj¹tku na przestrzeni lat z zaznaczonym progresem i regresem
    #analogicznie jak w funkcji z progresem i regresem
  pr1 <- substr(progres_lata, 1, 4)
  pr1 <- as.numeric(pr1)
  pr2 <- substr(progres_lata, 8, 11)
  pr2 <- as.numeric(pr2)
  pr <- networth[which(networth$names == imie), 9:2]
  for(j in 9:2){
    if(j == pr1 - 2013 | j == pr2 - 2013){
      pr[j - 1] <- pr[j - 1]
    }
    else{
      pr[j - 1] <- NA
    }
  }
  
  rg1 <- substr(regres_lata, 1, 4)
  rg1 <- as.numeric(rg1)
  rg2 <- substr(regres_lata, 8, 11)
  rg2 <- as.numeric(rg2)
  rg <- networth[which(networth$names == imie), 9:2]
  for(j in 9:2){
    if(j == rg1 - 2013 | j == rg2 - 2013){
      rg[j - 1] <- rg[j - 1]
    }
    else{
      rg[j - 1] <- NA
    }
  }
  
  year <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022)
  worth <- networth[which(networth$names == imie), 9:2]
  plot(year, worth, type = "b", pch = 16, col = "black", xlab = "Rok", ylab = "Wrtoœæ maj¹tku [MLD]", bty = "n", main = c(ZestawieniePr[i, 1]),
       font.main = 2, las = 0, lty = 1, lwd = 3)
  lines(year, pr, type = 'b', pch = 16, col = "green", lwd = 3)
  lines(year, rg, type = "b", pch = 16, col = "red", lwd = 3)
  legend("topleft", c("Wartoœæ maj¹tku na przestrzeni lat", "Najwiêkszy wzrost wartoœci maj¹tku", "Najwiêkszy spadek wartoœci maj¹tku"),
         col = c("black", "green", "red"), bty = "n", pch = c(16, 16, 16))
}




#Wyszukiwanie i zlicznanie miliarderów powy¿ej lub poni¿ej podanego roku ¿ycia
powyzej_ponizej <- function(l, po){
  w <- l
  #usuwamy dodatkowe spacje z wpisanego s³owa oraz zmieniamy litery jako wielkie 
  po <- gsub(" ", "", po)
  po <- gsub("  ", "", po)
  po <- gsub("   ", "", po)
  po <- toupper(po)
  #pobieramy wiek miliarderów z ramki billionaires
  age <- billionaires$age
  
  #funkcja zwracaj¹ca wiek miliarderów potrzebna do wykresu
  f <- function(x){
    as.numeric(billionaires$age[x] - l) #wiek pomniejszyliœmy o wyszukany aby ³adnie wygl¹da³o to na histogramie
  }
  f(2) #sprawdzenie dzia³ania funkcji
  x <- 1:2600 #wektor pomocniczy do wykresu
  
  #tworzymy histogram wieku miliarderów wzglêdem wybranego wieku i wysy³ami go do pdfa
  pdf("C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Rok ¿ycia/Histogram wed³ug wieku.pdf", width = 20, height = 5)
  plot(x, f(x), type = "h", col = terrain.colors(3600), xlab="", ylab="", main = stri_paste("Miliarderzy wzglêdem ", l, " roku ¿ycia"))
  lines(x, rep(0, 2600), type = "b", pch = 16, col = "black", lwd = 1)
  legend("bottomleft", stri_paste("Linia oznacza ", l, " rok ¿ycia."), col = "black", bty = "n", pch = 16)
  dev.off()
  
  #nastêpnie w zale¿noœci od wybranego parametru tworzymy ramkê danych 
  if(po == "POWYZEJ"){
    #wektory pomocnicze o d³ugoœci wierszy ramki billionaires poniewa¿ nie wiemy ile osób powy¿ej wybranego roku bêdzie
    pow_age <- rep(NA, 2600)
    pow_name <- rep(NA, 2600)
    pow_rank <- rep(NA, 2600)
    
    #wyszukujemy osoby powy¿ej wybranego roku i pobieramy interesuj¹ce nas informacje do wektorów
    for(i in 1:2600)
    {
      if(billionaires$age[i] >= l){
        pow_age[i] <- billionaires$age[i]
        pow_name[i] <- billionaires$name[i]
        pow_rank[i] <- billionaires$rank[i]
        #usuwamy wszystkie dane NA z wektorów i tworzymy ramkê danych z zebranych informacji
        pow_age <- pow_age[!is.na(pow_age)]
        pow_name <- pow_name[!is.na(pow_name)]
        pow_rank <- pow_rank[!is.na(pow_rank)]
        POWYZEJ <- data.frame("Nazwisko" = pow_name, "Wiek" = pow_age, "Aktualne miejsce w rankingu" = pow_rank)
      }
    }
    #dodajemy do ramki do pierwszego wiersza info na temat jaki wiek by³ wybrany do wyszukania osób powy¿ej tego roku ¿ycia
    w_rok <- c("Wybrany wiek:", w, NA)
    POWYZEJ <- rbind(w_rok, POWYZEJ)
    #wysy³amy ramkê do pliku
    write.csv(POWYZEJ, "C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Rok ¿ycia/Miliarderzy powyzej wybranego roku zycia.csv")
  }
  else if(po == "PONIZEJ"){
    #wektory pomocnicze o d³ugoœci wierszy ramki billionaires poniewa¿ nie wiemy ile osób poni¿ej wybranego roku bêdzie
    pon_age <- rep(NA, 2600)
    pon_name <- rep(NA, 2600)
    pon_rank <- rep(NA, 2600)
    
    #wyszukujemy osoby poni¿ej wybranego roku i pobieramy interesuj¹ce nas informacje do wektorów
    for(i in 1:2600)
    {
      if(billionaires$age[i] <= l){
        pon_age[i] <- billionaires$age[i]
        pon_name[i] <- billionaires$name[i]
        pon_rank[i] <- billionaires$rank[i]
        #usuwamy wszystkie dane NA z wektorów i tworzymy ramkê danych z zebranych informacji
        pon_age <- pon_age[!is.na(pon_age)]
        pon_name <- pon_name[!is.na(pon_name)]
        pon_rank <- pon_rank[!is.na(pon_rank)]
        PONIZEJ <- data.frame("Nazwisko" = pon_name, "Wiek" = pon_age, "Aktualne miejsce w rankingu" = pon_rank)
      }
    }
    #dodajemy do ramki do pierwszego wiersza info na temat jaki wiek by³ wybrany do wyszukania osób poni¿ej tego roku ¿ycia
    w_rok <- c("Wybrany wiek:", w, NA)
    PONIZEJ <- rbind(w_rok, PONIZEJ)
    #wysy³ami ramkê do pliku
    write.csv(PONIZEJ, "C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Rok ¿ycia/Miliarderzy ponizej wybranego roku zycia.csv")
  }
}




#funkcja wybieraj¹ca i analizuj¹ca bran¿ê
#tworzymy pomocnicz¹ ramke danych tak¹ jak networth tylko z ujêtymi bran¿ami miliarderów
branzaa <- 1:2600
for(i in 1:2600){
  branzaa[i] <- billionaires[i, 6]
}
networth_branza <- cbind(networth[,1], branzaa)
networth_branza <- cbind(networth_branza, networth[,2:9])
colnames(networth_branza) <- c("names", "branza", "2022", "2021", "2020", "2019", "2018", "2017", "2016", "2015")

#funkcja 
branza <- function(b){
  #branza <- data.frame("Ranking aktualnie" = c("Wybrana branza", rep(NA, 2599)), "Nazwisko" = c(b, rep(NA, 2599)), "Wiek" = )
  #jak w funkcji powyzej_ponizej() zamieniamy s³owo na du¿e litery i usuwamy nadmiarowe spacje
  b <- gsub(" ", "", b)
  b <- gsub("  ", "", b)
  b <- gsub("   ", "", b)
  b <- toupper(b)
  #pobieramy do pomocniczej ramki ramke billionaires ¿eby zamieniæ w niej nazwy branzy tak aby funkcja mog³a wyszukaæ t¹ która nas interesuje
  bill <- billionaires
  bill$industry <- gsub(" ", "", bill$industry)
  bill$industry <- toupper(bill$industry)
  bill
  #pobieramy dane na temat miliarderów z danej bran¿y i ich aktualnego maj¹tku
  branza <- bill[which(bill$industry == b), c(1:5, 7)]
  colnames(branza) <- c("Aktualny ranking", "Nazwisko", "Wiek", "Kraj pochodzenia", "Zrodlo dochodu", "Aktualny.majatek") #zamieniamy nazwy kolumn
  branza <- rbind(c(NA, stri_paste("Wybrana branza: ", b), NA, NA, NA, NA, NA), branza) #dodajemy pierwszy wiersz który bêdzie mia³ informacje na temat wybranej w funkcji branzy
  
  #wysylamy stworzon¹ ramkê do pliku
  write.csv(branza, "C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Analiza branzy/Miliarderzy z wybranej branzy.csv")
  
  #aby obliczyæ udzia³ bran¿y w ogólnej sumie maj¹tków w 2022 roku (aktualny stan) najpierw pobieramy maj¹tku osób z wyszukanej bran¿y
  #i zamieniamy je na wartoœci numeryczne usuwaj¹c z nich znaki nieliczbowe
  maj <- branza$Aktualny.majatek
  maj <- gsub("\\$", "", maj)
  maj <- gsub(" B", "", maj)
  maj <- as.numeric(maj)
  maj <- maj[-1] #usuwamy bo jest tam NA bo ramka ma tam informacjê na temat wybranej branzy
  
  #tworzymy ramkê samych wartoœci miliarderów z wybranej branzy
  wartosci <- data.frame("Nazwisko" = branza$Nazwisko[-1], "Wartosc" = maj)
  wartosci <- arrange(wartosci, desc(Wartosc)) #sortujemy j¹ wartoœciami w dó³
  
  branze <- as.factor(billionaires$industry) #pobieramy branze jako factor
  branze <- levels(branze) #pobieramy unikatowe dane (nazwy branzy)
  
  #pomocnicza ramka w której zapiszemy informacje na temat sumy maj¹tków ka¿dej bran¿y
  suma <- data.frame("Branza" = branze, "suma" = rep(0, 18))
  
  #szukamy maj¹tków z danej bran¿y i sumujemy je wpisuj¹c do ramki przy odpowiedniej bran¿y
  for(i in 1:18){
    for(j in 1:2600){
      if(networth_branza$branza[j] == branze[i]){
        suma[i,2] <- suma[i,2] + networth_branza$`2022`[j]
      }
    }
  }
  #nazwy branz dajemy wielkimi literami oraz usuwamy spacje ¿eby porównaæ z wybran¹ bran¿¹
  suma$Branza <- toupper(suma$Branza)
  suma$Branza <- gsub(" ", "", suma$Branza)
  suma_wszystkich <- sum(suma[,2]) - suma[which(suma$Branza == b), 2] #sumujemy maj¹tek wszystkich bran¿y oprócz wybranej
  #obliczamy procentowy udzia³ naszej branzy i reszty branz razem
  piepercentage <- c(round(suma_wszystkich/sum(suma[,2]) * 100, 2), round(suma[which(suma$Branza == b), 2]/sum(suma[,2]) * 100, 2))
  
  year <- c(2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022) #wektor pomocniczy do wykresu
  pdf("C:/Users/Piotrek/Desktop/Uczelnia/Programowanie w R/Projekt Koñcowy/Analiza branzy/Analiza.pdf", height = 20, width = 20)
  #tworzymy 6 wykresów na raz, maj¹tkóW 6 najbogatszych z wybranej branzy
  par(mfrow=c(2,3), col = "black")
  for(i in 1:6){
    plot(year, networth[which(networth$names == wartosci$Nazwisko[i]), c(9:2)], type = "b", col = terrain.colors(20)[15], pch = 16, xlab = "Rok", ylab = "Wrtoœæ maj¹tku [MLD]", bty = "n",
         font.main = 2, las = 0, lty = 1, lwd = 3, main = wartosci$Nazwisko[i])
  }
  
  #ustawiamy wykresy znowy na wyœwietlanie jednego na raz i tworzymy wykres ko³owo procentowego udzia³u wybranej bran¿u w ogólnej liczbie maj¹tków
  par(mfrow = c(1,1), col = "black")
  pie(c(suma_wszystkich, suma[which(suma$Branza == b), 2]), labels = stri_paste(piepercentage, "%"), edges = 600, col = c("darkolivegreen", "darkgray"), main = c("Udzia³ branzy ", b, "w sumie majatkow miliarderow", "w roku 2022"))
  legend("bottomright", c("Reszta branzy", b), col = c("darkolivegreen", "darkgray"), bty = "n", pch = 15)
  dev.off()
  
}




#FUNKCJE KTÓRE ZOSTA£Y STWORZONE W PROGRAMIE
#Funkcja wyszukuj¹ca szczegó³owe dane konkretnej osoby (nale¿y wpisaæ imiê i nazwisko ze spacj¹ na koñcu) np.:
zestawienie("Elon Musk ")
zestawienie("Amancio Ortega ")
#Funkcja zwracaj¹ca miliarderóW powy¿ej lub poni¿ej któregoœ roku ¿ycia (nale¿y wpisaæ rok ¿ycia oraz s³owo poni¿ej lub powyzej) np.:
powyzej_ponizej(63, "POWYZEJ")
powyzej_ponizej(58, "poni zej")
powyzej_ponizej(69, "PoWYZ  e J")
#Funkcja analizuj¹ca wybran¹ bran¿ê (nale¿y wpisaæ nazwê bran¿y wystêpuj¹cej w tabeli) np.:
b <- as.factor(billionaires$industry)
levels(b)
branza("TECHNOLOGY")
branza("Finance & InvestmentS")
branza("Sports")




#Przygotowali:    Gabriella Kiwacka   &   Piotrek Greñ
