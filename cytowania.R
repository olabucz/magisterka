rm(list=ls())
library(rscopus)
library(dplyr)

#wczytanie i sprawdzenie klucza API
Sys.getenv('Elsevier_API')
have_api_key()

#1. pobranie danych o autorze na podstawie ID
autor <- author_df(au_id = "35956989500",verbose = FALSE)

#2. pobranie ID wszystkich artykulow autora
artykuly_id <- autor$eid

#3. wstawienie ID artykulow w string (tworzenie kwerend)
kwerendy <- sapply(1:length(artykuly_id), function (i){
  q <- paste("REF(", artykuly_id[i],")",sep ="")
}) 

#4. pobranie informacji o wszystkich artykulach cytujacych kazdy artykul danego autora
ref <- sapply(1:length(artykuly_id), function (j){
  r <- scopus_search(query = kwerendy[j], view = "COMPLETE") 
})

# zamiana pobranych danych na ramki danych
dane <- sapply(1:length(artykuly_id), function (k){
  r1 <- entries_to_citation_df(ref[,k]$entries)
})

# uzyskanie lat, w ktorych powstaly publikacje cytujace kazda publikacje danego autora
lata <- sapply(1:length(artykuly_id), function (l){
  y1 <- dane[[l]]$cover_date
  y2 <- substr(y1,1,4)
})

#5. ramka z cytowaniami - zliczenie cytowan kazdej publikacji
z_kazdego_roku <- sapply(1:length(artykuly_id), function (m){
  if (is.na(lata[[m]])){
    zkr <- 0
  }
  else
  {
    zkr <- table(lata[[m]])
  }
})

library(reshape2)
# zamiana listy na ramke danych (kolumny - lata, rzedy - publikacje, wartosci - liczba cytowan uzyskanych przez dana publikacje w danym roku)
cytowania <- dcast(melt(z_kazdego_roku), L1~Var1)
cytowania[is.na(cytowania)] <- 0
cytowania <- cytowania[,-c(1,ncol(cytowania))]

# zapis do pliku (wynik cytowan dla pojedynczego autora) #ZMIENIC NAZWE
write.csv(cytowania,"C:/Users/abuczek/Desktop/cytowania/cytowania.csv", row.names = FALSE)

# wektor cytowan (zmiana w czasie)
df <- matrix(ncol=length(cytowania), nrow=nrow(cytowania)) #ile w sumie cytowan do kazdego roku miala dana publikacja
for (i in 1:length(cytowania)){
    df[,i] <- rowSums(cytowania[1:i])
}
df <- data.frame(df)
#autor$`citedby-count` dla sprawdzenia z sumarycznymi danymi z ostatniego roku

# ustawienie wektorow cytowan w kolejnosci malejacej
posortowanie <- df
for (i in 1:length(cytowania)){
  posortowanie[,i] <- sort(df[,i], decreasing = TRUE)
}

# indeks Hirscha (wektor zmiany wartosci indeksu h w czasie)
indeks_h <- sapply(1:length(cytowania), function (n){
  ih <- tail(which(posortowanie[,n] >= seq_along(posortowanie[,n])), 1) 
})

aktualny_indeks_h <- tail(indeks_h, n=1)
