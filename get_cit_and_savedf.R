#############################################
## PROGRAM POBIERA DANE DOTYCZACE CYTOWAN  ##
## I AUTOCYTOWAN I ZAPISUJE DANE W PLIKACH ##
#############################################

rm(list=ls())
library(rscopus)
library(dplyr)
library(reshape2)

# wczytanie i sprawdzenie klucza API
Sys.getenv('Elsevier_API')
have_api_key()

# przypisanie id autora do zmiennej
author_id <- "35956989500"

# pobranie danych o autorze na podstawie ID
author <- author_df(au_id = author_id,verbose = FALSE)

# pobranie ID wszystkich artykulow autora
articles_ids <- author$eid

# wstawienie ID artykulow w string (tworzenie kwerend)
queries <- sapply(1:length(articles_ids), function (i){
  q <- paste("REF(", articles_ids[i],")",sep ="")
}) 

# pobranie informacji o wszystkich artykulach cytujacych kazdy artykul danego autora
ref <- sapply(1:length(articles_ids), function (j){
  r <- scopus_search(query = queries[j], view = "COMPLETE") 
})

# zamiana pobranych danych na ramki danych
df <- sapply(1:length(articles_ids), function (k){
  r1 <- entries_to_citation_df(ref[,k]$entries)
})

# uzyskanie lat, w ktorych powstaly publikacje cytujace kazda publikacje danego autora
yrs <- sapply(1:length(articles_ids), function (l){
  y1 <- df[[l]]$cover_date
  y2 <- substr(y1,1,4)
})

# ramka z cytowaniami - zliczenie cytowan kazdej publikacji
each_year <- sapply(1:length(articles_ids), function (m){
  if (is.na(yrs[[m]])){
    yr <- 0
  }
  else
  {
    yr <- table(yrs[[m]])
  }
})

cit <- dcast(melt(each_year), L1~Var1)
cit[is.na(cit)] <- 0
cit <- cit[,-c(1,ncol(cit))]

# zapis cytowan do pliku
# nazwa pliku do zapisu
file_name_cit <- paste(author_id, "_cit.csv", sep ="")

# sciezka do pliku
file_path_cit <- paste("C:/Users/abuczek/Desktop/cytowania/", file_name_cit, sep ="")

# zapis ramki danych z cytowaniami do pliku 
write.csv(cit, file = file_path_cit, row.names = FALSE)

#autocytowania
no_pub <- c()
yr_pub <- c()

# sprawdzenie w kazdej publikacji id wszystkich autorow
for (i in 1:length(articles_ids))  #dla kazdego artykulu danego autora
{
  for (j in 1:length(ref[,i]$entries)) #dla wszystkich artykulow, ktore cytuja dany artykul
  {
    for (k in 1:length(ref[,i]$entries[[j]]$author)) #dla kazdego autora tego artykulu
    {
      if (!is.null(ref[,i]$entries[[j]]$author[[k]]$authid))
      {
        if((ref[,i]$entries[[j]]$author[[k]]$authid) == author_id)
        {
          #nr publikacji oraz rok, w ktorych nastapilo autocytowanie
          no_pub <- append(no_pub, i)
          yr_pub <- append(yr_pub, substr(ref[,i]$entries[[j]]$`prism:coverDate`,1,4))
        }
      }
    }
  }
}

selfcit <- cit
selfcit[,] <- 0

# ramka danych z autocytowaniami
for(i in 1:length(no_pub)){
  selfcit[no_pub[i],yr_pub[i]] = selfcit[no_pub[i],yr_pub[i]] + 1
}

# zapis autocytowan do pliku
# nazwa pliku do zapisu 
file_name_selfcit <- paste(author_id, "_selfcit.csv", sep ="")

# sciezka do pliku
file_path_selfcit <- paste("C:/Users/abuczek/Desktop/cytowania/", file_name_selfcit, sep ="")

# zapis ramki danych z cytowaniami do pliku 
write.csv(selfcit, file = file_path_selfcit, row.names = FALSE)