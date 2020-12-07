rm(list=ls())
library(rscopus)

ids <- read.csv(file = "C:/Users/abuczek/Desktop/authors_ids.csv")
ids <- unlist(ids)
ids <- as.vector(ids)

length(ids)
which(ids %in% 23670620000)

ids[7313]
for(id in ids[7313:length(ids)]){
  #wczytanie i sprawdzenie klucza API
  Sys.getenv('Elsevier_API')
  have_api_key()
  
  #przypisanie id autora do zmiennej
  author_id <- toString(id)
  
  #pobranie danych o autorze na podstawie ID 
  author <- author_df(au_id = author_id,verbose = FALSE) 
  
  #ID wszystkich artykulow autora
  articles_ids <- author$eid
  
  #wstawienie ID artykulow w string (tworzenie kwerend)
  queries_art <- sapply(1:length(articles_ids), function (i){
    q <- paste("EID(", articles_ids[i],")",sep ="")
  }) 
  
  #pobranie informacji o wszystkich artykulach danego autora
  art <- sapply(1:length(articles_ids), function (j){ 
    a <- scopus_search(query = queries_art[j], view = "COMPLETE") 
  })
  
  #wstawienie ID artykulow w string (tworzenie kwerend)
  queries_ref <- sapply(1:length(articles_ids), function (i){
    q <- paste("REF(", articles_ids[i],")",sep ="")
  }) 
  
  #pobranie informacji o wszystkich artykulach cytujacych kazdy artykul danego autora
  ref <- sapply(1:length(articles_ids), function (j){ 
    r <- scopus_search(query = queries_ref[j], view = "COMPLETE", max_count = 20000) 
  })
  
  #zapis do pliku RData
  #nazwa pliku do zapisu - id autora
  file_name <- paste(author_id, ".RData", sep ="")
  
  #sciezka do pliku
  file_path <- paste("C:/Users/abuczek/Desktop/data/", file_name, sep ="")
  
  #zapis RData
  save(author, art, ref, file = file_path)
}