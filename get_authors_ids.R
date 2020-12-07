#############################################
## PROGRAM POBIERA DANE DOTYCZACE AUTORÓW  ##
## PUBLIKACJI WYDANYCH W 2019 ROKU W:      ##
## PRE, APPA, NATURE ORAZ PNAS             ##  
## I ZWRACA ICH ID W PLIKU CSV             ## 
#############################################

rm(list=ls())
library(rscopus)

#wczytanie i sprawdzenie klucza API
Sys.getenv('Elsevier_API')
have_api_key()

#kwerendy
#query <- "( PUBYEAR  =  2019 )  AND  ( ISSN ( 2470-0045 )  OR  ISSN ( 0587-4246 )  OR  ISSN ( 0028-0836 )  OR  ISSN ( 1091-6490 ) ) "
query_pre <- "( PUBYEAR  =  2019 )  AND  ( ISSN ( 2470-0045 ))"
query_appa <- "( PUBYEAR  =  2019 )  AND  ( ISSN ( 0587-4246 ))"
query_nature <- "( PUBYEAR  =  2019 )  AND  ( ISSN ( 0028-0836 ))"
query_pnas <- "( PUBYEAR  =  2019 )  AND  ( ISSN ( 1091-6490 ))"

#pobranie wszystkich dokumentow dla danych kwerend
docs_pre <- scopus_search(query = query_pre, view = "COMPLETE") 
docs_appa <- scopus_search(query = query_appa, view = "COMPLETE") 
docs_nature <- scopus_search(query = query_nature, view = "COMPLETE") 
docs_pnas <- scopus_search(query = query_pnas, view = "COMPLETE") 

#uzyskanie numerow id wszystkich autorow pobranych publikacji
ids_pre <- c()
for (i in 1:docs_pre$total_results)  #dla wszystkich pobranych artykulow
{
  
    if(as.integer(docs_pre$entries[[i]]$`author-count`$`@total`) == 0)
    {
      ids_pre <- append(ids_pre, 0)
    }
    else 
    {
      for (j in 1:(as.integer(docs_pre$entries[[i]]$`author-count`$`@total`))) #dla wszystkich autorow kazdego artykulu
      {
      ids_pre <- append(ids_pre, docs_pre$entries[[i]]$author[[j]]$authid)
      }
    }
}

ids_appa <- c()
for (i in 1:docs_appa$total_results)  #dla wszystkich pobranych artykulow
{
  
    if(as.integer(docs_appa$entries[[i]]$`author-count`$`@total`) == 0)
    {
      ids_appa <- append(ids_appa, 0)
    }
    else 
    {
      for (j in 1:(as.integer(docs_appa$entries[[i]]$`author-count`$`@total`))) #dla wszystkich autorow kazdego artykulu
      {
      ids_appa <- append(ids_appa, docs_appa$entries[[i]]$author[[j]]$authid)
      }
    }
}

ids_nature <- c()
for (i in 1:docs_nature$total_results)  #dla wszystkich pobranych artykulow
{
  print("i: ")
  print(i)
    if(as.integer(docs_nature$entries[[i]]$`author-count`$`@total`) == 0)
    {
      ids_nature <- append(ids_nature,0)
    }
    else 
    {
      for (j in 1:(as.integer(docs_nature$entries[[i]]$`author-count`$`@total`))) #dla wszystkich autorow kazdego artykulu
      {
        print("j: ")
        print(j)
        if (j > 100){
          break
        }
        else
        {
          ids_nature <- append(ids_nature, docs_nature$entries[[i]]$author[[j]]$authid)
        }
      }
    }
  
}
#wystapil problem z 151 publikacja - wiecej niz 100 autorow - zebrane id stu pierwszych autorow publikacji

ids_pnas <- c()
for (i in 1:docs_pnas$total_results)  #dla wszystkich pobranych artykulow
{
  
  if(as.integer(docs_pnas$entries[[i]]$`author-count`$`@total`) == 0)
  {
    ids_pnas <- append(ids_pnas,0)
  }
  else 
  {
    for (j in 1:(as.integer(docs_pnas$entries[[i]]$`author-count`$`@total`))) #dla wszystkich autorow kazdego artykulu
    {
      ids_pnas <- append(ids_pnas, docs_pnas$entries[[i]]$author[[j]]$authid)
    }
  }
}

#zapisywanie numerow id do pliku csv
write.csv(ids_pre, file = "C:/Users/abuczek/Desktop/authors_ids_pre.csv", row.names = FALSE)
write.csv(ids_appa, file = "C:/Users/abuczek/Desktop/authors_ids_appa.csv", row.names = FALSE)
write.csv(ids_nature, file = "C:/Users/abuczek/Desktop/authors_ids_nature.csv", row.names = FALSE)
write.csv(ids_pnas, file = "C:/Users/abuczek/Desktop/authors_ids_pnas.csv", row.names = FALSE)

#polaczenie wektorow w jeden
ids <- c(ids_pre, ids_appa, ids_nature, ids_pnas)
ids <- unique(ids)
write.csv(ids, file = "C:/Users/abuczek/Desktop/authors_ids.csv", row.names = FALSE)