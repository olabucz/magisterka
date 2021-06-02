rm(list = ls())
library(reshape2) #do dcast
library(rscopus) #do abstract_retrieval

################################################################################################ FUNKCJE
#funkcja do tworzenia œcie¿ki do pliku: œcie¿ka do folderu + nazwa pliku RData (numer ID autora) + .RData
create_path <- function(folder_path, id){
  paste(folder_path, id, ".RData", sep="")
}


#funkcja do tworzenia ramki danych z cytowaniami:
citations <- function(){
  #id ka¿dego artyku³u danego autora
  articles_ids <- author$eid 
  
  #dla ka¿dego artyku³u zbierane s¹ lata, w których uzyska³ on cytowanie
  yrs_of_cit <- lapply(1:length(articles_ids), function (i){ #1. dla ka¿dego artyku³u
    y <- c()
    for (j in 1:length(ref[,i]$entries)) #2. dla ka¿dego artyku³u, który go cytuje
    {
      #3. zbierane s¹ lata, w których uzyska³ on cytowanie
      if(sum(ref[,i]$total_results) == 0)
      {
        y <- c(y, "0")
      }
      else
      {
        y <- c(y, substr(ref[,i]$entries[[j]]$`prism:coverDate`,1,4)) 
      } 
    }
    y
  })
  
  #dla ka¿dej publikacji, zliczenie, ile mia³a ona cytowañ w danym roku
  count_citations <- lapply(1:length(articles_ids), function (i){
    if (yrs_of_cit[[i]]== "0"){
      yr <- 0
    }
    else
    {
      yr <- table(yrs_of_cit[[i]])
    }
  })
  
  #utworzenie ramki z cytowaniami
  if(any(as.numeric(unlist(count_citations))) != 0)
  {
    cit <- dcast(melt(count_citations), L1~Var1)
    cit[is.na(cit)] <- 0
    if (colnames(cit)[ncol(cit)] == "NA")
    {
      cit <- cit[,-c(1,ncol(cit)), drop = FALSE]
    }else
    {
      cit <- cit[,-1, drop = FALSE]
    }
    return(cit)
  }
  
  else #gdy wystêpuje 1 publikacja, która jest 0x cytowana
  {
    return (c(0))
  }
}

#funkcja oblicza indeks Hirscha na podstawie ramki danych z cytowaniami: (mo¿na zastosowaæ te¿ do autocytowañ, czy cytowañ obcych)
calculate_h_index <- function(d_frame){
  if(is.null(d_frame)) #gdy np. funkcja citations nie zwróci³a nic
  {
    return ()
  }
  if(is.vector(d_frame) == TRUE) 
  {
    return (0)
  }
  else
  {
    df <- matrix(ncol=length(d_frame), nrow=nrow(d_frame)) #ile w sumie cytowan do kazdego roku miala dana publikacja
    for (i in 1:length(d_frame)){ #dla ka¿dej kolumny, czyli dla ka¿dego roku, bo musi przelecieæ przez ka¿dy rok, by zsumowaæ
      df[,i] <- rowSums(d_frame[1:i])
    }
    df <- data.frame(df)
    
    last_year <- df[,ncol(df)]
    sorted_last_year <- sort(last_year, decreasing = TRUE) #mo¿na przy okazji sprawdziæ wektor cytowañ aktualny, z tym sposobem, co wy¿ej
    
    if(max(sorted_last_year) == 0) return (0)
    h <- tail(which(sorted_last_year >= seq_along(sorted_last_year)), 1) 
    return(h)
  }
}

#funkcja zwraca wektor z numerami id wszystkich autorów danej publikacji
authors <- function(n_of_authors, eid, type, i, j, k){
  publication_authors <- c()
  if(!is.na(n_of_authors) & !is.na(eid) & !is.null(n_of_authors) & !is.null(eid))
  {
    if(as.numeric(n_of_authors) > 100) #limit - potrzebne abstract_retrieval
    {
      publ_authors <- abstract_retrieval(eid, identifier = "eid") #jednorazowa komenda z rscopus
      for(a in 1:length(publ_authors$content$`abstracts-retrieval-response`$authors$author))
      {
        if (!is.null(publ_authors$content$`abstracts-retrieval-response`$authors$author[[a]]$`@auid`))
        {
          publication_authors <- append(publication_authors, publ_authors$content$`abstracts-retrieval-response`$authors$author[[a]]$`@auid`)
        }
      }
    }  
    
    else #mniej autorów ni¿ 100
    {
      if(type == 0) #autorzy publikacji cytowanej
      {
        for(a in 1:length(art[,i]$entries[[1]]$author))
        {
          if (!is.null(art[,i]$entries[[1]]$author[[a]]$authid))
          {
            publication_authors <- append(publication_authors, art[,i]$entries[[1]]$author[[a]]$authid)
          }
        }
      }
      if(type == 1) #autorzy publikacji cytuj¹cej
      {
        for (k in 1:length(ref[,i]$entries[[j]]$author))
        {
          if (!is.null(ref[,i]$entries[[j]]$author[[k]]$authid))
          {
            publication_authors <- append(publication_authors, ref[,i]$entries[[j]]$author[[k]]$authid)
          }
        }
      }
    }
    publication_authors <- as.numeric(publication_authors)
  }
  return(publication_authors)
}


selfcitations <- function(id, cits) {
  #autocytowania w³asne autora
  no_pub_author <- c()
  yr_pub_author <- c()
  
  #autocytowania od wspó³autorów
  no_pub_coauthors <- c()
  yr_pub_coauthors <- c()
  
  for (i in 1:length(author$eid)) #dla kazdego artykulu danego autora
  {
    i_authors <- as.numeric(author$`author-count.@total`[i])
    i_eid <- author$eid[i]
    publication_authors <- c()
    publication_authors <- authors(i_authors, i_eid, 0, i, 0, 0) #0, i, null, null?
    
    for (j in 1:length(ref[,i]$entries)) #dla wszystkich artykulow, ktore cytuja dany artykul 
    {
      if((!is.null(ref[,i]$entries[[j]]$`author-count`$`@total`))) #jesli dla danej publikacji w ogole jest jakies cytowanie
      {
        if(as.integer(ref[,i]$entries[[j]]$`author-count`$`@total`) != 0) #jesli artykul j cytujacy dany artykul i nie ma autorow, to nie wchodz w petle
        {
          
          citing_authors <- c() 
          j_authors <- as.integer(ref[,i]$entries[[j]]$`author-count`$`@total`)
          j_eid <- ref[,i]$entries[[j]]$eid
          citing_authors <- authors(j_authors, j_eid, 1, i, j, k)
          
          if(any(citing_authors == id)) #autocytowania w³asne autora
          {
            no_pub_author <- append(no_pub_author, i)
            yr_pub_author <- append(yr_pub_author, substr(ref[,i]$entries[[j]]$`prism:coverDate`,1,4))
          }
          
          if(length(intersect(publication_authors[publication_authors != id], citing_authors)) != 0 & all(citing_authors != id)) #autocytowania od wspó³autorów
          {
            no_pub_coauthors <- append(no_pub_coauthors, i)
            yr_pub_coauthors <- append(yr_pub_coauthors, substr(ref[,i]$entries[[j]]$`prism:coverDate`,1,4))
          }
        }
      }
    }
  }
  
  if(length(no_pub_author) != length(yr_pub_author)){
    print("Blad w selfcitations from author")
  }
  
  if(length(no_pub_coauthors) != length(yr_pub_coauthors)){
    print("Blad w selfcitations from coauthors")
  }
  
  selfcit_from_author <- cits #by powstala ramka danych po takich samych wymiarach, co ze wszystkich cytowañ (inaczej byæ nie powinno)
  selfcit_from_coauthors <- cits
  if(!is.null(nrow(cits)) & !is.null(ncol(cits))) #jeœli jest to ramka danych, a nie sam wektor z zerem
  {
    selfcit_from_author[,] <- 0
    selfcit_from_coauthors[,] <- 0
    
    # ramka danych z autocytowaniami
    for(i in 1:length(no_pub_author)){
      selfcit_from_author[no_pub_author[i],yr_pub_author[i]] = selfcit_from_author[no_pub_author[i],yr_pub_author[i]] + 1
    }
    
    for(i in 1:length(no_pub_coauthors)){
      selfcit_from_coauthors[no_pub_coauthors[i],yr_pub_coauthors[i]] = selfcit_from_coauthors[no_pub_coauthors[i],yr_pub_coauthors[i]] + 1
    }
  }
  
  return(list(selfcit_from_author, selfcit_from_coauthors))
}


#funkcja do tworzenia ramki danych z cytowaniami obcymi:
outercitations <- function(cits, selfcits){
  if(!is.null(cits) & !is.null(selfcits[[1]]) & !is.null(selfcits[[2]]))
  {
    return(list(cits - selfcits[[1]], cits - selfcits[[1]] - selfcits[[2]]))
  }
}

#funkcja obliczaj¹ca liczbê cytowañ na podstawie ramki danych z podanymi cytowaniami
calculate_citations <- function(d_frame){
  if(is.null(d_frame)) #gdy np. funkcja citations nie zwróci³a nic
  {
    return ()
  }
  if(is.vector(d_frame) == TRUE) 
  {
    return (0)
  }
  else
  {
    return(sum(d_frame))
  }
}

################################################################################################
#wektory dla wszystkich autorów
h <- c() #indeks Hirscha
h_selfcit <- c() #indeks Hirscha z autocytowañ od autora
h_selfcit_from_others <- c() #indeks Hirscha z autocytowañ od wspó³autorów
h_outercit <- c() #indeks Hirscha z cytowañ obcych 1 (wszystkie cytowania - autocytowania od autora)
h_outercit2 <- c() #indeks Hirscha z cytowañ obcych 2 (wszystkie cytowania - autocytowania od autora - autocytowania od wspó³autorów)
N <- c() #liczba publikacji 
M <- c() #liczba wszystkich cytowañ
M_selfcit <- c() #liczba autocytowañ od autora
M_selfcit_from_others <- c() #liczba autocytowañ od wspó³autorów
M_outercit <- c() #liczba cytowañ obcych 1 rodzaju (wszystkie - autocytowania od danego autora)
M_outercit2 <- c() #liczba cytowañ obcych 2 rodzaju (wszystkie - od autora - od wspó³autorów)

#wczytanie numerów id autorów
ids <- read.csv(file = "C:/Users/abuczek/Desktop/authors_ids_nature.csv")
ids <- unlist(ids)
ids <- as.vector(ids)
ids <- unique(ids)

sciezka <- "C:/Users/abuczek/Desktop/data/"

for (id in ids)
{
  print(id)
  path <- create_path(sciezka, id)
  if(file.exists(path)) #w przeciwnym razie funkcja nie zwraca nic
  {
    load(path) #za³adowanie pliku id.RData
    
    print("ogolne")
    cits <- citations() 
    N <- append(N, nrow(author))
    h <- append(h, calculate_h_index(cits))
    M <- append(M, calculate_citations(cits))
    
    print("auto")
    selfcits <- selfcitations(id, cits)
    h_selfcit <- append(h_selfcit, calculate_h_index(selfcits[[1]]))
    M_selfcit <- append(M_selfcit, calculate_citations(selfcits[[1]]))
    
    print("auto od innych")
    h_selfcit_from_others <- append(h_selfcit_from_others, calculate_h_index(selfcits[[2]]))
    M_selfcit_from_others <- append(M_selfcit_from_others, calculate_citations(selfcits[[2]]))
    
    print("obce")
    outercits <- outercitations(cits, selfcits)
    h_outercit <- append(h_outercit, calculate_h_index(outercits[[1]]))
    M_outercit <- append(M_outercit, calculate_citations(outercits[[1]]))
    
    print("obce2")
    h_outercit2 <- append(h_outercit2, calculate_h_index(outercits[[2]]))
    M_outercit2 <- append(M_outercit2, calculate_citations(outercits[[2]]))
  }
}
df  <- NULL
df <- data.frame(id = ids, indeks_h = h, M = M, N = N,
                 indeks_h_auto = h_selfcit, M_auto = M_selfcit,
                 indeks_h_auto_inni = h_selfcit_from_others, M_auto_inni = M_selfcit_from_others,
                 indeks_h_obce1 = h_outercit, M_obce1 = M_outercit,
                 indeks_h_obce2 = h_outercit2, M_obce2 = M_outercit2)
write.table(df, "C:/Users/abuczek/Desktop/nature_2019_hM.txt", row.names = FALSE, col.names = TRUE)
