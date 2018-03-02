library(stringi)
library(stringr)
library(tidyr)
library(rvest)
library(SnowballC)
library(ggmap)

### Szükséges adatok beolvasása ###

a <- read.csv2("C:/Users/Szokolics_Daniel/Desktop/projects/ado1/171126_234458/egyszazalek.csv", sep = ";", fileEncoding = "utf-8",encoding = "utf-8", header = FALSE)
b <- read.csv2("C:/Users/Szokolics_Daniel/Desktop/projects/ado1/171127_012911/egyszazalek.csv", sep = ";", fileEncoding = "utf-8", encoding = "utf-8", header = FALSE)
c <- read.csv2("C:/Users/Szokolics_Daniel/Desktop/projects/ado1/180104_030331/egyszazalek.csv", sep = ";", fileEncoding = "utf-8", encoding = "utf-8", header = FALSE)
egyszazalek <- do.call("rbind", list(a, b, c))
egyszazalek$V8 <- NULL
names(egyszazalek) <- c("ID", "Adószám", "Év", "Adószám2", "Cím", "Felajánlók száma", "Összeg")

mappak <- c("171126_234458", "171127_012911", "180104_030331")
beolvasandok <- c("ngo_alap", "ngo_alap2", "ngos")
mappak <- paste("C:/Users/Szokolics_Daniel/Desktop/projects/ado1/", mappak, sep = "")

beolvlist <- list()
for (b in 1 : length(beolvasandok)) {
  nev <- paste(mappak, "/", beolvasandok[b], ".csv", sep = "")
    for (m in 1 : length(mappak)) {
      beolvlist[[m]] <- file(nev[m], encoding = "utf-8", open = "r")
      beolvlist[[m]] <- as.data.frame(readLines(beolvlist[[m]]))
      names(beolvlist[[m]]) <- "singlecol"
    }
  assign(beolvasandok[[b]], do.call("rbind", beolvlist))
  assign(beolvasandok[[b]], separate(beolvlist[[m]], singlecol, 
                                     into = c("ID", "Adószám", "Oszlopnév", "Érték"),
                        sep = ";", remove = TRUE, extra = "merge"))
}

ngo_alap[] <- lapply(ngo_alap, gsub, pattern=';', replacement='')
ngo_alap2[] <- lapply(ngo_alap2, gsub, pattern=';', replacement='')
ngos[] <- lapply(ngos, gsub, pattern=';', replacement='')

### Elemzendő adatok előállítása ###

a <- ngo_alap[ngo_alap$Oszlopnév %in% 
                       c("Cél szerinti besorolás", "Cél leírása"),]
b <- ngo_alap2[ngo_alap2$Oszlopnév %in% 
                         c("Tevékenység(ek) konkrét megnevezése"),]
c <- ngos[ngos$Oszlopnév %in% 
               c("Szervezet neve", "Cél szerinti besorolás", "Cél leírása"),]

work <- do.call("rbind", list(a, b, c))
work$ID <- NULL
work <- work[order(work$Adószám, work$Oszlopnév),]
work <- work[!duplicated(work),]

### Ezt fogja kiváltani Bence kódja ###
a <- work[work$Oszlopnév == "Cél szerinti besorolás",]
a$Oszlopnév <- NULL
names(a) <- c("Adószám", "Cél szerinti besorolás")
b <- work[work$Oszlopnév == "Szervezet neve",]
b$Oszlopnév <- NULL
names(b) <- c("Adószám", "Szervezet neve")
c <- work[work$Oszlopnév == "Cél leírása",]
c$Oszlopnév <- NULL
names(c) <- c("Adószám", "Cél leírása")
d <- work[work$Oszlopnév == "Tevékenység(ek) konkrét megnevezése",]
d$Oszlopnév <- NULL
names(d) <- c("Adószám", "Tevékenységek megnevezése")

work <- merge(b, a, by = 'Adószám', all.x = TRUE)
work <- merge(work, c, by = 'Adószám', all.x = TRUE)
work <- merge(work, d, by = 'Adószám', all.x = TRUE)
work$'Cél leírása'[is.na(work$'Cél leírása')] <- ""
work$`Tevékenységek megnevezése`[is.na(work$`Tevékenységek megnevezése`) == TRUE] <- ""
work$'Leírás' <- paste(work$`Cél leírása`, " ", work$`Tevékenységek megnevezése`)
work$`Cél leírása` <- NULL
work$`Tevékenységek megnevezése` <- NULL

rm(a, b, c, d)

### Kész a besorolásokhoz szükséges dataset ###

wordlist_group <- strsplit(gsub("[^[:alpha:][:space:]őű]","",tolower(as.character(work$`Leírás`))),"// |//-| ")
for(i in 1 : length(wordlist_group)){
  wordlist_group[[i]] <- unique(wordlist_group[[i]])
}
wordlist <- unlist(wordlist_group)
wlt <- as.data.frame(table(wordlist))
wlt <- wlt[wlt$Freq > 5,]
kotoszavak <- read.csv("C:/Users/Szokolics_Daniel/Desktop/projects/ado1/kotoszo.csv", sep = ";", encoding = "iso-8859-2", header = FALSE)
wlt <- wlt[!is.element(wlt$wordlist, kotoszavak[,1]) & !nchar(as.character(wlt$wordlist))<3,]
rm(kotoszavak, i, wordlist, wordlist_group)

### Kiszedem a felesleges szavakat ###

findWordType <- function(inputvector, nr){
  wordtype <- list()
  wlist <- list()
  for(i in 1 : nr){
    print(i)
    url <- paste("https://wikiszotar.hu/ertelmezo-szotar/", 
                 as.character(inputvector[[i]]), sep = "")
    webpage <- try(read_html(url, encoding = "UTF-8"))
    if(isTRUE(class(webpage)=="try-error")){ 
      aa <- wordStem(inputvector[[i]], "hungarian")
      url <- paste("https://wikiszotar.hu/ertelmezo-szotar/", aa, sep="")
      webpage <- try(read_html(url, encoding = "UTF-8"))
      if(isTRUE(class(webpage)=="try-error")){
        wordtype[i] <- ""
      }
      else{
        dta <- as(html_node(webpage,'.mw-headline'), "character")
        dta <- substring(dta,  
                         gregexpr(pattern ="\\(",dta)[[1]]+1,  
                         gregexpr(pattern ="\\)",dta)[[1]]-1)
        wordtype[i] <- dta
      }
    }
    else{
      dta <- as(html_node(webpage,'.mw-headline'), "character")
      dta <- substring(dta,  
                       gregexpr(pattern ="\\(",dta)[[1]]+1,  
                       gregexpr(pattern ="\\)",dta)[[1]]-1)
      wordtype[i] <- dta
    }
  }
  return(wordtype)
}

system.time(wordtype <- findWordType(wlt$wordlist, nrow(wlt)))

wlt$wordtype <- wordtype
wlt <- wlt[wlt$wordtype %in% c("főnév", "ige", "melléknév", "főnévi igenév", "tulajdonnév", "melléknév I.","", "főnév I.", "főnév, tulajdonnév"),]

### Itt már kész van a lista ###

typetable <- as.data.frame(table(work$`Cél szerinti besorolás`))
names(typetable) <- c("type","freq")
typetable <- typetable[order(-typetable$freq),]
typetouse <- typetable$type[typetable$freq>25 & typetable$type != ""]
typenotneeded <- typetable$type[typetable$freq < 25]
work$`Cél szerinti besorolás`[work$`Cél szerinti besorolás` %in% typenotneeded] <- "Egyéb"
typetouse <- factor(typetouse)

train <- work[is.element(work$`Cél szerinti besorolás`, typetouse) & nchar(work$Leírás) > 5 & is.na(work$Leírás) == FALSE,]
a <- as.data.frame(table(train$'Cél szerinti besorolás'))
names(a) <- c("a", "Freq")

tmptable <- list()
k=1
for(i in typetouse){
  tmp <- work[work$`Cél szerinti besorolás`==i,]
  tmplist <- strsplit(gsub("[^[:alnum:][:space:]őű]","",tolower(as.character(tmp$`Leírás`))),"// |//-| ")
  tmplist <- unlist(tmplist)
  tmplist <- tmplist[is.element(tmplist,wlt$wordlist)]
  tmptable[[k]] <- as.data.frame(table(tmplist))
  tmptable[[k]] <- tmptable[[k]][order(-tmptable[[k]]$Freq),]
#  tmptable[[k]] <- tmptable[[k]][tmptable[[k]]$Freq > 5,]
  tmptable[[k]]$percent <- tmptable[[k]]$Freq / a$Freq[a$a == i]
#  tmptable[[k]]$percent[tmptable[[k]]$percent < 0.03] <- 0
  k=k+1
}

scores <- as.data.frame(wlt$wordlist)
names(scores) <- "word"
for(i in 1 : 7){
  scores <- merge(scores,tmptable[[i]][,c("tmplist", "percent")], by.x = "word", by.y = "tmplist", all = TRUE)
}
colnames(scores)[2:8] <- as.vector(typetouse)
scores[is.na(scores)] <- 0

rm(tmptable, tmp, i, k, tmplist, typenotneeded, a)

mx <- matrix(nrow = nrow(scores), ncol = 7)
for(i in 1 : nrow(scores)){
  for(j in 1 : 7){
    mx[i,j] <- sign(scores[[i,j+1]]-sum(scores[i, 2:8])/7) * 
      (abs(scores[[i,j+1]]-sum(scores[i, 2:8])/7) ** 1.5) * 100 / sum(scores[i,2:8])
  }
}

scores[2:8] <- mx
scores <- as.data.frame(scores)

rm(mx, i, j)

pred <- work
pred <- pred[pred$`Cél szerinti besorolás`=="" & pred$`Leírás`>5,]

a <- matrix(nrow = nrow(pred), ncol = 7)
for(n in 1 : nrow(pred)){
  tempvr <- strsplit(gsub("[^[:alnum:][:space:]őű]","",tolower(as.character(pred$`Leírás`[n]))),"// |//-| ")
  tempvr <- tempvr[[1]]
  tempmx <- scores[is.element(scores$word, tempvr),]
  if(nrow(tempmx)>0){
    for(t in 1: length(typetouse)){
      a[n,t] <- sum(tempmx[t+1])
    }
  }
}
pred[5 : 11] <- a

colnames(pred)[5:11] <- as.vector(typetouse)
pred$`Cél szerinti besorolás` <- colnames(pred[5:11])[max.col(pred[5:11],ties.method="first")]
a <- work[nchar(work$`Cél szerinti besorolás`) > 2,]

results <- rbind(pred[1:3], a[1:3])

todo <- work[nchar(work$Leírás) < 5 & nchar(work$`Cél szerinti besorolás`) < 3,]
write.csv(todo, file = "C:/Users/Szokolics_Daniel/Desktop/projects/ado1/todo.csv", sep = ";", fileEncoding = "utf-8")

# kézi javítás
kezzel <- read.csv("C:/Users/Szokolics_Daniel/Desktop/projects/ado1/kezzel.csv", sep = ";", encoding = "utf-8")
kezzel <- kezzel[2:4]
names(kezzel) <- names(results)
results <- rbind(results, kezzel)

write.csv(results, file = "C:/Users/Szokolics_Daniel/Desktop/projects/ado1/celszerint.csv", sep = ";", fileEncoding = "utf-8")

rm(n, t, tempvr, a)

### GEOKÓDOLÁS ###

geo <- egyszazalek[2:7]
geo$Adószám2 <- NULL

cimkorrekt <- unique(geo[c("Adószám", "Év", "Cím")])

ezt <- c(" U. ", " U ", " KRT. ", " RKP. ", " RKP ", " SGT.", " SGT ")
erre <- c(" UTCA ", " UTCA ", " KÖRÚT ", " RAKPART ", " RAKPART ", " SUGÁRÚT ", " SUGÁRÚT ")
csere <- data.frame(ezt, erre)

for(i in 1 : nrow(csere)){
  cimkorrekt$Cím <- sub(csere$ezt[i], csere$erre[i], cimkorrekt$Cím)
}

cimkorrekt$Cím <- gsub("([^ ./()0123456789AÁBCDEÉFGHIÍJKLMNOÓÖŐPQRSTUÚÜŰVWXYZaábcdeéfghiíjklmnoóöőpqrstuúüűvwxyz-])", "#", cimkorrekt$Cím)
cimkorrekt$Cím <- gsub('####', "#", cimkorrekt$Cím)
cimkorrekt$Cím <- gsub('###', "#", cimkorrekt$Cím)
cimkorrekt$Cím <- gsub('##', "#", cimkorrekt$Cím)
cimkorrekt$Cím <- gsub('#', "#", cimkorrekt$Cím)

cimkorrekt$Cím[regexpr("([0-9]-[0-9])", cimkorrekt$Cím) > 0] <- substr(cimkorrekt$Cím, 1, regexpr("([0-9]#[0-9])", cimkorrekt$Cím))[regexpr("([0-9]#[0-9])", cimkorrekt$Cím) > 0]
cimkorrekt$Cím[regexpr("([0-9]#[0-9])", cimkorrekt$Cím) > 0] <- substr(cimkorrekt$Cím, 1, regexpr("([0-9]#[0-9])", cimkorrekt$Cím))[regexpr("([0-9]#[0-9])", cimkorrekt$Cím) > 0]

rowShift <- function(x, shiftLen = 1L) {
  r <- (1L + shiftLen):(length(x) + shiftLen)
  r[r<1] <- NA
  return(x[r])
}

for(i in 1:3){
  cimkorrekt$Cím[cimkorrekt$Adószám == rowShift(cimkorrekt$Adószám, -1) & grepl("#", cimkorrekt$Cím)] <- 
    rowShift(cimkorrekt$Cím, -1)[cimkorrekt$Adószám == rowShift(cimkorrekt$Adószám, -1) & grepl("#", cimkorrekt$Cím)]
}

#View(cimkorrekt[grepl("#", cimkorrekt$Cím),])
cimkorrekt$Cím2 <- str_replace_all(cimkorrekt$Cím, c("Á" = "A", "É" = "E", "Í" = "I", "Ó" = "O",
                                                     "Ö" = "O", "Ő" = "O", "Ú" = "U", "Ü" = "U",
                                                     "Ű" = "U",
                                                     "BUDAPEST I " = "BUDAPEST ",
                                                     "BUDAPEST II " = "BUDAPEST ",
                                                     "BUDAPEST III " = "BUDAPEST ",
                                                     "BUDAPEST IV " = "BUDAPEST ",
                                                     "BUDAPEST V " = "BUDAPEST ",
                                                     "BUDAPEST VI " = "BUDAPEST ",
                                                     "BUDAPEST VII " = "BUDAPEST ",
                                                     "BUDAPEST VIII " = "BUDAPEST ",
                                                     "BUDAPEST IX " = "BUDAPEST ",
                                                     "BUDAPEST X " = "BUDAPEST ",
                                                     "BUDAPEST XI " = "BUDAPEST ",
                                                     "BUDAPEST XII " = "BUDAPEST ",
                                                     "BUDAPEST XIII " = "BUDAPEST ",
                                                     "BUDAPEST XIV " = "BUDAPEST ",
                                                     "BUDAPEST XV " = "BUDAPEST ",
                                                     "BUDAPEST XVI " = "BUDAPEST ",
                                                     "BUDAPEST XVII " = "BUDAPEST ",
                                                     "BUDAPEST XVIII " = "BUDAPEST ",
                                                     "BUDAPEST XIX " = "BUDAPEST ",
                                                     "BUDAPEST XX " = "BUDAPEST ",
                                                     "BUDAPEST XXI " = "BUDAPEST ",
                                                     "BUDAPEST XXII " = "BUDAPEST ",
                                                     "BUDAPEST XXIII " = "BUDAPEST "))

cimkorrekt$Cím2 <- paste("Hungary, ", trimws(cimkorrekt$Cím2))

for(i in 1 : length(cimkorrekt$Cím2)){
  a <- gregexpr("UTCA [0-9]*", cimkorrekt$Cím2[i])
  b <- gregexpr("UT [0-9]*", cimkorrekt$Cím2[i])
  if(a[[1]][1] > 0){
    cimkorrekt$Cím2[i] <- substr(cimkorrekt$Cím2[i],
                                 1,
                                 attr(a[[1]], "match.length") + a[[1]][1] -1)
  }
  if(b[[1]][1] > 0){
    cimkorrekt$Cím2[i] <- substr(cimkorrekt$Cím2[i],
                                 1,
                                 attr(b[[1]], "match.length") + b[[1]][1] -1)
  }
}

cimkorrekt$Cím[regexpr("U.[0-9]+", cimkorrekt$Cím) > 0] <-
  sub(" U.", " UTCA ", cimkorrekt$Cím[regexpr("U.[0-9]+", cimkorrekt$Cím) > 0])

cimek <- unique(cimkorrekt$Cím2)

lon <- ls()
lat <- ls()
#name <- ls()
for(i in 1 : length(cimek)){
  if(is.na(lat[i] == TRUE)){
    result <- geocode(cimek[i], output = "latlona")
    lon[i] <- as.numeric(result[1])
    lat[i] <- as.numeric(result[2])
  }
  print(i)
#  name[i] <- as.numeric(result[3])
}
geocodeQueryCheck()

rrr <- data.frame(cimek, lat, lon)
rrr$cimek <- sub(" SGT. ", " SUGÁRÚT ", rrr$cimek)
rrr$cimek <- sub(" SGT ", " SUGÁRÚT ", rrr$cimek)
rrr$cimek <- sub(" RKP. ", " RAKPART ", rrr$cimek)
rrr$cimek <- sub(" U.", " UTCA ", rrr$cimek)

a <- merge(cimkorrekt, rrr, by.x = "Cím2", by.y = "cimek")
jo <- a[is.na(a$lat) == FALSE,]
nemjo <- a[is.na(a$lat) == TRUE,]
hrsz <- nemjo[regexpr("HRSZ", nemjo$Cím2) > 0,]
nemjo <- nemjo[regexpr("HRSZ", nemjo$Cím2) < 0,]

nemjo$Cím2[regexpr("([0-9]\\.)", nemjo$Cím2) > 0] <-
  substr(nemjo$Cím2[regexpr("([0-9]\\.)", nemjo$Cím2) > 0], 
         1, 
         regexpr("([0-9]\\.)", nemjo$Cím2[regexpr("([0-9]\\.)", nemjo$Cím2) > 0]))

cimek2 <- unique(nemjo$Cím2)

lon2 <- ls()
lat2 <- ls()
#name <- ls()
for(i in 1 : length(cimek2)){
  if(is.na(lat2[i] == TRUE)){
    result <- geocode(cimek2[i], output = "latlona")
    lon2[i] <- as.numeric(result[1])
    lat2[i] <- as.numeric(result[2])
  }
  print(i)
  #  name[i] <- as.numeric(result[3])
}

rrr <- data.frame(cimek, lat, lon)
rrr <- merge(cimkorrekt, rrr, by.x = "Cím2", by.y = "cimek")
rrr <- rrr[,c("Adószám", "Év", "lat", "lon")]
geolocated <- merge(geo, rrr, by.x = c("Adószám", "Év"), by.y = c("Adószám", "Év"))

write.csv2(geolocated, file = 'C:/Users/Szokolics_Daniel/Desktop/projects/ado1/geokodolt.csv', col.names = TRUE)
