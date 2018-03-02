library(stringi)
library(tidyr)
library(dplyr)

# azt hiszem itt az ideje, hogy a sajat dolgunkat legalabb annyival megkonnyitsuk, hogy a base library valtozoban van
# nalam a folder structure ugy nez ki, hogy a fajlok 3 kulon mappaban vannak, ennek megfeleloen
# lesz most itt a szerkezet
basedir <- "C:/Users/user/Google Drive/Projekt/201710_Egyszazalek/Data/raw/"

setwd(basedir)
# szedjuk ossze az almappakat, ebben vannak a scraping futtatasok eredmenyei

dir_count = 0

for(curr_dir in dir()){
  print(curr_dir)
  if(dir_count == 0){  
    egyszazalek <- read.csv2(paste0(basedir,curr_dir,"/egyszazalek.csv"), sep = ";", fileEncoding = "utf-8",encoding = "utf-8", header = FALSE, stringsAsFactors = FALSE)
    ngo_alap <- read.csv(paste0(basedir,curr_dir,"/ngo_alap.csv"), sep = ";", fileEncoding = "utf-8", encoding = "ISO-8859-2", quote = "", header = FALSE, stringsAsFactors = FALSE)
    ngo_alap2 <- read.csv(paste0(basedir,curr_dir,"/ngo_alap2.csv"), sep = ";", fileEncoding = "utf-8", encoding = "ISO-8859-2", quote = "", header = FALSE, stringsAsFactors = FALSE)
    dir_count = 1

    egyszazalek$V1 <- paste0("0_",egyszazalek$V1)
    ngo_alap$V1 <- paste0("0_",ngo_alap$V1)
    ngo_alap2$V2 <- paste0("0_",ngo_alap2$V1)
  } else {
    egyszazalek_new <- read.csv2(paste0(basedir,curr_dir,"/egyszazalek.csv"), sep = ";", fileEncoding = "utf-8",encoding = "utf-8", header = FALSE, stringsAsFactors = FALSE)
    ngo_alap_new <- read.csv(paste0(basedir,curr_dir,"/ngo_alap.csv"), sep = ";", fileEncoding = "utf-8", encoding = "ISO-8859-2", quote = "", header = FALSE, stringsAsFactors = FALSE)
    ngo_alap2_new <- read.csv(paste0(basedir,curr_dir,"/ngo_alap2.csv"), sep = ";", fileEncoding = "utf-8", encoding = "ISO-8859-2", quote = "", header = FALSE, stringsAsFactors = FALSE)
    dir_count = dir_count + 1

    egyszazalek_new$V1 <- paste0(dir_count,"_",egyszazalek_new$V1)
    ngo_alap_new$V1 <- paste0(dir_count,"_",ngo_alap_new$V1)
    ngo_alap2_new$V2 <- paste0(dir_count,"_",ngo_alap2_new$V1)

    egyszazalek <- rbind(egyszazalek, egyszazalek_new)
    ngo_alap <- rbind(ngo_alap, ngo_alap_new)
    ngo_alap2 <- rbind(ngo_alap2, ngo_alap2_new)
  }
}

# tamogatasi adatok beolvasasa 
# sor szam, adoszam, ev, teljes adoszam, cim, tamogatok szama, tamogatas osszege

a <- read.csv2(paste0(basedir,"171127_012911/egyszazalek.csv"), sep = ";", fileEncoding = "utf-8", encoding = "utf-8", header = FALSE, stringsAsFactors = FALSE)
b <- read.csv2(paste0(basedir,"180104_030331/egyszazalek.csv"), sep = ";", fileEncoding = "utf-8", encoding = "utf-8", header = FALSE, stringsAsFactors = FALSE)
egyszazalek$V1 <- paste0("1_",egyszazalek$V1)
a$V1 <- paste0("2_",a$V1)
b$V1 <- paste0("3_",b$V1)
egyszazalek <- rbind(egyszazalek, a)
egyszazalek <- rbind(egyszazalek, b)
egyszazalek$V8 <- NULL

# leiro adatok beolvasasa, long format
# sor szam, adoszam, mezonev, ertek


a <- read.csv(paste0(basedir,"171127_012911/ngo_alap.csv"), sep = ";", fileEncoding = "utf-8", encoding = "ISO-8859-2", quote = "", header = FALSE, stringsAsFactors = FALSE)
b <- read.csv(paste0(basedir,"180104_030331/ngo_alap.csv"), sep = ";", fileEncoding = "utf-8", encoding = "ISO-8859-2", quote = "", header = FALSE, stringsAsFactors = FALSE)
ngo_alap$V1 <- paste0("1_",ngo_alap$V1)
a$V1 <- paste0("2_",a$V1)
b$V1 <- paste0("3_",b$V1)
ngo_alap <- rbind(ngo_alap, a)
ngo_alap <- rbind(ngo_alap, b)

# tovabbi leiro adatok beolvasa, long format
# sor szam, adoszam, mezonev, ertek

ngo_alap2 <- read.csv(paste0(basedir,"171126_234458/ngo_alap2.csv"), sep = ";", fileEncoding = "utf-8", encoding = "ISO-8859-2", quote = "", header = FALSE, stringsAsFactors = FALSE)
a <- read.csv(paste0(basedir,"171127_012911/ngo_alap2.csv"), sep = ";", fileEncoding = "utf-8", encoding = "ISO-8859-2", quote = "", header = FALSE, stringsAsFactors = FALSE)
b <- read.csv(paste0(basedir,"180104_030331/ngo_alap2.csv"), sep = ";", fileEncoding = "utf-8", encoding = "ISO-8859-2", quote = "", header = FALSE, stringsAsFactors = FALSE)
ngo_alap2$V1 <- paste0("1_",ngo_alap2$V1)
a$V1 <- paste0("2_",a$V1)
b$V1 <- paste0("3_",b$V1)
ngo_alap2 <- rbind(ngo_alap2, a)
ngo_alap2 <- rbind(ngo_alap2, b)

# tamogatasi es leiro adatok egyben
# id, adoszam, ???
# szerintem hagyjuk ki

# ngos <- read.csv(paste0(basedir,"171126_234458/ngos.csv"), sep = ";", fileEncoding = "utf-8", encoding = "ISO-8859-2", quote = "", header = FALSE, stringsAsFactors = FALSE)
# a <- read.csv(paste0(basedir,"171127_012911/ngos.csv"), sep = ";", fileEncoding = "utf-8", encoding = "utf-8", quote = "", header = FALSE, stringsAsFactors = FALSE)
# b <- read.csv(paste0(basedir,"180104_030331/ngos.csv"), sep = ";", fileEncoding = "utf-8", encoding = "utf-8", quote = "", header = FALSE, stringsAsFactors = FALSE)
# ngos$V1 <- paste0("1_",ngos$V1)
# a$V1 <- paste0("2_",a$V1)
# b$V1 <- paste0("3_",b$V1)
# ngos <- rbind(ngos, a)
# ngos <- rbind(ngos, b)

rm(a)
rm(b)

ngo_alap <- ngo_alap[ngo_alap$V3!="No records found." & is.na(ngo_alap$V2) == FALSE,]
ngo_alap_unp <- spread(ngo_alap, V3, V4)
names(ngo_alap_unp)

w_ngo_alap2 <- ngo_alap2[!duplicated(ngo_alap2[,1:3]),]
ngo_alap2_unp <- spread(w_ngo_alap2, V3, V4)
names(ngo_alap2_unp)

names(egyszazalek) <- c("n","ID","ev","telj_adoszam","cim","db","osszeg")
names(ngo_alap_unp)[2] <- "ID"
names(ngo_alap2_unp)[2] <- "ID"

ngos2 <- merge(ngo_alap_unp, ngo_alap2_unp, all.x = TRUE, by = "ID")
ngos_all <- merge(egyszazalek, ngos2, all.x = TRUE, by = "ID")

remove(ngo_alap, ngo_alap2, egyszazalek, ngo_alap2_unp, ngo_alap_unp, ngos2, w_ngo_alap2)

w <- ngos_all
w$osszeg <- gsub("Ft", "", w$osszeg)
w$osszeg <- gsub("\\s", "", w$osszeg)
w$osszeg <- as.numeric(w$osszeg)

sum_all <- w %>%
  group_by(ev) %>%
  summarise(sum=sum(osszeg), darab=sum(db))

work <- ngos2[c("Szervezet neve", "Cél szerinti besorolás", "Cél leírása", "Tevékenység(ek) konkrét megnevezése")]

wordlist_group <- strsplit(gsub("[^[:alnum:][:space:]ûõ]","",tolower(as.character(work$`Cél leírása`))),"\\ |\\-| ")
for(i in 1 : length(wordlist_group)){
  wordlist_group[[i]] <- unique(wordlist_group[[i]])
}
wordlist <- unlist(wordlist_group)
#wlt <- as.data.frame(table(wordlist))
#wlt <- wlt[wlt$Freq > 5,]
#kotoszavak <- read.csv("C:/Users/Szokolics_Daniel/Desktop/projects/ado1/kotoszo.csv", sep = ";", encoding = "iso-8859-2", header = FALSE)
#wlt <- wlt[!is.element(wlt$wordlist, kotoszavak[,1]) & !nchar(as.character(wlt$wordlist))<3,]
#rm(kotoszavak, i)
sp_wlt <- read.csv("C:/Users/Szokolics_Daniel/Desktop/projects/ado1/sp_wlt.csv", sep = ";", encoding = "iso-8859-2", header = TRUE)
wlt <- sp_wlt


### Itt jön a játék a szavakkal ###

wlt <- wlt[wlt$wordtype %in% c("fõnév", "ige", "melléknév", "fõnévi igenév", "tulajdonnév", "melléknév I.",""),]

### Itt már kész van a lista ###

typetable <- as.data.frame(table(work$`Cél szerinti besorolás`))
typetable <- typetable[typetable$Freq>0,]
names(typetable) <- c("type","freq")
typetable <- typetable[order(-typetable$freq),]
typetouse <- typetable$type[typetable$freq>25 & typetable$type != ""]
typetouse <- factor(typetouse)

work <- work[is.element(work$`Cél szerinti besorolás`, typetouse),]
a <- work$`Cél szerinti besorolás`[work$`Cél leírása` != ""]
a <- as.data.frame(table(a))
a <- a[a$Freq > 0,]

tmptable <- list()
k=1
for(i in typetouse){
  tmp <- work[work$`Cél szerinti besorolás`==i,]
  tmplist <- strsplit(gsub("[^[:alnum:][:space:]]","",tolower(as.character(tmp$`Cél leírása`))),"\\ |\\-| ")
  tmplist <- unlist(tmplist)
  tmplist <- tmplist[is.element(tmplist,wlt$wordlist)]
  tmptable[[k]] <- as.data.frame(table(tmplist))
  tmptable[[k]] <- tmptable[[k]][order(-tmptable[[k]]$Freq),]
#  tmptable[[k]] <- tmptable[[k]][tmptable[[k]]$Freq > 5,]
  tmptable[[k]]$percent <- tmptable[[k]]$Freq / a$Freq[a$a == i]
  tmptable[[k]]$percent[tmptable[[k]]$percent < 0.03] <- 0
  k=k+1
}

tmptab <- tmptable[[1]][,c("tmplist", "percent")]
for(i in 2 : length(typetouse) + 1){
  tmptab <- merge(tmptab,tmptable[[i]][,c("tmplist", "percent")], by = "tmplist", all = TRUE)
}
colnames(tmptab)[2:7] <- as.vector(typetouse)
tmptab[is.na(tmptab)] <- 0

mx <- matrix(nrow = nrow(tmptab), ncol = 6)
for(i in 1 : nrow(tmptab)){
  for(j in 1 : 6){
    mx[i,j] <- tmptab[[i,j+1]]**2 * 100 / sum(tmptab[i,2:7])
  }
}

tmptab[2:7] <- mx
tmptab <- as.data.frame(tmptab)

pred <- ngos2[c("Szervezet neve", "Cél szerinti besorolás", "Cél leírása", "Tevékenység(ek) konkrét megnevezése")]
pred <- pred[pred$`Cél szerinti besorolás`=="" & pred$`Cél leírása`!="",]

a <- matrix(nrow = nrow(pred), ncol = length(typetouse))
for(n in 1 : nrow(pred)){
  tempvr <- strsplit(gsub("[^[:alnum:][:space:]]","",tolower(as.character(pred$`Cél leírása`[n]))),"\\ |\\-| ")
  tempvr <- unique(tempvr[[1]])
  db <- length(tempvr)
  tempmx <- tmptab[is.element(tmptab$tmplist, tempvr),]
  if(nrow(tempmx)>0){
    for(t in 1: length(typetouse)){
      a[n,t] <- sum(tempmx[1:db, t + 1])
    }
  }
}
pred[5 : 11] <- a

    
colnames(pred)[5:10] <- as.vector(typetouse)


tempmx <- merge(tempvr, tmptable[[1]], by.y = 'tmplist', all.x = FALSE, all.y = FALSE)

tempmx <- tmptable[[1]][is.element(tmptable[[1]]$tmplist, tempvr),]

