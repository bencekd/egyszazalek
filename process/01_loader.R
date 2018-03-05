library(stringr)
library(tidyr)
library(dplyr)

### setting base directory

basedir <- "C:/Users/user/Google Drive/Projekt/201710_Egyszazalek/Data/raw/"
setwd(basedir)

### szukseges adatok beolvasasa ###

mappak <- dir()
beolvasandok <- c("ngo_alap", "ngo_alap2","egyszazalek")

### 3 strukturaju adatforrast olvasunk, ngo_alap, ngo_alap2 formaja megegyezik
### egyszazalek elter

### clean ; characters from files

for(b in beolvasandok){
  dir_count = 0
  for(m in mappak){
    raw_file <- file(paste0(basedir,m,"/",b,".csv"), encoding = "utf-8", open = "r")
    raw_data <- as.data.frame(readLines(raw_file), stringsAsFactors = FALSE)
    close(raw_file)
    names(raw_data) <- c("var")
    raw_data <- raw_data %>%
      mutate(var_corr = str_replace_all(var,"(?<=Cél leírása;).{1,};(?=.{1,};)","\\0___")) %>%
      transform(var_corr = str_replace_all(var_corr,";___",",")) %>%
      select(var_corr)
    if(dir_count == 0){
      raw_table <- raw_data
      dir_count = dir_count + 1
    }
    else{
      raw_table <- rbind(raw_table, raw_data)
    }
  }
  assign(paste0("raw_",b), raw_table)
  rm(raw_table, raw_data)
}

### TO-DO> dobjuk ki az azonos adoszamhoz tartozo halmozott megfigyeleseket
### en ekezet nelkuli billentyuvel dolgozom, pls hasznaljunk ilyen valtozoneveket

### ngo alap struktura
struct_ngo_alap <- c("ID", "adoszam", "oszlop", "ertek")

### egyszazalek struktura
struct_egyszazalek <- c("ID", "adoszam", "year", "adoszam2", "cim", "db", "osszeg")

ngo_alap <- separate(raw_ngo_alap, var_corr, into = struct_ngo_alap,
                        sep = ";", remove = TRUE, extra = "merge")

ngo_alap2 <- separate(raw_ngo_alap2, var_corr, into = struct_ngo_alap,
                        sep = ";", remove = TRUE, extra = "merge")

egyszazalek <- separate(raw_egyszazalek, var_corr, into = struct_egyszazalek,
                        sep = ";", remove = TRUE, extra = "merge")

rm(raw_ngo_alap, raw_ngo_alap2, raw_egyszazalek)