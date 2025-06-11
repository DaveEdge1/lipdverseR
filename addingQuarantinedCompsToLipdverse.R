library(lipdR)
library(tidyverse)
updateVocabWebsites()
Sys.sleep(2)
standardTables <- readRDS(url("https://lipdverse.org/lipdverse/standardTables.RDS"),"rb")
#chdf <- createDatabaseReference(CH)

#standardize terminology

good <- map_lgl(D,hasStandardizedVocabulary,standardTables,.progress = TRUE)

BAD <- D[!good]

CHS <- map(BAD,standardizeLipd,standardTables)

CHS <- map(CHS,fixPubAuthorList)

val <- map_lgl(CHS,validLipd)

future::plan(strategy = future::multisession,workers = 16)
D <- readLipd("~/Dropbox/lipdverse/database/",parallel = TRUE)
databaseRef <- createDatabaseReference(D)

walk(CHS,addLipdToDatabase,checkValid = FALSE)


ts <- extractTs(D) |> ts2tibble()


