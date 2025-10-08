library(lipdR)
library(tidyverse)
updateVocabWebsites()
Sys.sleep(2)
standardTables <- readRDS(url("https://lipdverse.org/lipdverse/standardTables.RDS"),"rb")
#chdf <- createDatabaseReference(CH)

#standardize terminology
# for(i in seq_along(TREE)){
#   TREE[[i]]$datasetId <- paste0("NAm2kD",createDatasetId())
# }


all(map_lgl(BAD,validLipd))
good <- map_lgl(TREE,hasStandardizedVocabulary,standardTables,.progress = TRUE)

BAD <- TREE[!good]
# bts <- extractTs(BAD) |> ts2tibble()
# bts$paleoData_variableName[bts$paleoData_variableName == "count"] <- "sampleCount"
# bts$paleoData_units[bts$paleoData_units == "cores"] <- "count"
#
# BAD <- as.multiLipd(bts)

CHS <- map(BAD,standardizeLipd,standardTables)

goodNow <- map_lgl(CHS,hasStandardizedVocabulary,standardTables,.progress = TRUE)
val <- map_lgl(CHS,validLipd)
all(val)

all(goodNow)
CHS <- map(CHS,fixPubAuthorList)

nchs <- names(CHS)


N <- D[nchs]

for(i in nchs){
  N[[i]]$chronData <- CHS[[i]]$chronData
}

N <- map(N,fixPubAuthorList)
#missing chron TSids
for(i in nchs){
  if(!validLipd(N[[i]])){
    bts <- extractTs(N[[i]],mode = "chron")
    tsids <- pullTsVariable(bts,"chronData_TSid")
    ts <- which(is.na(tsids))
    for(t in ts){
    tsids[t] <- createTSid("rcn")
    }
    bts <- pushTsVariable(bts,"chronData_TSid",tsids)
    new <- collapseTs(bts)
    if(validLipd(new)){
      N[[i]] <- new
    }else{
      print("datasetName")
    }
  }
}
val <- map_lgl(N,validLipd)

#For THursday Morning, add these data to the RAW compilation! Then add to database
#The TSids or in the Table S1 csv

NTS <- extractTs(N)

D <- loadLipdverseDatabase()
databaseRef <- createDatabaseReference(D)

walk(CHS,addLipdToDatabase,checkValid = TRUE)


ts <- extractTs(D) |> ts2tibble()


for(n in names(CHS)){
  CHS[[n]]$pub <- O[[n]]$pub
}

#new names?

aa <- map_chr(O,\(x) {
  as <- humaniformat::last_name(getAuthor(x))
  if(as[[1]] != "Author"){
    return(as[[1]])
  }else{
    return(as[[2]])
  }})

aa[aa == "T."] <- "Harlan"

aa <- str_remove_all(aa,"[^A-Za-z0-9]")

sn <- map_chr(CHS,\(x) x$geo$siteName) |>
  str_remove_all("[^A-Za-z0-9]")

py1 <-  map(CHS,\(x){
  py <- x$pub[[1]]$year
  ifelse(is.null(py),NA,py)
  }) |> unlist()
py2 <-  map(CHS,\(x){
  if(length(x$pub) == 1){
  py <- x$pub[[1]]$Urldate
  }else{
    py <- x$pub[[2]]$Urldate
  }
  ifelse(is.null(py),NA,py)
}) |> unlist()

py <- py1

py[is.na(py1)] <- py2[is.na(py1)]

sum(is.na(py))
py

#these are the dsns to add to compilation.
# dsns <- paste(sn,aa,py,sep = ".")
# any(duplicated(dsns)) #oh no!
# for(i in 1:length(CHS)){
#   CHS[[i]]$dataSetName <- dsns[i]
# }

fixTRW <- function(L){
  tts <- extractTs(L) |> ts2tibble()
  tc <- which(tts$paleoData_variableName == "ringWidth")
  if(length(tc) > 0){
    tts$paleoData_units[tc] <- "unitless"
    if(is.null(tts$longName)){
      tts$longName <- rep(NA,times = nrow(tts))
    }
    tts$longName[tc] <- "tree ring standardized growth index"
    tts$paleoData_variableName[tc] <- "trsgi"
    Ln <- as.lipd(tts)
    return(Ln)
  }else{
    return(L)
  }
}

TFN <- map(TF,fixTRW,.progress = TRUE)

validLipd(TFN$`Arc-Avam-Taimyr.Briffa.2008`)
TFN <- map(TFN,fixPubAuthorList)

walk(TFN,addLipdToDatabase)
CHSo <- CHS

CHS <- map(CHS,fixTRW,.progress = TRUE)

NCHS <- CHS
for(i in 1:length(CHS)){
  L <- CHS[[i]]
  if(!is.character(L$geo$itrdbName)){
    next
  }
# L$geo$itrdbName <- on[i]
 #delete old bad meas tables
 for(p in 1:length(L$paleoData)){
   L$paleoData[[p]]$measurementTable <- NULL
 }

 ts <- as.lipdTsTibble(L)
 avn <- unique(ts$paleoData_variableName)


 if(grepl("cana",on[i])){
   country <- "canada"
 }else if(grepl("mexi",on[i])){
   country <- "canada"
 }else{
   country <- "usa"
 }

#figure out what paleo objects are what proxy

 trwP <- mxdP <- NA
 for(p in 1:length(L$paleoData)){
   an <- extract_by_key(L$paleoData[[p]]$model,"variableName") |> unique()
   if("ringWidth" %in% an & !"MXD" %in% an){
     trwP <- p
   }else if("MXD" %in% an & !"ringWidth" %in% an){
     mxdP <- p
   }else{
     stop("I don't think this should happen")
   }
 }


 if(!is.na(mxdP)){
   L$originalDataUrl <- glue("https://www.ncei.noaa.gov/pub/data/paleo/treering/measurements/northamerica/{country}/{on[i]}x.rwl")
   mxdData <- try(dplR::read.rwl(L$originalDataUrl,format = "tucson"))
   if(is(mxdData, "try-error")){
     mxdMt <- NA
   }else{
     mxdMt <- rwlToMeasTable(mxdData,on[i],mxd = TRUE)
   }
 }
 if(!is.na(trwP)){
   L$originalDataUrl <- glue("https://www.ncei.noaa.gov/pub/data/paleo/treering/measurements/northamerica/{country}/{on[i]}.rwl")
   rwlData <- try(dplR::read.rwl(L$originalDataUrl,format = "tucson"))
   if(is(rwlData, "try-error")){
     trwMt <- NA
   }else{
     trwMt <- rwlToMeasTable(rwlData,on[i],mxd = FALSE)
   }
 }

 if(!is.na(trwP)){#then there should be a measurementTable
   if(!all(is.na(trwMt))){#there is one!
     if(trwP == 1){#it's the first one!
       L$paleoData[[trwP]]$measurementTable[[1]] <- trwMt
     }else{
       stop("does this ever happen?")
     }
   }
 }

 if(!is.na(mxdP)){#then there should be a measurementTable
   if(!all(is.na(mxdMt))){#there is one!
     if(mxdP == 1){#it's the first one!
       L$paleoData[[mxdP]]$measurementTable[[1]] <- mxdMt
     }else if(mxdP == 2){
       L$paleoData[[mxdP]]$measurementTable[[1]] <- mxdMt
     }
   }
 }


 NCHS[[i]] <- L
}

rwlToMeasTable <- function(rwlData,itrdbName,mxd = FALSE){
  mt <- list()
  mt$year <- list(values = as.numeric(rownames(rwlData)),
                  TSid = createTSid(),
                  variableName = "year",
                  units = "yr AD")
  for(i in 1:ncol(rwlData)){
    if(mxd){
    mt[[paste0("MXD",i)]] <- list(values = as.numeric(rwlData[,i]),
                                        TSid = paste0(str_remove_all(string = paste0(itrdbName,names(rwlData)[i]),pattern = "[^A-Za-z0-9]"),createTSid()),
                                        variableName = "MXD",
                                        description = glue("Raw MXD measurements from series {names(rwlData)[i]}"),
                                        longName = glue("{names(rwlData)[i]} MXD"),
                                        units = "unitless")
    }else{
      mt[[paste0("ringWidth",i)]] <- list(values = as.numeric(rwlData[,i]),
                                          TSid = paste0(str_remove_all(string = paste0(itrdbName,names(rwlData)[i]),pattern = "[^A-Za-z0-9]"),createTSid()),
                                          variableName = "ringWidth",
                                          description = glue("Raw ring width measurements from series {names(rwlData)[i]}"),
                                          longName = glue("{names(rwlData)[i]} ring width"),
                                          units = "mm")
    }

  }

 return(mt)
}

fixTableNames <- function(L){
  ts <- as.lipdTsTibble(L)
  if(any(is.na(ts$paleoData_tableName))){
    tc <- which(is.na(ts$paleoData_tableName))
    ntn <- paste0("paleo",ts$paleoNumber,"measurement",ts$tableNumber)
    ts$paleoData_tableName[tc] <- ntn[tc]
    L <- as.lipd(ts)
  }
  return(L)
}

NCHS3 <- map(NCHS2,fixTableNames)

#should now be 6367 +

#HartsPassN1.Peterson.1994 (NAm2kD9n5saO5ZCWMQrmbryZLO)
