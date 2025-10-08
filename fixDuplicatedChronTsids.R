library(lipdR)
library(tidyverse)

fixDuplicatedChronTsids <- function(L){
  cts <- extractTs(L,mode = "chron")
  if(length(cts) == 0){
    return(NA)
  }
  ctsids <- try(pullTsVariable(cts,"chronData_TSid"))
  if(is(ctsids,"try-error")){
    #no TSids at all
    ntsids <- map_chr(seq_along(cts),\(x) createTSid("cd"))
    cts <- pushTsVariable(cts,"chronData_TSid",ntsids,createNew = TRUE)
    L <- collapseTs(cts)
    return(L)
  }

  if(any(duplicated(ctsids))){
    ntsids <- map_chr(seq_along(ctsids),\(x) createTSid("cd"))
    ctsids <- paste0(ntsids,ctsids)
    cts <- pushTsVariable(cts,"chronData_TSid",ctsids)
    L <- collapseTs(cts)
  }else{
    L <- NA
  }
  return(L)
}

N <- map(D,fixDuplicatedChronTsids,.progress = TRUE)

tc <- which(!is.na(N))
ND <- N[tc]

walk(ND,addLipdToDatabase)

