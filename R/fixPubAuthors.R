fixPubAuthorList <- function(L){
  for(p in 1:length(L$pub)){
    if(!is.null(L$pub[[p]]$author)){
      if(is.character(L$pub[[p]]$author)){
        L$pub[[p]]$author <- list(name = L$pub[[p]]$author)
      }
    }
  }
  return(L)
}

fixGeoErrors <- function(L){
  if(!is.null(L$geo$sisalSiteId)){
    if(!is.numeric(L$geo$sisalSiteId)){
      L$geo$sisalSiteId <- as.numeric(L$geo$sisalSiteId)
    }
  }
  return(L)
}


addChronsBackIn <- function(L,C){
  L$chronData <- C
  return(L)
}
