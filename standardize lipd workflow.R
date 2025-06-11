updateVocabWebsites()
Sys.sleep(2)
standardTables <- readRDS(url("https://lipdverse.org/lipdverse/standardTables.RDS"),"rb")
#G <- vector(mode = "list",length = length(BN))
for(b in b:length(BN)){
  L <- BN[[b]]

  Ln <- standardizeLipd(L,standardTables) |> fixPubAuthorList()

  if(!is.null(Ln$chronData[[1]]$measurementTable[[1]]$reject)){
    if(is.null(Ln$chronData[[1]]$measurementTable[[1]]$reject$TSid)){
      Ln$chronData[[1]]$measurementTable[[1]]$reject$TSid <- createTSid()
    }
  }

  if(validLipd(Ln)){
    #G[[b]] <- Ln
    addLipdToDatabase(Ln,checkValid = FALSE)
  }else{
    #beepr::beep(7)
    stop("not valid")
  }
  sum(!map_lgl(G,is.null))
}

b <- b + 1


#addLipdToDatabase(Ln,checkValid = FALSE)
