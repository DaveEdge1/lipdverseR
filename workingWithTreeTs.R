
thisTree2Lipd <- function(thisTree){
  #for each row, add a row for each companion dataset
  for(r in 1:nrow(thisTree)){
    nr <- thisTree[r,]

    nry <- thisTree[r,]
    nry$paleoData_variableName <- "year"
    nry$paleoData_values <- nr$year
    nry$paleoData_TSid <- createTSid("namy")
    nry$paleoData_units <- nr$yearUnits

    nre <- thisTree[r,]
    nre$paleoData_variableName <- "EPS"
    nre$paleoData_values <- nr$eps
    nre$paleoData_TSid <- nr$`eps-TSid`
    nre$paleoData_units <- nr$`eps-units`

    nrc <- thisTree[r,]
    nrc$paleoData_variableName <- "sampleCount"
    nrc$paleoData_values <- nr$ncores
    nrc$paleoData_TSid <- nr$`ncores-TSid`
    nrc$paleoData_units <- "count"

    nrr <- thisTree[r,]
    nrr$paleoData_variableName <- "RBAR"
    nrr$paleoData_values <- nr$rbar
    nrr$paleoData_TSid <- nr$`rbar-TSid`
    nrr$paleoData_units <- nr$`rbar-units`

    if(r == 1){
      anr <- bind_rows(nry,nre,nrc,nrr)
    }else{
      anr <- bind_rows(anr,nry,nre,nrc,nrr)
    }

  }

  #remove reows that shouldn't be in anr
  anrt <- select(anr,-contains("_"),paleoData_variableName,paleoData_TSid, paleoData_units, paleoData_values, starts_with("geo_"), starts_with("pub_") )


  newTree <- bind_rows(anrt, thisTree) |>
    select(-starts_with("eps"),
           -starts_with("ncores"),
           -starts_with("rbar")) |>
    arrange(paleoNumber, modelNumber, tableNumber)

  L <- purrr::transpose(newTree) |>
    collapseTs(force = TRUE)

  L$geo$longitude <- as.numeric(L$geo$longitude)
  L$geo$latitude <- as.numeric(L$geo$latitude)
  L$geo$elevation <- as.numeric(L$geo$elevation)

  L <- fixPubAuthorList(L)
  L$datasetId <- paste0("NAm2kD",createDatasetId())

  if(!validLipd(L)){
    stop("not valid")
  }

  return(L)

}


allDsn <- unique(treeTStib$dataSetName)
for(a in allDsn){
  thisTree <- filter(treeTStib,dataSetName == a)
  L <- thisTree2Lipd(thisTree)
  writeLipd(L,"~/Dropbox/lipdverse/nam2ktrees/")
}
