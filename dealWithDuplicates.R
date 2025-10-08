#deal with duplicated TSids
whichDup <- which(duplicated(ts$paleoData_TSid))
dupTsids <- ts$paleoData_TSid[whichDup]

wd_dsns <- filter(ts,paleoData_TSid %in% dupTsids) |>
  select(dataSetName) |>
  unique() |> unlist()

test <- readLipd(paste0("~/Dropbox/lipdverse/database/",wd_dsns,".lpd"))

for(i in 1:length(dupTsids)){
  dsns <- ts$dataSetName[ts$paleoData_TSid == dupTsids[i]]

  if(length(dsns) == 2){
    L1 <- readLipd(file.path("~/Dropbox/lipdverse/database/",paste0(dsns[1],".lpd")),dont.load.ensemble = FALSE)
    TS1 <- as.lipdTs(L1)
    ts1 <- ts2tibble(TS1)
    L2 <- readLipd(file.path("~/Dropbox/lipdverse/database/",paste0(dsns[2],".lpd")),dont.load.ensemble = FALSE)
    TS2 <- as.lipdTs(L2)
    ts2 <- ts2tibble(TS2)

    #are there still duplicate TSids? We might have already fixed these files
    if(!any(ts1$paleoData_TSid %in% ts2$paleoData_TSid)){
      next
    }
    mrc1 <- unique(getMostRecentInCompilationsTs(TS1))
    mrc2 <- unique(getMostRecentInCompilationsTs(TS2))


    #if(FALSE){
    if(any(grepl("iso2k",mrc1)) & all(is.na(mrc2))){
      wd <- 2
      appendWhat <- "namDendro"

    }else if(any(grepl("iso2k",mrc2)) & all(is.na(mrc1))){
      wd <- 1
      appendWhat <- "namDendro"

    }else{

      message(glue("{L1$dataSetName} - dataset 1 compilations:"))
      message(unique(getMostRecentInCompilationsTs(TS1)))

      message(glue("{L2$dataSetName} - dataset 2 compilations:"))
      message(unique(getMostRecentInCompilationsTs(TS2)))

      #which TSids are duplicated?
      wd1 <- which(ts1$paleoData_TSid %in% ts2$paleoData_TSid)
      wd2 <- which(ts2$paleoData_TSid %in% ts1$paleoData_TSid)

      wd <- askUser("To which dataset should we append 'dup' to the TSid")
      appendWhat <- askUser("What should we append? Leave blank for 'dup'")
    }

    if(appendWhat == ""){
      appendWhat <- "namDendro"
    }

    if(as.numeric(wd) == 1){
      ts1$paleoData_TSid[wd1] <- paste0(ts1$paleoData_TSid[wd1],"_",appendWhat)
      NL1 <- purrr::transpose(ts1) %>%
        collapseTs(force = TRUE)
      NL1$savedEnsembles <- NULL
      writeLipd(NL1,path = "~/Dropbox/lipdverse/database/")
    }else if(as.numeric(wd) == 2){
      ts2$paleoData_TSid[wd2] <- paste0(ts2$paleoData_TSid[wd2],"_",appendWhat)
      NL2 <- purrr::transpose(ts2) %>%
        collapseTs(force = TRUE)
      NL2$savedEnsembles <- NULL
      writeLipd(NL2,path = "~/Dropbox/lipdverse/database/")
    }else{
      message("skipping")
    }
  }
}
