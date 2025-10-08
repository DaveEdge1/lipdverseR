#deal with duplicate datasetIds
dsid <- purrr::map_chr(D,"datasetId")
any(duplicated(dsid))
dupDsn <- dsn[which(duplicated(dsid))]
dsn <- purrr::map_chr(D,"dataSetName")
dupDsn <- dsn[which(duplicated(dsid))]
dupDsid <- dsid[which(duplicated(dsid))]

changed <- data.frame(dsn = "start", oldDsid = "start", newDsid = "start")

# ask user to append 'dup' to datasetId --------------------------------

for(i in dupDsid){
  
  dupDsn <- dsn[which(dsid == i)]

  NL1 <- D[[dupDsn[[1]]]]
  NL2 <- D[[dupDsn[[2]]]]

  print(NL1)
  Sys.sleep(3)
  print(NL2)

  wd <- askUser("To which dataset should we append 'dup' to the datasetId")
  appendWhat <- askUser("What should we append? Leave blank for 'iso'")

  if(appendWhat == ""){
    appendWhat <- "iso"
  }

  if(as.numeric(wd) == 1){
    NL1$datasetId <- paste0(NL1$datasetId,appendWhat)
    writeLipd(NL1,path = "~/Dropbox/lipdverse/database/")
    newRow <- data.frame(dsn = NL1$dataSetName, oldDsid = i, newDsid = NL1$datasetId)
    changed <- bind_rows(changed,newRow)
  }else if(as.numeric(wd) == 2){
    NL2$datasetId <- paste0(NL2$datasetId,appendWhat)
    writeLipd(NL2,path = "~/Dropbox/lipdverse/database/")
    newRow <- data.frame(dsn = NL2$dataSetName, oldDsid = i, newDsid = NL2$datasetId)
    changed <- bind_rows(changed,newRow)
  }else{
    message("skipping")
  }



}



for(i in changed$dsn){
  L <- readLipd(paste0("~/Dropbox/lipdverse/database/",i,".lpd"))
  D[[i]] <- L
}


# fix datasets in compilation for iso2k and pages2k -----------------------
ss <- "1OzbcpfnxRUDfYs3XJYaBXniFL5nVL8dW1rIrC_EFPpQ"

dsns <- read_sheet(ss,sheet = "datasetsInCompilation")

for(r in 1:nrow(dsns)){
  if(dsns$dsn[r] %in% changed$dsn){
    dsns$dsid[r] <- changed$newDsid[which(changed$dsn == dsns$dsn[r])]
    dsns$inComp[r] <- FALSE

    #print summary
    print(glue::glue("updated {dsns$dsn[r]} to {dsns$dsid[r]}"))
  }
}

test <- filter(dsns,endsWith(dsid,suffix = "iso"))

write_sheet(dsns,ss,sheet = "datasetsInCompilation")


afi <- list.files("~/Dropbox/lipdverse/iso2k/",pattern = ".lpd") |> str_remove(".lpd")

#work on pages2k
for(r in 1:nrow(dsns)){
  if(dsns$dsn[r] %in% afi){
    if(dsns$inComp[r] == "FALSE"){
    dsns$inComp[r] <- "TRUE"
    #print summary
    print(glue::glue("set {dsns$dsn[r]} to TRUE"))
    }
  }

 
}


#
# #work on pages2k
# for(r in 1:nrow(dsns)){
#   if(dsns$dsn[r] %in% olddsn){
#     dsns$inComp[r] <- TRUE
#     #print summary
#     print(glue::glue("set {dsns$dsn[r]} to TRUE"))
#   }
#
#   }
# }


