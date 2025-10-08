#check to make sure all database files have current data pages

isDataPageCurrent <- function(L,webdir = "~/Dropbox/lipdverse/html/"){
  verspath <- file.path(webdir,"data",L$datasetId,stringr::str_replace_all(getVersion(L),"\\.","_"))

  htmlsToCheck <- c('index.html', 'paleoPlots.html','changelog.html','sidebar.html')
  lpdToCheck <- c('lipd.lpd',paste0(L$dataSetName,".lpd"))

  current <- FALSE

  #folder exist
  if(dir.exists(verspath)){#that's good
    #check for key files
    af <- list.files(verspath)
    if(all(htmlsToCheck %in% af) & all(lpdToCheck %in% af)){
      current <- TRUE
    }
  }

  return(current)
}
# To create new ones...
#walk(D[bad],quietly(createDataWebPage),.progress = TRUE)
hasPrimaryAgeColumn <- function(L){
  return(any(grepl(names(unlist(L)),pattern = "primaryAgeColumn")))
}


#hpac <- map_lgl(D,hasPrimaryAgeColumn,.progress = TRUE)
#check to make sure all database files have current data pages

isDataPageHighestVersion <- function(L,webdir = "~/Dropbox/lipdverse/html/"){
  af <- list.files(file.path(webdir, "data", L$datasetId), pattern = "^\\d+_\\d+_\\d+$") |>
    stringr::str_replace_all("_","\\.") |>
    as.numeric_version()

  lv <- as.numeric_version(getVersion(L))

  if(any(af > lv)){
    #message(glue("{L$dataSetName}-{L$datasetId}: There is a folder for version {max(af)}, but the LiPD file is {lv}"))
    td <- file.path(webdir, "data", L$datasetId,stringr::str_replace_all(as.character(max(af)),"\\.","_"))
    #unlink(td,recursive = TRUE)
    return(FALSE)
  }else{
    return(TRUE)
  }
}

