updateLipdverseLink <- function(L){
  vers <- getVersion(L)
  L$lipdverseLink <- paste0("https://lipdverse.org/data/",L$datasetId,"/",stringr::str_replace_all(as.character(vers),"[.]","_"))
  return(L)
}


#' Update the changelog entry in a LiPD file with new changes
#'
#' @param L
#' @param changelog
#' @param version
#' @param notes
#' @param curator
#' @param timestamp
#'
#' @return
#' @export
createNewChangelog <- function(L,
                               version = NA,
                               notes = NA,
                               curator = Sys.info()[["user"]],
                               timestamp = lubridate::now(tzone = "UTC")){

  if(!is.null(L$changelog)){#no changes, don't write
    stop("Changelog already exists, use updateChangelog()")
  }

  #get the comparison version
  lastVers <- "none"
  vers <- "0.0.0"

  changelist <- list("Starting new changelog")


  if(!is.na(notes)){#add in notes if present
    thisChange <- list(version = as.character(vers),
                       lastVersion = as.character(lastVers),
                       curator = curator,
                       timestamp = paste(timestamp,tz(timestamp)),
                       notes = notes,
                       changes =  changelist)
  }else{
    #create this instance of the changelog
    thisChange <- list(version = as.character(vers),
                       lastVersion = as.character(lastVers),
                       curator = curator,
                       timestamp = paste(timestamp,tz(timestamp)),
                       changes =  changelist)
  }

  #update the changes
  L$changelog <- list(thisChange)

  return(L)

}


#' Create html changelog details and summaries for the differences between two projects
#'
#' @param Dold
#' @param Dnew
#' @param proj
#' @param projVersOld
#' @param projVersNew
#' @param webDirectory
#'
#' @return
#' @export
createProjectChangelog <- function(Dold,
                                   Dnew,
                                   proj,
                                   projVersOld,
                                   projVersNew,
                                   webDirectory = "~/GitHub/lipdverse/html",
                                   notesTib = NA){

  #figure which are in each compilation
  #old
  TSo <- extractTs(Dold)
  wicO <- inThisCompilation(TSo,proj,projVersOld)
  dsnO <- pullTsVariable(TSo,"dataSetName",strict.search = TRUE)
  allNames <- unique(unlist(sapply(TSo,names)))
  if(!"datasetId"  %in% allNames){
    TSn <- extractTs(Dnew)
    wicN <- inThisCompilation(TSn,proj,projVersNew)
    dsnN <- pullTsVariable(TSn,"dataSetName",strict.search = TRUE)
    dsIdN <- pullTsVariable(TSn,"datasetId",strict.search = TRUE)
    mdfO <- tibble::tibble(dsn = dsnO)
    mdfN <- tibble::tibble(dsn = dsnN,dsid = dsIdN)
    mdf <- dplyr::left_join(mdfO,dplyr::distinct(mdfN),by = "dsn")
    dsIdO <- mdf$dsid
  }else{
    dsIdO <- pullTsVariable(TSo,"datasetId",strict.search = TRUE)
  }
  icOi <- which(purrr::map_lgl(wicO,isTRUE))
  dsIdIcO <- unique(dsIdO[icOi])
  dsnIcO <- unique(dsnO[icOi])
  dataVersOld <- map_chr(dsnIcO,~ getVersion(Dold[[.x]]))

  oT <- tibble::tibble(datasetId = dsIdIcO,
                       dataSetNameOld = dsnIcO,
                       versionOld = dataVersOld)

  #new
  TSn <- extractTs(Dnew)
  wicN <- inThisCompilation(TSn,proj,projVersNew)
  dsnN <- pullTsVariable(TSn,"dataSetName",strict.search = TRUE)
  dsIdN <- pullTsVariable(TSn,"datasetId",strict.search = TRUE)
  icNi <- which(purrr::map_lgl(wicN,isTRUE))
  dsIdIcN <- unique(dsIdN[icNi])
  dsnIcN <- unique(dsnN[icNi])
  dataVersNew <- map_chr(dsnIcN,~ getVersion(Dnew[[.x]]))

  nT <- tibble::tibble(datasetId = dsIdIcN,
                       dataSetNameNew = dsnIcN,
                       versionNew = dataVersNew)


  #combined tibble
  cT <- dplyr::full_join(oT,nT,by = "datasetId")

  #look for removed datasets
  rT <- cT %>%
    dplyr::filter(is.na(dataSetNameNew))

  nRemoved <- nrow(rT)

  #added
  aT <- cT %>%
    dplyr::filter(is.na(dataSetNameOld))

  nAdded <- nrow(aT)

  #go through and check out the differences
  cTg <- cT %>%
    dplyr::filter(!is.na(dataSetNameNew) & !is.na(dataSetNameOld)) %>% #only when both are in there
    dplyr::filter(versionOld != versionNew) #and teh versions are different

  if(!all(is.na(notesTib))){
    notesTib <- dplyr::select(notesTib,datasetId,notes = changes)
    cTg <- dplyr::left_join(cTg,notesTib,by = "datasetId")
  }else{
    cTg$notes <- NA
  }

  #setup big changelog
  bigCl <- tibble::tibble()

  Dchanged <- list()
  bigCl <- tibble::tibble() #for later
  if(nrow(cTg)>0){#then there are changes to record

    for(i in 1:nrow(cTg)){
      tdsid <- cTg$datasetId[i]
      # print(i)
      # print(cTg$dataSetNameOld[i])
      # print(cTg$dataSetNameNew[i])

      cl <- try(createChangelog(Dold[[cTg$dataSetNameOld[i]]],Dnew[[cTg$dataSetNameNew[i]]]))
      if(is(cl,"try-error")){
        changelog <- tibble::tibble(type = "Error", change = cl[1], variable = "Error",dataSetName = NULL,lastVersion = NULL)
        Dchanged[[cTg$dataSetNameNew[i]]] <- updateChangelog(Dnew[[cTg$dataSetNameNew[i]]],
                                                             changelog = changelog,
                                                             notes = cTg$notes[i],
                                                             version = paste0(cTg$versionNew[i],".1000"))#force it to have the same version (with a .1000) for this purpose
      }else{
        if(nrow(cl) > 0){
          bigCl <- dplyr::bind_rows(bigCl,cl)
          Dchanged[[cTg$dataSetNameNew[i]]] <- updateChangelog(Dnew[[cTg$dataSetNameNew[i]]],
                                                               changelog = cl,
                                                               notes = cTg$notes[i],
                                                               version = paste0(cTg$versionNew[i],".1000"))#force it to have the same version (with a .1000) for this purpose
        }
      }
    }
  }

  #create a list that has what's needed for the markdown summary
  mdList <- list(bigCl = bigCl,aT = aT,rT = rT,projVersOld = projVersOld, projVersNew = projVersNew, proj = proj)
  #save a file of data needed for the summary
  save(mdList,file = file.path(webDirectory,"markdownChangelogData.RData"))

  #create the page
  rmarkdown::render(input = file.path(webDirectory,"changelogSummarizer.Rmd"),
                    output_file = file.path(webDirectory,proj,projVersNew,"changelogSummary.html"))


  #create a detailed changelog
  if(nrow(cTg)>0){#then there are changes to record

    mdcl <- glue::glue("# **{proj}**: Detailed changes from version *{projVersOld}* to *{projVersNew}*") %>%
      str_c("\n\n") %>%
      str_c(glue::glue("#### **{date()}**")) %>%
      str_c("\n\n") %>%
      str_c("A summary of changes made to the project is listed [here](changelogSummary.html)") %>%
      str_c("\n\n")
    for(d in 1:length(Dchanged)){
      tcl <- getChangelog(Dchanged[[d]],version = "newest")
      mdcl <- mdcl %>%
        str_c(glue::glue("## {Dchanged[[d]]$dataSetName}")) %>%
        str_c("\n\n") %>%
        str_c(lipdR::createSingleMarkdownChangelog(tcl)) %>%
        str_c("\n\n")

      # if(is(tmdcl,"try-error")){
      #   mdcl <- mdcl %>%
      #                 glue::glue("## {Dchanged[[d]]$dataSetName}") %>%
      #                 str_c("\n\n") %>%
      #                 str_c("changelog includes special characters that are breaking createProjectChangelog. Skipping.")
      # }else{
      #   mdcl <- tmdcl
      #}
    }

    mdcl <- stringr::str_replace_all(mdcl,pattern = "''",replacement = "NULL")

  }else{

    mdcl <- glue::glue("## **{proj}** version *{projVersNew}* There do not appear to have been any changes to the datasets.")
  }

  write_file(mdcl,file.path(webDirectory,proj,projVersNew,"changelogDetail.Rmd"))

  rmarkdown::render(file.path(webDirectory,proj,projVersNew,"changelogDetail.Rmd"),output_file = file.path(webDirectory,proj,projVersNew,"changelogDetail.html"))
}


