iso2kDsn <- unique(iso2k$dataSetName)

I <- D[iso2kDsn]
newI <- I
changed <- matrix(FALSE,nrow = length(I))
#correctYears <- function(L){
for(i in (i):length(I)){

  L <- I[[i]]

  ts <- as.lipdTsTibble(L)
  tsn <- ts$paleoData_variableName


  if(any(tsn == "year")){
    ai <- which(tsn == "year")
    for(a in ai){
      if(any(is.finite(ts$paleoData_values[[a]]))){
      if(min(dim(as.matrix(ts$paleoData_values[[a]]))) > 1){#probably an ensemble
        hut <- apply(ts$paleoData_values[[a]],2,heuristicUnits)
        hut <- hut[hut == "AD" | hut == "BP"]
        if(length(hut) == 0){
          stop("no AD or BP")
        }
        hu <- paste0("yr ",names(sort(table(hut), decreasing=TRUE)[1]))
      }else{
        hu <- paste0("yr ",geoChronR::heuristicUnits(ts$paleoData_values[[a]]))
      }
      if(hu == "yr AD" & ts$paleoData_units[[a]] == "yr AD"){#good
      }else if(hu == "yr BP" & ts$paleoData_units[[a]] == "yr BP"){#then we need to change the name
        message(glue("{L$dataSetName}: Changing the variableName to age"))
        ts$paleoData_variableName[[a]] <- "age"
        changed[i] <- TRUE
      }else if(hu == "yr BP" & ts$paleoData_units[[a]] == "yr AD"){#then we probably need to change the name and the units
        print(ts$paleoData_values[[a]])
        change <- askUser("These look like BP values, do you agree?")
        if(grepl("y",x = change, ignore.case = TRUE)){
          message(glue("{L$dataSetName}: Changing the units to yr BP and the name to age"))
          ts$paleoData_variableName[[a]] <- "age"
          ts$paleoData_units[[a]] <- "yr BP"
          changed[i] <- TRUE
        }else{
          stop()
        }
      }else if(hu == "yr AD" & ts$paleoData_units[[a]] == "yr BP"){#then we probably need to change the units
        ts$paleoData_units[[a]] <- "yr AD"
        changed[i] <- TRUE
        message(glue("{L$dataSetName}: Changing the units to yr AD"))

      }else{
        message(glue::glue("{L$dataSetName}: {ts$paleoData_variableName[[a]]} and {ts$paleoData_units[[a]]}"))
        plot(ts$paleoData_values[[a]])
        change <- askUser("This needs a look, is it ok?")
        if(grepl("y",x = change, ignore.case = TRUE)){
          next
        }else if(grepl("bp",x = change, ignore.case = TRUE)){
          message(glue("{L$dataSetName}:Converting the data to year AD"))
          ts$paleoData_values[[a]] <- convertBP2AD(ts$paleoData_values[[a]])
          changed[i] <- TRUE
        }else{
          stop("Something different")
        }
      }
      }
    }
  }

  #also check age
  if(any(tsn == "age")){
    ai <- which(tsn == "age")
    for(a in ai){
      if(any(is.finite(ts$paleoData_values[[a]]))){

      if(min(dim(as.matrix(ts$paleoData_values[[a]]))) > 1){#probably an ensemble
          hut <- apply(ts$paleoData_values[[a]],2,heuristicUnits)
          hut <- hut[hut == "AD" | hut == "BP"]
          if(length(hut) == 0){
            stop("no AD or BP")
          }
          hu <- paste0("yr ",names(sort(table(hut), decreasing=TRUE)[1]))
        }else{
          hu <- paste0("yr ",geoChronR::heuristicUnits(ts$paleoData_values[[a]]))
        }
      if(hu == "yr BP" & ts$paleoData_units[[a]] == "yr BP"){#good
      }else if(hu == "yr AD" & ts$paleoData_units[[a]] == "yr AD"){#then we need to change the name
        message(glue("{L$dataSetName}: Changing the variableName to year"))
        ts$paleoData_variableName[[a]] <- "year"
        changed[i] <- TRUE
      }else if(hu == "yr AD" & ts$paleoData_units[[a]] == "yr BP"){#then we probably need to change the name and the units
        print(ts$paleoData_values[[a]])
        change <- askUser("These look like AD values, do you agree?")
        if(grepl("y",x = change, ignore.case = TRUE)){
          ts$paleoData_variableName[[a]] <- "year"
          ts$paleoData_units[[a]] <- "yr AD"
          changed[i] <- TRUE
        }else{
          stop()
        }
      }else if(hu == "yr BP" & ts$paleoData_units[[a]] == "yr AD"){#then we probably need to change the units
        ts$paleoData_units[[a]] <- "yr BP"
        changed[i] <- TRUE
        message(glue("{L$dataSetName}: Changing the units to yr BP"))

      }else{
        message(glue::glue("{L$dataSetName}: {ts$paleoData_variableName[[a]]} and {ts$paleoData_units[[a]]}"))
        plot(ts$paleoData_values[[a]])
        change <- askUser("This needs a look, is it ok?")
        if(grepl("y",x = change, ignore.case = TRUE)){
          next
        }else{
          stop("Something different")
        }

      }
    }
    }
  }
  if(changed[i]){
    newI[[i]] <- as.lipd(ts)
  }
}

#}

#IN <- map(I,correctYears)
