library(semver)

# Function to check for missing versions and exact increments of 1
check_sequential_increments <- function(versions) {
  n <- length(versions)
  if (n <= 1) {
    return(list(is_sequential = TRUE, missing_versions = NULL))
  }

  missing_versions <- list()
  probable_missing_version <- c()
  for (i in 1:(n - 1)) {
    current_version <- versions[[i]]
    next_version <- versions[[i + 1]]

    # Render the versions to extract major, minor, and patch numbers
    current_parts <- render_version(current_version)
    next_parts <- render_version(next_version)

    # Check for increments
    if (current_parts$major == next_parts$major &&
        current_parts$minor == next_parts$minor &&
        current_parts$patch + 1 == next_parts$patch) {
      # Valid patch increment
      next
    } else if (current_parts$major == next_parts$major &&
               current_parts$minor + 1 == next_parts$minor &&
               next_parts$patch == 0) {
      # Valid minor increment
      next
    } else if (current_parts$major + 1 == next_parts$major &&
               next_parts$minor == 0 &&
               next_parts$patch == 0) {
      # Valid major increment
      next
    } else {
      # Missing version or invalid increment
      missing_versions <- c(missing_versions, paste("Expected version after", current_version, "is missing or increment is incorrect"))
      probable_missing_version <- c(probable_missing_version,semver::increment_version(current_version, "patch",1L))
    }
  }

  return(list(is_sequential = length(missing_versions) == 0, missing_versions = missing_versions,probable_missing_version = probable_missing_version))
}

# look through lipdverse data folder and fill in missing versions with following ones
dataDir <- "~/Dropbox/lipdverse/html/data/"

# get all files in dataDir
files <- list.files(dataDir, full.names = TRUE)
copiedCount <- 0
copiedList <- c()
#setup progress bar
pb <- txtProgressBar(min = 0, max = length(files), style = 3)

for(f in files){
  #update progress bar
  setTxtProgressBar(pb, which(files == f))
  #is it a directory?
  if(file.info(f)$isdir){
    #find the files that match the pattern number underscore number underscore number
    # and convert them to numeric versions
    # e.g. 1_0_0 becomes 1.0.0
    # get the version numbers from the directory names
    versionPattern <- "^([0-9]+)_([0-9]+)_([0-9]+)$"
    ad <- list.files(f, pattern = versionPattern, full.names = FALSE) |>
      stringr::str_replace_all("_",".") |>
      as.numeric_version() |>
      sort() |>
      as.character()

    #test if the list of versions is sequential
    versions_semver <- semver::parse_version(ad)

    # Check the list of semantic versions
    result <- check_sequential_increments(versions_semver)

    while(!result$is_sequential){
      for(mv in 1:length(result$probable_missing_version)){
        tmv <- as.character(result$probable_missing_version[[mv]]) |>
          stringr::str_replace_all("\\.","_")
        #make sure that folder doesn't exist
        if(!dir.exists(file.path(f, tmv))){
          #copy the next version folder into that one.
          nextvers <- increment_version(result$probable_missing_version[[mv]],"patch",1L) |>
            as.character() |>
            stringr::str_replace_all("\\.","_")
          if(dir.exists(file.path(f, nextvers))){
            system(paste("cp -r", file.path(f, nextvers), file.path(f, tmv)))
            message(paste("Copied next version folder:", nextvers, "into", tmv, "in", f))
            copiedCount <- copiedCount + 1
            copiedList <- c(copiedList, paste("Copied next version folder:", nextvers, "into", tmv, "in", f))
          }
        }
      }
      #check again
      ad <- list.files(f, pattern = versionPattern, full.names = FALSE) |>
        stringr::str_replace_all("_",".") |>
        as.numeric_version() |>
        sort() |>
        as.character()

      #test if the list of versions is sequential
      versions_semver <- semver::parse_version(ad)

      # Check the list of semantic versions
      result <- check_sequential_increments(versions_semver)
    }
  }
}
close(pb)



# Now look for missing html files -----------------------------------------
# look through lipdverse data folder and fill in missing versions with following ones
dataDir <- "~/Dropbox/lipdverse/html/data/"

# get all files in dataDir
files <- list.files(dataDir, full.names = TRUE)
copiedCount <- 0
copiedList <- c()
missingHtmlsInFinalVersion <- c()
#setup progress bar
pb <- txtProgressBar(min = 0, max = length(files), style = 3)

for(f in files){
  #update progress bar
  setTxtProgressBar(pb, which(files == f))
  #is it a directory?
  if(file.info(f)$isdir){
    #find the files that match the pattern number underscore number underscore number
    # and convert them to numeric versions
    # e.g. 1_0_0 becomes 1.0.0
    # get the version numbers from the directory names
    versionPattern <- "^([0-9]+)_([0-9]+)_([0-9]+)$"

    ado <- list.files(f, pattern = versionPattern, full.names = FALSE) |>
      stringr::str_replace_all("_",".") |>
      as.numeric_version() |>
      sort() |>
      as.character() |>
      stringr::str_replace_all("_","\\.")

    ad <- list.files(f, pattern = versionPattern, full.names = TRUE)

    #sort ad to match order in ado
    ad <- ad[order(ado)]


    for(tadi in seq_along(ad)){
      tad <- ad[tadi]
      htmlFiles <- list.files(tad, pattern = ".html$", full.names = TRUE)


      htmlsToCheck <- c('index.html', 'paleoPlots.html','chronPlots.html', 'changelog.html','sidebar.html')
      #see if missing htmls exists
      for(h in htmlsToCheck){
        if(!any(grepl(h, htmlFiles))){
          #is it the last one in ad?
          if(tadi == length(ad)){
            if(h %in%  c('index.html', 'changelog.html','sidebar.html')){
              missingHtmlsInFinalVersion <- c(missingHtmlsInFinalVersion, f)
            }
          } else {
            #if the file exists in the next version
            if(file.exists(file.path(ad[tadi+1], h))){
              #copy in index.html from the next version
              system(paste("cp", file.path(ad[tadi+1], h), file.path(tad, h)))
              #print a message
              message(paste("Copied",h,"from", ad[tadi+1], "to \n", ad[tadi]))
            }
          }
        }
      }

    }

  }
}
close(pb)
