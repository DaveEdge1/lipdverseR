
#find all the folders in the directory that end with "iso"
# and store them in a list called afiso
afiso <- list.files("~/Dropbox/lipdverse/html/data", pattern = "iso$", full.names = TRUE)

#loop through them, and recursively check for versions where the folder is nested inside a folder with the same name. In that case, move the inner folder to the top level, eliminating the duplications
for (folder in afiso) {
  #All versions
  av <- list.files(folder,full.names = TRUE)

  for(tv in av){
    if(isDirectory(tv)){

      # Get the name of the folder without the path
      folder_name <- basename(tv)


      # Find nested folders with the same name
      nested_folders <- list.files(tv, full.names = TRUE, recursive = FALSE)



      # If there are nested folders, move them to the top level
      if (length(nested_folders) > 0) {
        for (nested_folder in nested_folders) {
          if(basename(nested_folder) == folder_name){
            file.copy(nested_folder, dirname(dirname(nested_folder)),recursive = TRUE)
            unlink(nested_folder,recursive = TRUE)
            #print summary of what you did
            message(paste("Moved nested folder:", basename(nested_folder), "to", dirname(dirname(nested_folder))))
          }

        }
      }
    }
  }
}

