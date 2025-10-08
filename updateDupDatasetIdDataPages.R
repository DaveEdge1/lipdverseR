setwd("~/Dropbox/lipdverse/html/data")
alldp <- list.files()

changed <- googlesheets4::read_sheet(ss = "15sQwPJImE3PLJWQXksYg4uBxSo93uuB60ySdiMcJgPs")

for(i in changed$oldDsid){
  if(!paste0(i,"iso") %in% alldp){

    message(paste0("Creating new directory for ", paste0(i,"iso")))

    #create a new directory
    dir.create(file.path("~/Dropbox/lipdverse/html/data",paste0(i,"iso")), showWarnings = FALSE)

    #figure out which versions are before last september
    aftc <- list.files(file.path("~/Dropbox/lipdverse/html/data",i),full.names = TRUE)
    #figure out when these files were created
    #get the modification times
    mt <- file.mtime(file.path(aftc,"index.html"))

    #which modtimes were before september 2024
    before <- aftc[mt < as.POSIXct("2024-09-01")]
    after <- aftc[mt >= as.POSIXct("2024-09-01")]
    after <- after[!is.na(after)]

    #copy the afters to the new directory
    for(a in after){
    dir.create(file.path("~/Dropbox/lipdverse/html/data",paste0(i,"iso"),basename(a)), showWarnings = FALSE)
    file.copy(from = file.path(a),
              to = file.path("~/Dropbox/lipdverse/html/data",paste0(i,"iso"),basename(a)),
              recursive = TRUE,overwrite = TRUE)

    unlink( a,recursive = TRUE)
    }

    file.copy(from = file.path("~/Dropbox/lipdverse/html/data",i,"index.html"),
              to = file.path("~/Dropbox/lipdverse/html/data",paste0(i,"iso"),"index.html"),
             overwrite = TRUE)


  }

}
