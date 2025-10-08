inP2k <- timeseriesInCompilation(ts, "Pages2kTemperature")
i <- filter(ts,inP2k) |>
  select(dsn = dataSetName, dsid = datasetId) |>
  distinct()

dscomp <- data.frame(dsn = map_chr(D,"dataSetName"),dsid = map_chr(D,"datasetId"),inComp = FALSE)

tf <- which(dscomp$dsid %in% combin$dsid)

dscomp$inComp[tf] <- TRUE

dscomp$instructions <- ""

dscomp$instructions[1] <- 'Any datasets marked as FALSE will not be considered for the update, NA or TRUE will be considered.'


write_sheet_retry(dscomp, ss = "1_ZvQXV-jXMLi7DSC9vc8tAXdlimfkYrna0fKbIB_5Og", sheet = "datasetsInCompilation")
