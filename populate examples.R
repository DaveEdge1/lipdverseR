keys <- read_sheet_retry("14DCrnkT0AGaWCOr61UCKdNASMUmWIZ-MYsdLWUF9Gis")
gk <- filter(keys, good)


examples <- matrix(NA,nrow = nrow(keys))
for(i in 1:nrow(keys)){
  if(!keys$Good[i]){
    next
  }
  print(i)
  thisKey <- keys$key[i]



  if(object.size(ts[[thisKey]]) > 1e8){
    next
  }

  tab <- try(ts |>  select(!!thisKey) |>  table() ,silent = TRUE)
  if(is(tab,"try-error")){
    tab <- try(ts |>  select(!!thisKey) |> unlist() |> table() ,silent = TRUE)
  }
  if(is(tab,"try-error")){
    next
  }
  tab <- tab |>
    as.data.frame() |>
    arrange(desc(Freq))

  n <- min(nrow(tab),3)
  examples[i] <- tab[1:n,1] |> paste(collapse = ", ")

}
keys$`Example terms` <- examples
write_sheet_retry("14DCrnkT0AGaWCOr61UCKdNASMUmWIZ-MYsdLWUF9Gis",sheet = "uploaded")
