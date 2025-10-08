listValues2Matrix <- function(AEV){
  if(is.list(AEV)){
    AEV <- as.matrix(as.data.frame(AEV))
  }
  return(AEV)
}

process_values_recursively <- function(x) {
  # Base case: if not a list, return as-is
  if (!is.list(x)) {
    return(x)
  }

  # Check if current list has a "values" element
  if ("values" %in% names(x)) {
    # Check if "values" is a list and transform it
    if (is.list(x$values)) {
      x$values <- listValues2Matrix(x$values)
    }
  }

  # Recursively process all elements in the list
  for (i in seq_along(x)) {
    x[[i]] <- process_values_recursively(x[[i]])
  }

  return(x)
}

#PD2 <- process_values_recursively(PD)
