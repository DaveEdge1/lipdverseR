is_duplicate_pub <- function(item1, item2, similarity_threshold = 0.9) {
  #' Determines if two publication items are duplicates based on key metadata fields.
  #'
  #' @param item1 A list representing the first publication item.
  #' @param item2 A list representing the second publication item.
  #' @param similarity_threshold The threshold for string similarity (0.0 to 1.0).
  #' @return A boolean value: TRUE if the items are considered duplicates, FALSE otherwise.

  fields_to_check <- c("doi", "journal", "title", "year")

  match_count <- 0
  present_fields_count <- 0

  for (field in fields_to_check) {
    # Check if the field name exists and is not NULL in both items
    if (!is.null(item1[[field]]) && !is.null(item2[[field]])) {
      present_fields_count <- present_fields_count + 1

      val1 <- item1[[field]]
      val2 <- item2[[field]]

      # For 'year', we expect an exact match
      if (field == "year") {
        if (as.character(val1) == as.character(val2)) {
          match_count <- match_count + 1
        }
      } else {
        # For string fields, check for exact or close match
        # Normalize strings: lowercase and trim whitespace
        s_val1 <- trimws(tolower(as.character(val1)))
        s_val2 <- trimws(tolower(as.character(val2)))

        if (s_val1 == s_val2) {
          match_count <- match_count + 1
        } else {
          # Use stringsim for close matches.
          # method='jw' (Jaro-Winkler) is good for short strings like titles.
          similarity <- stringsim(s_val1, s_val2, method = "jw")
          if (similarity >= similarity_threshold) {
            match_count <- match_count + 1
          }
        }
      }
    }
  }

  # If there are no common fields to compare, they are not duplicates
  if (present_fields_count == 0) {
    return(FALSE)
  }

  # If all present fields match, it's a duplicate.
  # If at least two fields match, it's also considered a duplicate.
  if (match_count == present_fields_count || match_count >= 2) {
    return(TRUE)
  }

  return(FALSE)
}


find_all_duplicates <- function(pub_list, similarity_threshold = 0.9) {
  #' Finds all sets of duplicate publications within a list.
  #'
  #' @param pub_list A list of publication items (each item is a list).
  #' @param similarity_threshold The similarity threshold to pass to is_duplicate.
  #' @return A list of numeric vectors. Each vector contains the indices of a
  #'         set of duplicate items. Returns an empty list if no duplicates are found.

  n <- length(pub_list)
  if (n < 2) {
    return(list()) # Cannot have duplicates with less than 2 items
  }

  processed_indices <- c()
  duplicate_groups <- list()

  # Iterate through each item in the publication list
  for (i in 1:(n - 1)) {
    # If this index has already been grouped, skip it
    if (i %in% processed_indices) {
      next
    }

    # Start a new potential group with the current item's index
    current_group <- c(i)

    # Compare the current item with all subsequent items
    for (j in (i + 1):n) {
      # If this index has already been grouped, skip it
      if (j %in% processed_indices) {
        next
      }

      # Check if item j is a duplicate of item i
      if (is_duplicate(pub_list[[i]], pub_list[[j]], similarity_threshold)) {
        # If it's a duplicate, add its index to the current group
        current_group <- c(current_group, j)
      }
    }

    # If the group has more than one member, it's a set of duplicates
    if (length(current_group) > 1) {
      duplicate_groups <- append(duplicate_groups, list(current_group))
      # Add all members of this new group to the processed list
      processed_indices <- c(processed_indices, current_group)
    }
  }

  return(duplicate_groups)
}

pubDups <- map(D,\(x) find_all_duplicates(x$pub),.progress = TRUE)

dd <- map_lgl(pubDups,\(x) length(x) > 0)

DP <- D[dd]

removeDuplicatePubs <- function(L){
  ds <- find_all_duplicates(L$pub) |> unlist()
  if(length(ds) == 1){
    stop("shouldnt")
  }
  message(glue("{L$dataSetName}: removing {length(ds) - 1} pubs"))
  if(length(ds) > 1){
    L$pub <- L$pub[-ds[-1]]
  }

  return(L)

}

sum(dd)
find_all_duplicates(L$pub)
is_duplicate_pub(L$pub[[26]],L$pub[[1]])

nPubs <- map_dbl(D, \(x) length(x$pub),.progress = TRUE)

dois <- map_chr(L$pub,\(x) ifelse(!is.null(x$doi),x$doi,"nodoi"))

fixed <- map(DP,removeDuplicatePubs)
