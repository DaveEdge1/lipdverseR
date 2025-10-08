
# typical usage: `Dn <- purrr::map(D,handleDeleteMes,.progress = TRUE)`, then check for non-NAs.
# deal with the deleteMe's and the deleteThisColumns
handleDeleteMes <- function(L){
  tts <- as.lipdTsTibble(L)
  Ln <- NA
  td <- which(tts == "deleteMe",arr.ind = TRUE)
  if(nrow(td) > 0){
    for(i in 1:nrow(td)){
      tts[[td[i,2]]][td[i,1]] <- NA
    }
    message(glue("{L$dataSetName}: deleted {nrow(td)} 'deleteMe' from {unique(names(tts)[td[,2]])} "))
    Ln <- as.lipd(tts)
  }

  #check for "delete this column"
  td2 <- which(tts == "deleteMe",arr.ind = TRUE)
  if(nrow(td2) > 0 | any(grepl("delete",names(tts),ignore.case = TRUE))){
    stop("We found a delete this column!")
  }

  return(Ln)

}

#' Recursively Remove NA Values from a List with Verbose Output
#'
#' This function traverses a list, including any nested lists within it,
#' and removes any elements that are NA. It preserves the original
#' structure of the list. When an NA value is removed, it prints a message
#' to the console indicating the name (or path) of the key being removed.
#'
#' @param x A list, potentially containing nested lists and NA values.
#' @return A list with all NA values removed.
#'
#' @examples
#' my_list <- list(
#'   a = 1,
#'   b = NA,
#'   c = list(
#'     d = 4,
#'     e = list(f = NA, g = "hello"),
#'     h = NA,
#'     k = list() # Empty list to test edge case
#'   ),
#'   i = "world",
#'   j = list(k = 10, l = NA)
#' )
#'
#' # The function will now print messages as it removes NAs, like:
#' # Removing NA at key: b
#' # Removing NA at key: c$e$f
#' # Removing NA at key: c$h
#' # Removing NA at key: j$l
#' cleaned_list <- remove_na_recursive(my_list)
#' print(cleaned_list)
#' # Expected Output:
#' # $a
#' # [1] 1
#' #
#' # $c
#' # $c$d
#' # [1] 4
#' #
#' # $c$e
#' # $c$e$g
#' # [1] "hello"
#' #
#' # $c$k
#' # list()
#' #
#' # $i
#' # [1] "world"
#' #
#' # $j
#' # $j$k
#' # [1] 10
#' Recursively Remove NA Values from a List with Verbose Output
#'
#' This function traverses a list, including any nested lists within it,
#' and removes any elements that are NA. It preserves the original
#' structure of the list. When an NA value is removed, it prints a message
#' to the console indicating the name (or path) of the key being removed.
#'
#' @param x A list, potentially containing nested lists and NA values.
#' @return A list with all NA values removed.
#'
#' @examples
#' my_list <- list(
#'   a = 1,
#'   b = NA,
#'   c = list(
#'     d = 4,
#'     # Unnamed list as an element
#'     list(e = NA, f = "hello"),
#'     h = NA
#'   )
#' )
#'
#' # The function will now print messages as it removes NAs, like:
#' # Removing NA at key: b
#' # Removing NA at key: c[[1]]$e
#' # Removing NA at key: c$h
#' cleaned_list <- remove_na_recursive(my_list)
#' print(cleaned_list)

remove_na_recursive <- function(x) {
  # This internal worker function does the recursion and carries the path.
  # This keeps the main function signature clean for the user.
  worker <- function(sub_list, path_prefix) {
    # If the list is empty or not a list, return it as is.
    if (length(sub_list) == 0 || !is.list(sub_list)) {
      return(sub_list)
    }

    # Step 1: Recurse into any sub-lists first.
    # We iterate by index using seq_along for robustness with unnamed list elements.
    processed_list <- lapply(seq_along(sub_list), function(i) {
      item <- sub_list[[i]]

      # Get the name of the current item. If it's unnamed, use its index.
      item_name <- names(sub_list)[i]
      if (is.null(item_name) || !nzchar(item_name)) {
        # For unnamed elements, the path is represented by index in double brackets
        path_segment <- paste0("[[", i, "]]")
      } else {
        path_segment <- item_name
      }

      # Construct the path for the current item for potential recursion
      current_path <- if (nzchar(path_prefix)) {
        # Decide separator based on whether it's a named or indexed path
        sep <- if (grepl("\\[\\[", path_segment)) "" else "$"
        paste(path_prefix, path_segment, sep = sep)
      } else {
        path_segment
      }

      # If the item is a list, recurse. Otherwise, return it as is.
      if (is.list(item)) {
        return(worker(item, current_path))
      } else {
        return(item)
      }
    })
    # Restore the names of the processed list
    names(processed_list) <- names(sub_list)

    # Step 2: Filter NAs from the current level, now with messaging.
    # We build a logical vector indicating which elements to keep.
    to_keep <- vapply(seq_along(processed_list), function(i) {
      item <- processed_list[[i]]

      # The condition to identify an element to be removed.
      is_single_na <- is.atomic(item) && length(item) == 1 && is.na(item)

      if (is_single_na) {
        # If it's NA, print a message and mark it for removal (return FALSE).
        item_name <- names(processed_list)[i]
        if (is.null(item_name) || !nzchar(item_name)) {
          path_segment <- paste0("[[", i, "]]")
        } else {
          path_segment <- item_name
        }

        current_path <- if (nzchar(path_prefix)) {
          sep <- if (grepl("\\[\\[", path_segment)) "" else "$"
          paste(path_prefix, path_segment, sep = sep)
        } else {
          path_segment
        }

        message(paste("Removing NA at key:", current_path))
        return(FALSE)
      } else {
        # Otherwise, mark it to be kept (return TRUE).
        return(TRUE)
      }
    }, FUN.VALUE = logical(1)) # Ensures the output is a logical vector

    # Subset the list using the logical vector to remove the NAs.
    return(processed_list[to_keep])
  }

  # Initial call to the worker function to start the process.
  return(worker(x, path_prefix = ""))
}
