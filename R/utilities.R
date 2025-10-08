#' @title Recursively find and update a key's value within a 'chronData' sub-list.
#' @description This function looks for a list named 'chronData' at the top level
#' of the input list. It then traverses that 'chronData' sub-list. If it finds a
#' key whose value matches any element in a given vector, it appends a random
#' 8-character string to that value and updates the list.
#' @param nested_list A list, which must contain a top-level list named 'chronData'.
#' @param key_to_find The name of the key to search for within 'chronData'.
#' @param values_to_match A character vector of values to match against the key's value.
#' @return The modified list if an update was made. Otherwise, returns NA.
#' @examples
#' sample_list <- list(
#'   sessionId = "abc",
#'   chronData = list(
#'     a = 1,
#'     b = list(
#'       c = "find_me",
#'       d = list(
#'         e = "another_level"
#'       )
#'     ),
#'     f = "do_not_find"
#'   )
#' )
#'
#' values <- c("find_me", "some_other_value")
#'
#' # The search for key 'c' will occur inside 'chronData' and a match will be found.
#' updated_list <- find_and_update_nested_key(sample_list, "c", values)
#' print(updated_list)
#'
#' # No match will be found, so NA should be returned.
#' no_change_result <- find_and_update_nested_key(sample_list, "e", c("non_matching_value"))
#' print(no_change_result)

deduplicateChronTsids <- function(nested_list, key_to_find, values_to_match) {
  # Flag to track if any change has been made to the list.
  change_made <- FALSE

  # This is a recursive helper function that will perform the actual traversal.
  recursive_search <- function(x) {
    # Check if the current element is a list.
    if (is.list(x)) {
      # If the key exists at the current level...
      if (key_to_find %in% names(x)) {
        # ...and its value is in the vector of values to match...
        if (x[[key_to_find]] %in% values_to_match) {
          # ...generate a random 8-character string.
          random_suffix <- stri_rand_strings(1, 8)
          # Append the random string to the original value.
          x[[key_to_find]] <- paste0(x[[key_to_find]], "_", random_suffix)
          # Set the flag to TRUE, using <<- to modify the parent environment's variable.
          change_made <<- TRUE
        }
      }
      # Recursively apply the function to each element of the list.
      return(lapply(x, recursive_search))
    } else {
      # If the element is not a list, return it as is.
      return(x)
    }
  }

  # Check if 'chronData' exists at the top level of the input list and is a list itself.
  if ("chronData" %in% names(nested_list) && is.list(nested_list$chronData)) {
    # If it exists, apply the recursive search *only* to the 'chronData' sub-list.
    modified_chronData <- recursive_search(nested_list$chronData)

    # Update the main list with the potentially modified 'chronData' sub-list.
    nested_list$chronData <- modified_chronData
  }

  # --- MODIFIED RETURN LOGIC ---
  # If a change was made, return the modified list.
  if (change_made) {
    return(nested_list)
  } else {
    # Otherwise, return NA.
    return(NA)
  }
}

# --- Example Usage ---

# 1. Define a sample nested list that contains a 'chronData' list.
my_nested_list <- list(
  session_id = "xyz-789",
  unrelated_data = list(
    profile_type = "guest" # This will be ignored by the function.
  ),
  chronData = list(
    id = "user123",
    data = list(
      profile_type = "admin", # This should be found and modified.
      settings = list(
        theme = "dark",
        notifications = "enabled"
      )
    ),
    status = "active"
  )
)


#' Recursively find all values for a specific key in a nested list.
#'
#' @param lst The list to search through.
#' @param key The character name of the key to find.
#' @return A vector containing all the values found for the specified key.
#' @import purrr
#' @examples
#' example_list <- list(
#'   a = 1,
#'   b = list(
#'     c = 2,
#'     target_key = "value1"
#'   ),
#'   d = list(
#'     e = list(
#'       f = 3,
#'       target_key = "value2"
#'     )
#'   ),
#'   target_key = "value3",
#'   g = list(h = 4)
#' )
#'
#' extract_by_key(example_list, "target_key")
#' #> [1] "value3" "value1" "value2"

extract_by_key <- function(lst, key) {

  # Use purrr::reduce to recursively walk through the list
  purrr::reduce(lst, function(acc, elem) {

    # This is the recursive part:
    # If the current element is another list, call this function on it.
    if (is.list(elem)) {
      # c() combines the results from deeper levels with what we've found so far.
      return(c(acc, extract_by_key(elem, key)))
    }

    # This is the base case for the recursion:
    # If the element is not a list, check if it has the named key.
    # We use `names(elem)` in case the element is a named vector.
    if (key %in% names(elem)) {
      # If we find the key, add its value to our accumulator.
      return(c(acc, elem[[key]]))
    }

    # If the key is not found in this element, just return the accumulator.
    return(acc)

  }, .init = lst[[key]]) # Initialize with the value if key is at the top level
}

# --- Example Usage ---

# Define a sample nested list
sample_data <- list(
  id = "A1",
  metadata = list(
    timestamp = "2024-07-08",
    source = "sensor"
  ),
  payload = list(
    data_type = "temperature",
    reading = list(
      value = 25.5,
      unit = "Celsius",
      id = "B2" # A nested ID
    )
  ),
  another_payload = list(
    data_type = "humidity",
    reading = list(
      value = 60,
      unit = "%",
      id = "C3" # Another nested ID
    )
  ),
  id = "D4" # A duplicate key at the top level (lists can have them)
)
