#' Load the LiPDverse Database with Enhanced Reporting
#'
#' This function loads all .lpd files from a specified path, checks for a
#' cached version, and runs several validation checks, reporting the
#' status of each step with clear visual cues using the 'cli' package.
#'
#' @param path The file path to the directory containing the LiPDverse database.
#' @return A list 'D' where each element is a LiPD object.
#' @import cli
#' @import purrr
#' @import glue
#' @import tools
#' @import future
#' @import lipdR
#'
loadLipdverseDatabase <- function(path = "~/Dropbox/lipdverse/database/") {
  # --- Setup and Package Check ---
  # The 'cli' package is essential for the enhanced visual reporting.
  if (!requireNamespace("cli", quietly = TRUE)) {
    stop("The 'cli' package is required for the enhanced reporting. Please install it using: install.packages('cli')")
  }

  # The 'lipdR' package is needed for its core functions
  if (!requireNamespace("lipdR", quietly = TRUE)) {
    stop("The 'lipdR' package is required. Please install it.")
  }

  cli::cli_h1("Loading LiPDverse Database")

  # --- Step 1: Find LiPD files ---
  cli::cli_process_start("Scanning for {.lpd .lpd} files in {.path {path}}")
  allLipd <- list.files(path, pattern = "\\.lpd$", full.names = TRUE)
  cli::cli_process_done()

  if (length(allLipd) == 0) {
    cli::cli_alert_danger("No {.lpd .lpd} files found in the specified path: {.path {path}}")
    return(NULL)
  }
  cli::cli_alert_info("Found {length(allLipd)} {.lpd .lpd} file{?s}.")

  # --- Step 2: Check for a cached database version using an MD5 hash ---
  cli::cli_process_start("Calculating database checksum to check for changes...")
  allmd5 <- tools::md5sum(allLipd)
  md5f <- file.path(tempdir(), "lipdverseAllMd5.txt")
  write(allmd5, file = md5f)
  tc <- tools::md5sum(md5f)
  rds <- file.path(path, paste0(tc, ".RDS"))
  cli::cli_process_done()

  # --- Step 3: Load data from cache or from source files ---
  if (file.exists(rds)) {
    cli::cli_alert_success("Database is up-to-date. Loading from cached version: {.path {basename(rds)}}")
    D <- readRDS(rds)
  } else {
    cli::cli_alert_warning("No up-to-date cache found. Loading all datasets from source files.")

    # Remove any old, outdated RDS cache files
    toDelete <- list.files(path, pattern = "\\.RDS$", full.names = TRUE)
    if (length(toDelete) > 0) {
      unlink(x = toDelete)
      cli::cli_alert_info("Removed {length(toDelete)} old cache file{?s}.")
    }

    # Load from source files in parallel
    cli::cli_process_start("Loading {length(allLipd)} dataset{?s}... (this may take a while)")
    future::plan(future::sequential) #reset first
    future::plan(future::multisession, workers = 16)
    D <- lipdR::readLipd(path, parallel = TRUE)
    cli::cli_process_done()

    cli::cli_alert_success("Successfully loaded {length(D)} dataset{?s} from source files.")

    # Save a new cache for faster loading next time
    cli::cli_process_start("Saving new cache to {.path {basename(rds)}} for future use")
    saveRDS(D, file = rds)
    cli::cli_process_done()
  }

  cli::cli_h1("Verifying Database Integrity")

  # --- Helper function for running and reporting checks ---
  # This avoids repeating the same reporting logic for each check.
  run_check <- function(check_name, check_results, dataset_names) {
    if (all(check_results)) {
      cli::cli_alert_success("{.field {check_name}} check {.strong PASSED}")
    } else {
      n_bad <- sum(!check_results)
      failed_dsns <- dataset_names[!check_results]
      cli::cli_alert_danger("{.field {check_name}} check {.strong FAILED} for {n_bad} dataset{?s}")

      # List problematic files if the list is not too long, for easier debugging
      if (n_bad <= 15) {
        cli::cli_ul("Problematic dataset{?s}: {.val {failed_dsns}}")
      }
    }
  }

  # Get dataset names for reporting purposes
  dsn <- purrr::map_chr(D, "dataSetName", .default = "UnknownName")

  # --- Step 4: Check for duplicated datasetIds ---
  cli::cli_process_start("Checking for duplicates...")

  dsid <- purrr::map_chr(D, "datasetId", .default = "UnknownID")
  run_check("Duplicate datasetIds", !duplicated(dsid), dsn)

  # --- Step 5a: Check for duplicated dataSetNames ---
  run_check("Duplicate dataSetNames", !duplicated(dsn), dsn)

  # --- Step 5b: Check for duplicated TSids ---
  allTsids <- map(D, extract_by_key, key = "TSid",.progress = TRUE) |>
    list_c()

  run_check("Duplicate TSids", !duplicated(allTsids), allTsids)
  if(any(duplicated(allTsids))){
    allTsids <<- allTsids
  }

  cli::cli_process_done()


  # --- Step 6: Check for standardized vocabulary ---
  cli::cli_process_start("Checking for standardized vocabulary...")
  standardTables <- readRDS(url("https://lipdverse.org/lipdverse/standardTables.RDS", "rb"))
  good_vocab <- purrr::map_lgl(D, hasStandardizedVocabulary, standardTables,.progress = TRUE)
  if(!all(good_vocab)){
    good_vocab <<- good_vocab
  }
  cli::cli_process_done()
  run_check("Standardized Vocabulary", good_vocab, dsn)

  # --- Step 7: Check for valid LiPD files ---
  cli::cli_process_start("Checking for LiPD validity...")
  valid <- purrr::map_lgl(D, lipdR::validLipd)
  cli::cli_process_done()
  run_check("LiPD Validity", valid, dsn)
  if(!all(valid)){
    valid <<- valid
  }

  # --- Step 8: Check if data pages are current ---
  cli::cli_process_start("Checking if data pages are current...")
  ac <- purrr::map_lgl(D, isDataPageCurrent)
  if(!all(ac)){
    ac <<- ac
  }
  cli::cli_process_done()
  run_check("Current Data Pages", ac, dsn)

  # --- Step 9: Check if data pages are the highest version ---
  cli::cli_process_start("Checking if data pages are the highest version...")
  hv <- purrr::map_lgl(D, isDataPageHighestVersion)
  if(!all(hv)){
    hv <<- hv
  }
  cli::cli_process_done()
  run_check("Highest Version", hv, dsn)

  cli::cli_h1("Load and Verification Complete")

  return(D)
}
