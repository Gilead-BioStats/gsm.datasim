#' Study Utility Functions
#'
#' This file contains utility functions for study validation, data generation,
#' and analytics pipeline execution.

#' Validate study input parameters
#'
#' @param participants Number of participants
#' @param sites Number of sites
#' @param timepoints Number of timepoints
#' @param domains Domains to include
#' @export
validate_study_inputs <- function(participants, sites, timepoints, domains) {
  if (participants <= 0) stop("Participants must be positive")
  if (sites <= 0) stop("Sites must be positive")
  if (timepoints <= 0) stop("Timepoints must be positive")
  if (length(domains) == 0) stop("At least one domain must be specified")
}

#' Ensure core mappings are included
#'
#' @param domains Vector of domain names
#' @return Vector with required core mappings added
#' @export
ensure_core_mappings <- function(domains) {
  # Required core mappings for any study
  required_mappings <- c("STUDY", "SITE", "SUBJ", "ENROLL")

  # Convert domains to mapping names (add Raw_ prefix)
  mapping_names <- paste0("Raw_", domains)

  # Add required mappings
  all_mappings <- unique(c(paste0("Raw_", required_mappings), mapping_names))

  return(all_mappings)
}

#' Generate study data across multiple timepoints
#'
#' @param study_id Study identifier
#' @param participants Number of participants
#' @param sites Number of sites
#' @param timepoints Number of timepoints
#' @param interval Time interval between snapshots
#' @param mappings Vector of mapping names to use
#' @return List of raw data for each timepoint
#' @export
generate_study_snapshots <- function(study_id, participants, sites, timepoints, interval, mappings) {
  snapshot_width <- parse_interval_to_snapshot_width(interval)

  raw_data_list <- list()

  for (i in 1:timepoints) {
    cat("Generating timepoint", i, "of", timepoints, "\n")

    config <- StudyConfig$new(
      study_id = study_id,
      participant_count = participants,
      site_count = sites
    )
    config$set_temporal(snapshot_count = 1, snapshot_width = snapshot_width)

    # Add enabled datasets based on mappings
    for (mapping in mappings) {
      config$add_dataset(mapping, enabled = TRUE)
    }

    raw_data <- generate_study_data_with_config(config)
    raw_data_list[[i]] <- raw_data
  }

  return(raw_data_list)
}

#' Parse interval string to snapshot width
#'
#' @param interval Interval string (e.g., "1 month", "2 weeks")
#' @return Snapshot width for temporal configuration
#' @export
parse_interval_to_snapshot_width <- function(interval) {
  if (grepl("month", interval, ignore.case = TRUE)) {
    return("months")
  } else if (grepl("week", interval, ignore.case = TRUE)) {
    return("weeks")
  } else if (grepl("day", interval, ignore.case = TRUE)) {
    return("days")
  } else {
    return("months")  # default
  }
}

#' Execute analytics pipeline
#'
#' @param raw_data Raw study data
#' @param config StudyConfig object
#' @return Analytics pipeline results
#' @export
execute_analytics_pipeline <- function(raw_data, config) {
  tryCatch({
    # Check if gsm.core is available
    if (!requireNamespace("gsm.core", quietly = TRUE)) {
      warning("gsm.core package not available. Skipping analytics pipeline.")
      return(NULL)
    }

    # Get workflow mappings - using available mappings from package
    if (!requireNamespace("gsm.mapping", quietly = TRUE)) {
      warning("gsm.mapping package not available. Skipping analytics pipeline.")
      return(NULL)
    }
    
    # Use available mapping functions
    tryCatch({
      lMappings <- gsm.mapping::CombineSpecs(list())
    }, error = function(e) {
      warning("Could not load mappings: ", e$message)
      return(NULL)
    })

    # Filter to only enabled datasets
    enabled_datasets <- names(config$dataset_configs)[
      sapply(config$dataset_configs, function(x) x$enabled)
    ]

    # Convert to mapping names by removing Raw_ prefix
    mapping_names <- gsub("^Raw_", "Mapped_", enabled_datasets)
    available_mappings <- intersect(names(lMappings), mapping_names)

    if (length(available_mappings) == 0) {
      warning("No compatible mappings found for analytics pipeline")
      return(NULL)
    }

    # Filter mappings to available data
    lMappings <- lMappings[available_mappings]

    # Run the analytics workflow
    pipeline_results <- gsm.core::RunWorkflow(
      lData = raw_data,
      lMapping = lMappings
    )

    return(pipeline_results)

  }, error = function(e) {
    warning("Analytics pipeline failed: ", e$message)
    return(NULL)
  })
}

#' Organize analytics pipeline results
#'
#' @param pipeline_results Raw pipeline results from gsm.core
#' @return Organized analytics results
#' @export
organize_analytics_results <- function(pipeline_results) {
  if (is.null(pipeline_results)) {
    return(NULL)
  }

  # Initialize organized results structure
  organized_results <- list(
    by_site = list(results = data.frame(), metadata = list()),
    by_country = list(results = data.frame(), metadata = list()),
    by_study = list(results = data.frame(), metadata = list())
  )

  # Process site-level results
  site_results <- pipeline_results[grep("^site", names(pipeline_results), ignore.case = TRUE)]
  if (length(site_results) > 0) {
    organized_results$by_site$results <- do.call(rbind, lapply(site_results, function(x) x$lResults))
    organized_results$by_site$metadata <- lapply(site_results, function(x) x$lData)
  }

  # Process country-level results
  country_results <- pipeline_results[grep("^country", names(pipeline_results), ignore.case = TRUE)]
  if (length(country_results) > 0) {
    organized_results$by_country$results <- do.call(rbind, lapply(country_results, function(x) x$lResults))
    organized_results$by_country$metadata <- lapply(country_results, function(x) x$lData)
  }

  # Process study-level results
  study_results <- pipeline_results[grep("^study", names(pipeline_results), ignore.case = TRUE)]
  if (length(study_results) > 0) {
    organized_results$by_study$results <- do.call(rbind, lapply(study_results, function(x) x$lResults))
    organized_results$by_study$metadata <- lapply(study_results, function(x) x$lData)
  }

  return(organized_results)
}

#' Generate study data with configuration
#'
#' @param config StudyConfig object
#' @param workflow_path Path to workflow mappings
#' @param mappings Optional mappings list to use
#' @param package Package containing the workflows
#' @return List of generated study data
#' @export
generate_study_data_with_config <- function(config, workflow_path = "workflow/1_mappings",
                                           mappings = NULL, package = "gsm.mapping") {

  # Determine which mappings to use based on enabled datasets
  if (is.null(mappings)) {
    # Extract dataset types and convert to mapping names
    enabled_datasets <- names(config$dataset_configs)[
      sapply(config$dataset_configs, function(x) x$enabled)
    ]

    # Get available mappings - simplified approach
    if (!requireNamespace("gsm.mapping", quietly = TRUE)) {
      warning("gsm.mapping package not available")
      mappings <- list()
    } else {
      # Use a basic mapping approach since get_shared_mapping doesn't exist
      mappings <- list()
    }

    if (length(mappings) == 0) {
      stop("No mappings found for enabled datasets: ", paste(enabled_datasets, collapse = ", "))
    }
  }

  # Generate raw data using the raw data generator
  cat("Generating raw data for", length(mappings), "domains...\n")

  # Use generate_rawdata_for_single_study function
  raw_data <- generate_rawdata_for_single_study(
    SnapshotCount = config$temporal_config$snapshot_count,
    SnapshotWidth = config$temporal_config$snapshot_width,
    ParticipantCount = config$study_params$participant_count,
    SiteCount = config$study_params$site_count,
    StudyID = config$study_params$study_id,
    workflow_path = workflow_path,
    mappings = gsub("^Mapped_", "", names(mappings)),
    package = package
  )

  return(raw_data)
}