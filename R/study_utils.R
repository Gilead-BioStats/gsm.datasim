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

  # Calculate start dates for each timepoint
  base_date <- as.Date("2012-01-31")
  start_dates <- seq(base_date, length.out = timepoints, by = snapshot_width)

  raw_data_list <- list()

  for (i in 1:timepoints) {
    cat("Generating timepoint", i, "of", timepoints, "\n")

    config <- StudyConfig$new(
      study_id = study_id,
      participant_count = participants,
      site_count = sites
    )

    # Set temporal configuration with the specific start date for this timepoint
    config$set_temporal(
      start_date = start_dates[i],
      snapshot_count = 1,
      snapshot_width = snapshot_width
    )

    # Add enabled datasets based on mappings
    for (mapping in mappings) {
      config$add_dataset(mapping, enabled = TRUE)
    }

    raw_data <- generate_study_data_with_config(config)
    raw_data_list[[i]] <- raw_data
  }

  # Add the calculated dates as names to the list
  names(raw_data_list) <- as.character(start_dates)

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
#' @param study_type Study type ("standard", "endpoints", or custom)
#' @param analytics_package Package containing workflows (optional)
#' @param analytics_workflows Specific workflows to run (optional)
#' @return Analytics pipeline results
#' @export
execute_analytics_pipeline <- function(raw_data, config) {
  tryCatch({
    # Check if gsm.core is available
    if (!requireNamespace("gsm.core", quietly = TRUE)) {
      message("gsm.core package not available. Skipping analytics pipeline.")
      return(NULL)
    }

    cat("Running analytics pipeline on", length(raw_data), "timepoints...\n")

    # Use the latest timepoint for analytics (most complete data)
    latest_data <- raw_data[[length(raw_data)]]

    cat("Available datasets:", paste(names(latest_data), collapse = ", "), "\n")

    # Check if we have the minimum required datasets
    required_datasets <- c("Raw_SITE", "Raw_SUBJ")
    available_datasets <- intersect(required_datasets, names(latest_data[[1]]))

    if (length(available_datasets) < length(required_datasets)) {
      warning("Missing required datasets for analytics. Need: ",
              paste(required_datasets, collapse = ", "))
      return(NULL)
    }

    # First, map the raw data using gsm.mapping workflows
    cat("Mapping raw data to analysis-ready format...\n")


    # Create mapping workflows
    mappings_wf <- gsm.core::MakeWorkflowList(
      strNames = config$domains,
      strPath = "workflow/1_mappings",
      strPackage = "gsm.mapping"
    )

    # Ingest data using mappings workflow to create mapped data layer for analytics
    mappings_spec <- CombineSpecs(mappings_wf)
    lRaw <- Ingest(latest_data[[1]], mappings_spec)

    # Run mapping workflows on raw data
    mapped_data <- gsm.core::RunWorkflows(
      lWorkflow = mappings_wf,
      lData = lRaw
    )

    cat("Data mapping completed. Mapped datasets:", length(mapped_data), "\n")

    # Run actual GSM analytics workflows
    cat("Executing GSM workflows...\n")

    # Determine workflow configuration
    if (!is.null(config$study_params$analytics_package) && !is.null(config$study_params$analytics_workflows)) {
      # Custom package and workflows specified
      lWorkflow <- gsm.core::MakeWorkflowList(
        strPackage = config$analytics_package,
        strNames = config$analytics_workflows
      )
    } else if (!is.null(config$study_params$analytics_package)) {
      lWorkflow <- gsm.core::MakeWorkflowList(
        strPackage = config$study_params$analytics_package
      )
    } else {
      # Default fallback
      lWorkflow <- gsm.core::MakeWorkflowList(
        strPackage = "gsm.kri"
      )
    }

    # Run the workflows on the mapped data
    lResults <- gsm.core::RunWorkflows(
      lWorkflow = lWorkflow,
      lData = mapped_data
    )

    # Extract meaningful results
    analytics_summary <- list(
      total_participants = nrow(latest_data[[1]]$Raw_SUBJ %||% data.frame()),
      total_sites = nrow(latest_data[[1]]$Raw_SITE %||% data.frame()),
      domains_available = names(latest_data[[1]]),
      timepoints_processed = length(raw_data),
      workflows_executed = names(lResults),
      kri_results = length(lResults)
    )

    # Organize results
    analytics_results <- list(
      summary = analytics_summary,
      results = lResults,
      data = latest_data
    )

    cat("Analytics pipeline completed successfully!\n")
    cat("- Participants:", analytics_summary$total_participants, "\n")
    cat("- Sites:", analytics_summary$total_sites, "\n")
    cat("- Domains:", length(analytics_summary$domains_available), "\n")
    cat("- KRI Metrics:", analytics_summary$kri_results, "\n")

    return(analytics_results)

  }, error = function(e) {
    warning("GSM analytics pipeline failed: ", e$message)
    cat("Analytics pipeline skipped due to error.\n")
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
    cat("Analytics pipeline completed with results for 0 study-level metrics\n")
    return(NULL)
  }

  # Initialize organized results structure
  organized_results <- list(
    by_site = list(results = data.frame(), metadata = list()),
    by_country = list(results = data.frame(), metadata = list()),
    by_study = list(results = data.frame(), metadata = list())
  )

  # Count total metrics
  total_metrics <- 0

  # If we have GSM workflow results
  if ("results" %in% names(pipeline_results) && !is.null(pipeline_results$results)) {
    results <- pipeline_results$results

    # Process site-level results
    site_results <- results[grep("^site", names(results), ignore.case = TRUE)]
    if (length(site_results) > 0) {
      organized_results$by_site$results <- do.call(rbind, lapply(site_results, function(x) x$lResults))
      organized_results$by_site$metadata <- lapply(site_results, function(x) x$lData)
      total_metrics <- total_metrics + length(site_results)
    }

    # Process country-level results
    country_results <- results[grep("^country", names(results), ignore.case = TRUE)]
    if (length(country_results) > 0) {
      organized_results$by_country$results <- do.call(rbind, lapply(country_results, function(x) x$lResults))
      organized_results$by_country$metadata <- lapply(country_results, function(x) x$lData)
      total_metrics <- total_metrics + length(country_results)
    }

    # Process study-level results
    study_results <- results[grep("^study", names(results), ignore.case = TRUE)]
    if (length(study_results) > 0) {
      organized_results$by_study$results <- do.call(rbind, lapply(study_results, function(x) x$lResults))
      organized_results$by_study$metadata <- lapply(study_results, function(x) x$lData)
      total_metrics <- total_metrics + length(study_results)
    }

    # Add direct workflow results count
    total_metrics <- total_metrics + length(results)
  }

  # Report the total count
  if (total_metrics > 0) {
    cat("Analytics pipeline completed with results for", total_metrics, "study-level metrics\n")
  } else {
    cat("Analytics pipeline completed with results for 0 study-level metrics\n")
  }

  return(organized_results)
}

#' Generate raw data for study configuration
#'
#' @param config StudyConfig object with enabled datasets
#' @return List of raw data for enabled datasets
#' @export
generate_raw_data_from_config <- function(config) {
  # For now, fall back to using the existing generate_rawdata_for_single_study function
  # since the Raw_ functions require complex spec and previous_data parameters

  # Extract enabled dataset names and convert to mappings format
  enabled_datasets <- names(config$dataset_configs)[
    sapply(config$dataset_configs, function(x) x$enabled)
  ]

  # Convert Raw_ dataset names to mapping names by removing Raw_ prefix
  mapping_names <- gsub("^Raw_", "", enabled_datasets)

  cat("Generating raw data for", length(mapping_names), "domains...\n")

  # Use the existing function with proper parameters, including start date
  raw_data <- generate_rawdata_for_single_study(
    SnapshotCount = config$temporal_config$snapshot_count,
    SnapshotWidth = config$temporal_config$snapshot_width,
    ParticipantCount = config$study_params$participant_count,
    SiteCount = config$study_params$site_count,
    StudyID = config$study_params$study_id,
    strStartDate = as.character(config$temporal_config$start_date),
    workflow_path = "workflow/1_mappings",
    mappings = mapping_names,
    package = "gsm.mapping"
  )

  return(raw_data)
}

#' Generate study data with configuration
#'
#' @param config StudyConfig object
#' @param workflow_path Path to workflow mappings (not used in new approach)
#' @param mappings Optional mappings list (not used in new approach)
#' @param package Package containing workflows (not used in new approach)
#' @return List of generated study data
#' @export
generate_study_data_with_config <- function(config, workflow_path = "workflow/1_mappings",
                                           mappings = NULL, package = "gsm.mapping") {

  # Use the new helper function instead of the old generate_rawdata_for_single_study
  raw_data <- generate_raw_data_from_config(config)

  return(raw_data)
}
