#' High-Level Study Creation API
#'
#' This file contains convenience functions for creating studies with minimal configuration.
#' These functions provide simplified interfaces for common study types.

#' Create longitudinal study data
#'
#' Creates a complete longitudinal study with multiple timepoints
#' @param study_id Study identifier
#' @param participants Number of participants
#' @param sites Number of sites
#' @param timepoints Number of timepoints
#' @param interval Time between snapshots (e.g., "1 month", "2 weeks")
#' @param domains Clinical domains to include
#' @param include_pipeline Whether to run analytics pipeline
#' @return LongitudinalStudy object with generated data
#' @export
create_longitudinal_study <- function(study_id = "STUDY-001",
                                     participants = 100,
                                     sites = 10,
                                     timepoints = 5,
                                     interval = "1 month",
                                     domains = c("AE", "LB", "VISIT"),
                                     include_pipeline = TRUE) {

  # Validate inputs
  validate_study_inputs(participants, sites, timepoints, domains)

  # Ensure required mappings are included
  mappings <- ensure_core_mappings(domains)

  # Generate the raw data for all timepoints
  raw_data <- generate_study_snapshots(study_id, participants, sites, timepoints, interval, mappings)

  # Create study object
  study <- LongitudinalStudy$new(
    study_id = study_id,
    raw_data = raw_data,
    timepoints = timepoints,
    participant_count = participants,
    site_count = sites
  )

  # Run analytics pipeline if requested
  if (include_pipeline) {
    cat("Running analytics pipeline...\n")

    # Create configuration for analytics
    config <- StudyConfig$new(
      study_id = study_id,
      participant_count = participants,
      site_count = sites
    )

    # Run pipeline on the most recent timepoint
    latest_data <- raw_data[[length(raw_data)]]
    pipeline_results <- execute_analytics_pipeline(latest_data, config)
    study$analytics <- organize_analytics_results(pipeline_results)
  }

  return(study)
}

#' Quick longitudinal study creation
#'
#' Creates a complete longitudinal study with sensible defaults and runs analytics
#' @param study_name Name of the study
#' @param participants Number of participants (default 1000)
#' @param sites Number of sites (default 150)
#' @param months_duration Duration in months (default 24)
#' @param study_type Type of study - "standard" or "endpoints"
#' @return LongitudinalStudy object with complete data and analytics
#' @export
quick_longitudinal_study <- function(study_name = "Clinical Trial",
                                    participants = 1000,
                                    sites = 150,
                                    months_duration = 24,
                                    study_type = "standard") {

  cat("Creating", study_type, "longitudinal study:", study_name, "\n")
  cat("Parameters:", participants, "participants,", sites, "sites,", months_duration, "months\n")

  # Create study ID from name
  study_id <- gsub("[^A-Za-z0-9]", "-", toupper(study_name))

  # Determine domains based on study type
  domains <- if (study_type == "standard") {
    c("AE", "LB", "VISIT", "PD", "PK", "QUERY", "DATACHG", "DATAENT", "STUDCOMP", "SDRGCOMP", "IE", "EXCLUSION")
  } else if (study_type == "endpoints") {
    c("AE", "LB", "VISIT", "PD", "STUDCOMP")
  } else {
    stop("study_type must be 'standard' or 'endpoints'")
  }

  # Create the longitudinal study directly
  study <- create_longitudinal_study(
    study_id = study_id,
    participants = participants,
    sites = sites,
    timepoints = months_duration,
    interval = "1 month",
    domains = domains,
    include_pipeline = TRUE
  )

  cat("Study creation completed successfully!\n")
  cat("Generated", length(study$raw_data), "timepoints with", length(study$get_domain_names()), "domains\n")

  if (!is.null(study$analytics)) {
    cat("Analytics pipeline completed with results for",
        nrow(study$analytics$by_study$results), "study-level metrics\n")
  }

  return(study)
}
