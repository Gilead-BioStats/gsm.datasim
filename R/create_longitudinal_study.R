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
#' @param analytics_package Package containing workflows (optional)
#' @param analytics_workflows Specific workflows to run (optional)
#' @return LongitudinalStudy object with generated data
#' @export
create_longitudinal_study <- function(study_id = "STUDY-001",
                                     participants = 100,
                                     sites = 10,
                                     timepoints = 5,
                                     interval = "1 month",
                                     domains = c("AE", "LB", "VISIT"),
                                     include_pipeline = TRUE,
                                     analytics_package = NULL,
                                     analytics_workflows = NULL) {

  # Validate inputs
  validate_study_inputs(participants, sites, timepoints, domains)

  # Ensure required mappings are included
  mappings <- ensure_core_mappings(domains)

  # Generate the raw data for all timepoints
  raw_data <- generate_study_snapshots(study_id, participants, sites, timepoints, interval, mappings)

  # Create study object with proper config
  config <- list(
    participants = participants,
    sites = sites,
    timepoints = timepoints,
    interval = interval,
    domains = domains,
    study_type = "standard",  # default for this function
    analytics_package = analytics_package,
    analytics_workflows = analytics_workflows
  )

  study <- LongitudinalStudy$new(
    study_id = study_id,
    raw_data = raw_data,
    config = config
  )

  # Run analytics pipeline if requested
  if (include_pipeline) {
    cat("Running analytics pipeline...\n")

    # Create configuration for analytics
    config <- StudyConfig$new(
      study_id = study_id,
      participant_count = participants,
      site_count = sites,
      analytics_package = analytics_package,
      analytics_workflows = analytics_workflows
    )

    # Run pipeline on all timepoint data
    pipeline_results <- execute_analytics_pipeline(
      raw_data,
      config
    )
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
#' @param run_analytics Whether to run the analytics pipeline (default TRUE)
#' @return LongitudinalStudy object with complete data and analytics
#' @export
quick_longitudinal_study <- function(study_name = "Clinical Trial",
                                    participants = 1000,
                                    sites = 150,
                                    months_duration = 24,
                                    study_type = "standard",
                                    run_analytics = TRUE) {

  cat("Creating", study_type, "longitudinal study:", study_name, "\n")
  cat("Parameters:", participants, "participants,", sites, "sites,", months_duration, "months\n")

  # Create study ID from name
  study_id <- gsub("[^A-Za-z0-9]", "-", toupper(study_name))

  # Determine domains based on study type
  if (study_type == "standard") {
    domains <- c("AE", "LB", "VISIT", "PD", "PK", "QUERY", "DATACHG", "DATAENT", "STUDCOMP", "SDRGCOMP", "IE", "EXCLUSION")
    pkg <- "gsm.kri"
  } else if (study_type == "endpoints") {
    domains <- c("AE", "LB", "VISIT", "PD", "STUDCOMP", "SDRGCOMP", "AntiCancer", "Baseline", "Death", "OverallResponse", "Randomization", "Consents")
    pkg <- "gsm.endpoints"
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
    include_pipeline = run_analytics,
    analytics_package = pkg
  )

  cat("Study creation completed successfully!\n")
  cat("Generated", length(study$raw_data), "timepoints with", length(study$get_domain_names()), "domains\n")

  if (!is.null(study$analytics)) {
    cat("Analytics pipeline completed with results for",
        nrow(study$analytics$by_study$results), "study-level metrics\n")
  }

  return(study)
}
