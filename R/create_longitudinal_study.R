#' High-Level Study Creation API
#'
#' This file contains convenience functions for creating studies with minimal configuration.
#' These functions provide simplified interfaces for common study types.

#' Create longitudinal study data
#'
#' Creates a complete longitudinal study with multiple snapshots
#' @param study_id Study identifier
#' @param participants Number of participants
#' @param sites Number of sites
#' @param snapshots Number of snapshots
#' @param interval Time between snapshots (e.g., "1 month", "2 weeks")
#' @param domains Clinical domains to include
#' @param include_pipeline Whether to run analytics pipeline
#' @param analytics_package Package containing workflows (optional)
#' @param analytics_workflows Specific workflows to run (optional)
#' @param verbose Whether to print progress/output messages
#' @return LongitudinalStudy object with generated data
#' @export
create_longitudinal_study <- function(study_id = "STUDY-001",
                                     participants = 100,
                                     sites = 10,
                                     snapshots = 5,
                                     interval = "1 month",
                                     domains = c("AE", "LB", "VISIT"),
                                     include_pipeline = TRUE,
                                     analytics_package = NULL,
                                     analytics_workflows = NULL,
                                     verbose = FALSE) {

  # Validate inputs
  validate_study_inputs(participants, sites, snapshots, domains)

  # Ensure required mappings are included
  mappings <- ensure_core_mappings(domains)

  # Generate the raw data for all snapshots
  raw_data <- generate_study_snapshots(study_id, participants, sites, snapshots, interval, mappings, verbose = verbose)

  # Create study object with proper config
  config <- list(
    participants = participants,
    sites = sites,
    snapshots = snapshots,
    interval = interval,
    domains = domains, # domains should be without Raw_
    study_type = "standard",  # default for this function
    analytics_package = analytics_package,
    analytics_workflows = analytics_workflows,
    verbose = verbose
  )

  study <- create_longitudinal_study_data(
    study_id = study_id,
    raw_data = raw_data,
    config = config
  )

  # Run analytics pipeline if requested
  if (include_pipeline) {
    if (isTRUE(verbose)) cat("Running analytics pipeline...\n")

    # Create configuration for analytics
    analytics_config <- create_study_config(
      study_id = study_id,
      participant_count = participants,
      site_count = sites,
      analytics_package = analytics_package,
      analytics_workflows = analytics_workflows
    )
    analytics_config$domains <- domains
    analytics_config$verbose <- verbose

    study$analytics <- generate_analytics_layers(
      raw_data = raw_data,
      config = analytics_config,
      verbose = verbose
    )
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
#' @param verbose Whether to print progress/output messages
#' @return LongitudinalStudy object with complete data and analytics
#' @export
quick_longitudinal_study <- function(study_name = "GS-US-000-0001",
                                    participants = 1000,
                                    sites = 150,
                                    months_duration = 24,
                                    study_type = "standard",
                                    run_analytics = TRUE,
                                    verbose = FALSE) {

  if (isTRUE(verbose)) {
    cat("Creating", study_type, "longitudinal study:", study_name, "\n")
    cat("Parameters:", participants, "participants,", sites, "sites,", months_duration, "months\n")
  }

  # Create study ID from name
  study_id <- gsub("[^A-Za-z0-9]", "-", toupper(study_name))

  # Determine domains based on study type

  if (study_type == "standard") {
    # Only include standard domains, no endpoints-only domains
    domains <- c("AE", "LB", "VISIT", "PD", "PK", "QUERY", "DATACHG", "DATAENT", "STUDCOMP", "SDRGCOMP", "IE", "EXCLUSION")
    pkg <- "gsm.kri"
    study <- create_longitudinal_study(
      study_id = study_id,
      participants = participants,
      sites = sites,
    snapshots = months_duration,
      interval = "1 month",
      domains = domains,
      include_pipeline = run_analytics,
      analytics_package = pkg,
      verbose = verbose
    )
  } else if (study_type == "endpoints") {
    # Endpoints: use multi-package logic
    domain_pkg_df <- get_endpoints_domains()
    domains <- domain_pkg_df$domain
    # Build config
    config <- create_study_config(
      study_id = study_id,
      participant_count = participants,
      site_count = sites
    )
    # Add all endpoint/mapping domains
    for (d in domains) {
      config <- add_dataset_config(config, paste0("Raw_", d), enabled = TRUE)
    }
    # Set temporal config
    config <- set_temporal_config(config, snapshot_count = months_duration, snapshot_width = "months")
    # Generate raw data using new helper
    raw_data <- generate_raw_data_for_endpoints(config, domain_pkg_df)
    # Create study object
    study <- create_longitudinal_study_data(
      study_id = study_id,
      raw_data = raw_data,
      config = list(
        participants = participants,
        sites = sites,
        snapshots = months_duration,
        interval = "1 month",
        domains = domains,
        study_type = "endpoints",
        analytics_package = "gsm.endpoints",
        verbose = verbose
      )
    )
    # Run analytics if requested
    if (run_analytics) {
      if (isTRUE(verbose)) cat("Running analytics pipeline...\n")
      analytics_config <- create_study_config(
        study_id = study_id,
        participant_count = participants,
        site_count = sites,
        analytics_package = "gsm.endpoints"
      )
      analytics_config$domains <- domains
      analytics_config$verbose <- verbose
      study$analytics <- generate_analytics_layers(
        raw_data = raw_data,
        config = analytics_config,
        verbose = verbose
      )
    }
  } else {
    stop("study_type must be 'standard' or 'endpoints'")
  }

  if (isTRUE(verbose)) {
    cat("Study creation completed successfully!\n")
    cat("Generated", length(study$raw_data), "snapshots with", length(study$config$domains), "domains\n")
  }

  if (isTRUE(verbose) && !is.null(study$analytics)) {
    total_metrics <- 0
    analytics_snapshots <- study$analytics
    if (is.list(analytics_snapshots) && length(analytics_snapshots) > 0) {
      # Count metrics directly from raw analytics results
      if ("results" %in% names(analytics_snapshots)) {
        # Single snapshot
        results <- analytics_snapshots$results
        total_metrics <- length(results[grep("^(Analysis_|site|country|study)", names(results), ignore.case = TRUE)])
      } else {
        # Multiple snapshots
        for (snapshot_name in names(analytics_snapshots)) {
          snapshot_analytics <- analytics_snapshots[[snapshot_name]]
          if (!is.null(snapshot_analytics) && "results" %in% names(snapshot_analytics)) {
            results <- snapshot_analytics$results
            snapshot_metrics <- length(results[grep("^(Analysis_|site|country|study)", names(results), ignore.case = TRUE)])
            total_metrics <- total_metrics + snapshot_metrics
          }
        }
      }
    }
    cat("Analytics pipeline completed with results for",
        total_metrics, "metrics\n")
  }

  return(study)
}
