#' Study Configuration Class
#'
#' Manages study parameters, temporal configuration, and dataset specifications
#' for clinical trial data generation.
#'
#' @importFrom R6 R6Class
#' @export
StudyConfig <- R6::R6Class("StudyConfig",
  public = list(
    #' @field study_params List of basic study parameters
    study_params = NULL,
    #' @field temporal_config List of temporal configuration settings
    temporal_config = NULL,
    #' @field dataset_configs List of dataset configurations
    dataset_configs = NULL,

    #' Initialize a new StudyConfig
    #' @param study_id Study identifier
    #' @param participant_count Number of participants
    #' @param site_count Number of sites
    initialize = function(study_id = "STUDY001", participant_count = 100, site_count = 10) {
      self$study_params <- list(
        study_id = study_id,
        participant_count = participant_count,
        site_count = site_count
      )

      self$temporal_config <- list(
        start_date = as.Date("2023-01-01"),
        snapshot_count = 5,
        snapshot_width = "months",
        end_date = NULL
      )

      self$dataset_configs <- list()

      # Add default required datasets
      self$add_dataset("Raw_STUDY", enabled = TRUE, count_formula = 1)
      self$add_dataset("Raw_SITE", enabled = TRUE, count_formula = function(config) config$study_params$site_count)
      self$add_dataset("Raw_SUBJ", enabled = TRUE, count_formula = function(config) config$study_params$participant_count)
      self$add_dataset("Raw_ENROLL", enabled = TRUE, count_formula = function(config) config$study_params$participant_count)
    },

    #' Set temporal configuration
    #' @param start_date Study start date
    #' @param snapshot_count Number of snapshots
    #' @param snapshot_width Time between snapshots
    #' @param end_date Study end date
    set_temporal = function(start_date = NULL, snapshot_count = NULL, snapshot_width = NULL, end_date = NULL) {
      if (!is.null(start_date)) self$temporal_config$start_date <- as.Date(start_date)
      if (!is.null(snapshot_count)) self$temporal_config$snapshot_count <- snapshot_count
      if (!is.null(snapshot_width)) self$temporal_config$snapshot_width <- snapshot_width
      if (!is.null(end_date)) {
        tryCatch({
          self$temporal_config$end_date <- as.Date(end_date)
        }, error = function(e) {
          # Skip setting end_date if conversion fails
          NULL
        })
      }

      invisible(self)
    },

    #' Add a dataset configuration
    #' @param dataset_type Type of dataset (e.g., "Raw_AE")
    #' @param enabled Whether the dataset should be generated
    #' @param count_formula Formula for calculating record count
    #' @param growth_pattern How the dataset grows over time
    #' @param dependencies Dependencies on other datasets
    #' @param custom_args Additional arguments for the dataset generator
    add_dataset = function(dataset_type, enabled = TRUE, count_formula = NULL,
                          growth_pattern = "linear", dependencies = character(0),
                          custom_args = list()) {
      self$dataset_configs[[dataset_type]] <- list(
        enabled = enabled,
        count_formula = count_formula,
        growth_pattern = growth_pattern,
        dependencies = dependencies,
        custom_args = custom_args
      )

      invisible(self)
    },

    #' Remove a dataset configuration
    #' @param dataset_type Type of dataset to remove
    remove_dataset = function(dataset_type) {
      self$dataset_configs[[dataset_type]] <- NULL
      invisible(self)
    },

    #' Validate the configuration
    #' @description Basic validation of study parameters
    validate = function() {
      # Basic checks only
      if (self$temporal_config$snapshot_count < 1) {
        stop("snapshot_count must be at least 1")
      }

      if (self$study_params$participant_count < 1) {
        stop("participant_count must be at least 1")
      }

      if (self$study_params$site_count < 1) {
        stop("site_count must be at least 1")
      }

      invisible(TRUE)
    }
  )
)

#' Fluent Builder Interface for Clinical Study Data Generation
#'
#' A fluent, chainable interface that makes dataset generation more intuitive
#' and discoverable through method chaining.
#'
#' @export
StudyBuilder <- R6::R6Class("StudyBuilder",
  public = list(
    #' @field config Internal StudyConfig object
    config = NULL,

    #' Initialize a new StudyBuilder
    #' @param study_id Study identifier
    initialize = function(study_id = "STUDY001") {
      self$config <- StudyConfig$new(study_id = study_id)
    },

    #' Configure basic study parameters
    #' @param participants Number of study participants
    #' @param sites Number of study sites
    #' @param study_id Study identifier (if different from initialization)
    with_study_design = function(participants = NULL, sites = NULL, study_id = NULL) {
      if (!is.null(participants)) self$config$study_params$participant_count <- participants
      if (!is.null(sites)) self$config$study_params$site_count <- sites
      if (!is.null(study_id)) self$config$study_params$study_id <- study_id

      invisible(self)
    },

    #' Configure temporal aspects of the study
    #' @param start_date Study start date
    #' @param snapshots Number of snapshots to generate
    #' @param frequency Time between snapshots (e.g., "months", "2 weeks")
    #' @param end_date Optional explicit end date
    over_time = function(start_date = NULL, snapshots = NULL, frequency = NULL, end_date = NULL) {
      self$config$set_temporal(
        start_date = start_date,
        snapshot_count = snapshots,
        snapshot_width = frequency,
        end_date = end_date
      )

      invisible(self)
    },

    #' Include standard clinical datasets with default configurations
    #' @param adverse_events Include adverse event data
    #' @param protocol_deviations Include protocol deviation data
    #' @param lab_data Include laboratory data
    #' @param visits Include visit data
    #' @param enrollment Include enrollment data
    #' @param data_quality Include data quality domains (DATACHG, DATAENT, QUERY)
    #' @param pharmacokinetics Include pharmacokinetics data
    #' @param study_completion Include study completion data
    #' @param inclusion_exclusion Include inclusion/exclusion criteria
    #' @param country Include country mapping
    with_standard_datasets = function(adverse_events = TRUE,
                                    protocol_deviations = TRUE,
                                    lab_data = TRUE,
                                    visits = TRUE,
                                    enrollment = TRUE,
                                    data_quality = TRUE,
                                    pharmacokinetics = TRUE,
                                    study_completion = TRUE,
                                    inclusion_exclusion = TRUE,
                                    country = TRUE) {

      # Safely evaluate logical parameters
      adverse_events <- tryCatch(isTRUE(adverse_events), error = function(e) TRUE)
      protocol_deviations <- tryCatch(isTRUE(protocol_deviations), error = function(e) TRUE)
      lab_data <- tryCatch(isTRUE(lab_data), error = function(e) TRUE)
      visits <- tryCatch(isTRUE(visits), error = function(e) TRUE)
      enrollment <- tryCatch(isTRUE(enrollment), error = function(e) TRUE)
      data_quality <- tryCatch(isTRUE(data_quality), error = function(e) TRUE)
      pharmacokinetics <- tryCatch(isTRUE(pharmacokinetics), error = function(e) TRUE)
      study_completion <- tryCatch(isTRUE(study_completion), error = function(e) TRUE)
      inclusion_exclusion <- tryCatch(isTRUE(inclusion_exclusion), error = function(e) TRUE)
      country <- tryCatch(isTRUE(country), error = function(e) TRUE)

      # Core datasets are always included automatically in StudyConfig

      if (adverse_events) {
        self$config$add_dataset("Raw_AE", enabled = TRUE)
      }

      if (protocol_deviations) {
        self$config$add_dataset("Raw_PD", enabled = TRUE)
      }

      if (lab_data) {
        self$config$add_dataset("Raw_LB", enabled = TRUE)
      }

      if (visits) {
        self$config$add_dataset("Raw_SV", enabled = TRUE)
        self$config$add_dataset("Raw_VISIT", enabled = TRUE)
      }

      if (enrollment) {
        self$config$add_dataset("Raw_ENROLL", enabled = TRUE)
      }

      if (data_quality) {
        self$config$add_dataset("Raw_DATACHG", enabled = TRUE)
        self$config$add_dataset("Raw_DATAENT", enabled = TRUE)
        self$config$add_dataset("Raw_QUERY", enabled = TRUE)
      }

      if (pharmacokinetics) {
        self$config$add_dataset("Raw_PK", enabled = TRUE)
      }

      if (study_completion) {
        self$config$add_dataset("Raw_STUDCOMP", enabled = TRUE)
        self$config$add_dataset("Raw_SDRGCOMP", enabled = TRUE)
      }

      if (inclusion_exclusion) {
        self$config$add_dataset("Raw_IE", enabled = TRUE)
        self$config$add_dataset("Raw_EXCLUSION", enabled = TRUE)
      }

      if (country) {
        self$config$add_dataset("Raw_COUNTRY", enabled = TRUE)
      }

      invisible(self)
    },

    #' Add custom dataset with flexible configuration
    #' @param dataset_type Type of dataset (e.g., "Raw_CustomDomain")
    #' @param count Count formula (numeric, function, or expression)
    #' @param depends_on Dependencies on other datasets
    #' @param growth How the dataset grows over time ("linear", "exponential", "constant")
    #' @param ... Additional arguments for the dataset generator
    add_custom_dataset = function(dataset_type,
                                 count = NULL,
                                 depends_on = character(0),
                                 growth = "linear",
                                 ...) {

      self$config$add_dataset(
        dataset_type = dataset_type,
        enabled = TRUE,
        count_formula = count,
        growth_pattern = growth,
        dependencies = depends_on,
        custom_args = list(...)
      )

      invisible(self)
    },

    #' Configure adverse events with specific parameters
    #' @param rate_per_patient Average AEs per patient
    #' @param severity_distribution Distribution of severity levels
    #' @param temporal_pattern How AEs occur over time
    with_adverse_events = function(rate_per_patient = 3,
                                  severity_distribution = c(mild = 0.6, moderate = 0.3, severe = 0.1),
                                  temporal_pattern = "increasing") {

      count_formula <- function(config, snapshot_idx = 1) {
        base_count <- config$study_params$participant_count * rate_per_patient

        # Apply temporal pattern
        factor <- switch(temporal_pattern,
          "constant" = 1,
          "increasing" = snapshot_idx / config$temporal_config$snapshot_count,
          "decreasing" = (config$temporal_config$snapshot_count - snapshot_idx + 1) / config$temporal_config$snapshot_count,
          1  # default
        )

        round(base_count * factor)
      }

      self$config$add_dataset(
        dataset_type = "Raw_AE",
        enabled = TRUE,
        count_formula = count_formula,
        custom_args = list(
          severity_distribution = severity_distribution,
          temporal_pattern = temporal_pattern
        )
      )

      invisible(self)
    },

    #' Configure study completion patterns
    #' @param completion_rate Overall study completion rate (0-1)
    #' @param dropout_pattern Pattern of dropouts ("early", "late", "uniform")
    with_study_completion = function(completion_rate = 0.85, dropout_pattern = "uniform") {

      # Enable study completion datasets
      self$config$add_dataset("Raw_STUDCOMP",
                             enabled = TRUE,
                             count_formula = function(config) {
                               ceiling(config$study_params$participant_count * (1 - completion_rate))
                             },
                             custom_args = list(
                               completion_rate = completion_rate,
                               dropout_pattern = dropout_pattern
                             ))

      invisible(self)
    },

    #' Include data quality datasets
    #' @param queries Include query data
    #' @param data_changes Include data change tracking
    #' @param data_entry Include data entry tracking
    with_data_quality = function(queries = TRUE, data_changes = TRUE, data_entry = TRUE) {

      if (queries) {
        self$config$add_dataset("Raw_QUERY", enabled = TRUE)
      }

      if (data_changes) {
        self$config$add_dataset("Raw_DATACHG", enabled = TRUE)
      }

      if (data_entry) {
        self$config$add_dataset("Raw_DATAENT", enabled = TRUE)
      }

      invisible(self)
    },

    #' Preview the configuration without generating data
    #' @description Display study configuration summary
    preview = function() {
      cat("Study Configuration Preview\n")
      cat("==========================\n\n")

      # Study parameters  
      cat("Study Parameters:\n")
      study_id <- tryCatch(as.character(self$config$study_params$study_id), error = function(e) "Unknown")
      participant_count <- tryCatch(as.numeric(self$config$study_params$participant_count), error = function(e) 0)
      site_count <- tryCatch(as.numeric(self$config$study_params$site_count), error = function(e) 0)
      
      cat(sprintf("  Study ID: %s\n", study_id))
      cat(sprintf("  Participants: %d\n", participant_count))
      cat(sprintf("  Sites: %d\n", site_count))
      cat("\n")

      # Temporal configuration
      cat("Temporal Configuration:\n")
      cat(sprintf("  Start Date: %s\n", self$config$temporal_config$start_date))
      cat(sprintf("  Snapshots: %d\n", self$config$temporal_config$snapshot_count))
      cat(sprintf("  Frequency: %s\n", self$config$temporal_config$snapshot_width))
      cat("\n")

      # Dataset configuration
      cat("Enabled Datasets:\n")
      enabled_datasets <- names(self$config$dataset_configs)[
        sapply(self$config$dataset_configs, function(x) x$enabled)
      ]

      for (dataset in enabled_datasets) {
        config <- self$config$dataset_configs[[dataset]]
        cat(sprintf("  - %s", dataset))

        if (length(config$dependencies) > 0) {
          cat(sprintf(" (depends on: %s)", paste(config$dependencies, collapse = ", ")))
        }
        cat("\n")
      }

      invisible(self)
    },

    #' Generate the configured study data
    #' @param workflow_path Path to workflow mappings
    #' @param mappings Mapping specifications to use
    #' @param package Package containing the workflows
    generate = function(workflow_path = "workflow/1_mappings",
                       mappings = NULL,
                       package = "gsm.mapping") {

      # Validate configuration before generation
      self$config$validate()

      # Use the new generator with the configuration
      generate_study_data_with_config(
        config = self$config,
        workflow_path = workflow_path,
        mappings = mappings,
        package = package
      )
    }
  )
)

#' Create a new study builder
#' @param study_id Study identifier
#' @export
create_study <- function(study_id = "STUDY001") {
  StudyBuilder$new(study_id = study_id)
}

#' Example of fluent interface usage
#'
#' @name study_builder_examples
#' @examples
#' \dontrun{
#' # Simple study with standard datasets
#' study_data <- create_study("ONCOLOGY001") %>%
#'   with_study_design(participants = 200, sites = 15) %>%
#'   over_time(start_date = "2023-01-01", snapshots = 12, frequency = "months") %>%
#'   with_standard_datasets(adverse_events = TRUE, visits = TRUE) %>%
#'   with_adverse_events(rate_per_patient = 2.5, temporal_pattern = "increasing") %>%
#'   generate()
#'
#' # Preview configuration before generation
#' create_study("TRIAL002") %>%
#'   with_study_design(participants = 100, sites = 10) %>%
#'   with_standard_datasets() %>%
#'   preview()
#'
#' # Custom dataset configuration
#' study_data <- create_study("CUSTOM001") %>%
#'   with_study_design(participants = 300, sites = 20) %>%
#'   add_custom_dataset("Raw_Biomarker",
#'                     count = function(config) config$study_params$participant_count * 5,
#'                     depends_on = "Raw_SUBJ") %>%
#'   generate()
#' }
NULL

#' Generate study data using configuration object
#'
#' Internal function to generate study data based on StudyConfig settings
#'
#' @param config StudyConfig object containing all configuration
#' @param workflow_path Path to workflow mappings
#' @param mappings Mapping specifications to use
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
    # Remove "Raw_" prefix for mapping names
    mappings <- gsub("^Raw_", "", enabled_datasets)
    # Remove duplicates and ensure core mappings are included
    mappings <- unique(c("STUDY", "SITE", "SUBJ", "ENROLL", mappings))
  }

  # Generate the data using existing function
  generate_rawdata_for_single_study(
    SnapshotCount = config$temporal_config$snapshot_count,
    SnapshotWidth = config$temporal_config$snapshot_width,
    ParticipantCount = config$study_params$participant_count,
    SiteCount = config$study_params$site_count,
    StudyID = config$study_params$study_id,
    workflow_path = workflow_path,
    mappings = mappings,
    package = package
  )
}

#' Generate Longitudinal Clinical Trial Data with Integrated Workflow
#'
#' Creates a longitudinal study with multiple time points and optional integrated
#' pipeline processing. This function provides a clean, intuitive interface for
#' generating realistic clinical trial data over time.
#'
#' @param study_id Study identifier
#' @param participants Number of study participants
#' @param sites Number of study sites
#' @param timepoints Number of data collection timepoints
#' @param interval Time interval between collection points (e.g., "1 month", "2 weeks")
#' @param start_date Study start date
#' @param domains Clinical domains to include (e.g., c("AE", "LB", "VISIT"))
#' @param include_pipeline Whether to run full analytics pipeline (raw -> mapped -> metrics -> reports)
#' @param study_type Type of study - "standard" uses gsm.kri metrics, "endpoints" uses gsm.endpoints metrics
#' @return LongitudinalStudy object containing all generated data and results
#' @export
#' @examples
#' \dontrun{
#' # Simple longitudinal study
#' study <- create_longitudinal_study(
#'   study_id = "ONCOLOGY-001",
#'   participants = 200,
#'   sites = 15,
#'   timepoints = 6,
#'   interval = "2 months",
#'   domains = c("AE", "LB", "VISIT")
#' )
#'
#' # With full analytics pipeline
#' study_with_analytics <- create_longitudinal_study(
#'   study_id = "CARDIO-002",
#'   participants = 500,
#'   sites = 25,
#'   timepoints = 12,
#'   interval = "1 month",
#'   domains = c("AE", "LB", "VISIT", "QUERY"),
#'   include_pipeline = TRUE
#' )
#'
#' # Access results intuitively
#' raw_data <- study_with_analytics$raw_data
#' site_analytics <- study_with_analytics$analytics$by_site
#' country_analytics <- study_with_analytics$analytics$by_country
#' }
create_longitudinal_study <- function(study_id = "STUDY-001",
                                     participants = 100,
                                     sites = 10,
                                     timepoints = 5,
                                     interval = "1 month",
                                     start_date = Sys.Date() - 365,
                                     domains = c("AE", "LB", "VISIT"),
                                     include_pipeline = FALSE,
                                     study_type = "standard") {

  # Validate inputs
  validate_study_inputs(participants, sites, timepoints, domains)

  # Ensure core mappings are included
  core_mappings <- c("STUDY", "SITE", "SUBJ", "ENROLL")
  mappings <- unique(c(core_mappings, domains))

  # Generate core study structure
  message(sprintf("Creating longitudinal study '%s' with %d participants across %d sites over %d timepoints",
                 study_id, participants, sites, timepoints))

  raw_data <- generate_study_snapshots(
    study_id = study_id,
    participants = participants,
    sites = sites,
    timepoints = timepoints,
    interval = interval,
    mappings = mappings
  )

  # Create study object
  study <- LongitudinalStudy$new(
    study_id = study_id,
    raw_data = raw_data,
    config = list(
      participants = participants,
      sites = sites,
      timepoints = timepoints,
      interval = interval,
      domains = domains,
      start_date = start_date,
      study_type = study_type
    )
  )

  # Run analytics pipeline if requested
  if (include_pipeline) {
    message("Running integrated analytics pipeline...")
    study$run_analytics_pipeline()
  }

  message("Study generation complete!")
  return(study)
}

#' Longitudinal Study Data Container
#'
#' R6 class that encapsulates longitudinal study data and provides intuitive
#' access methods for different analysis perspectives.
#'
#' @export
LongitudinalStudy <- R6::R6Class("LongitudinalStudy",
  public = list(
    #' @field study_id Study identifier
    study_id = NULL,
    #' @field raw_data List of raw data snapshots
    raw_data = NULL,
    #' @field config Study configuration parameters
    config = NULL,
    #' @field analytics Processed analytics results
    analytics = NULL,

    #' Initialize new LongitudinalStudy
    #' @param study_id Study identifier
    #' @param raw_data Raw study data snapshots
    #' @param config Configuration parameters
    initialize = function(study_id, raw_data, config) {
      self$study_id <- study_id
      self$raw_data <- raw_data
      self$config <- config
      self$analytics <- NULL
    },

    #' Get summary of study structure
    #' @description Display comprehensive study summary
    summary = function() {
      cat("Longitudinal Study Summary\n")
      cat("=========================\n")
      cat(sprintf("Study ID: %s\n", self$study_id))
      cat(sprintf("Participants: %d\n", self$config$participants))
      cat(sprintf("Sites: %d\n", self$config$sites))
      cat(sprintf("Timepoints: %d\n", self$config$timepoints))
      cat(sprintf("Interval: %s\n", self$config$interval))
      cat(sprintf("Domains: %s\n", paste(self$config$domains, collapse = ", ")))
      cat(sprintf("Analytics Available: %s\n", !is.null(self$analytics)))

      if (length(self$raw_data) > 0) {
        cat(sprintf("\nData Snapshots: %d\n", length(self$raw_data)))
        cat("Available datasets per snapshot:\n")
        for (i in seq_along(self$raw_data)[1:min(3, length(self$raw_data))]) {
          cat(sprintf("  Timepoint %d: %s\n", i, paste(names(self$raw_data[[i]]), collapse = ", ")))
        }
        if (length(self$raw_data) > 3) {
          cat("  ...\n")
        }
      }
    },

    #' Run the complete analytics pipeline
    #' @description Execute full analytics pipeline on study data
    run_analytics_pipeline = function() {
      pipeline_results <- execute_analytics_pipeline(self$raw_data, self$config)
      self$analytics <- organize_analytics_results(pipeline_results)

      invisible(self)
    },

    #' Get data for specific timepoint
    #' @param timepoint Timepoint number (1-based)
    get_timepoint = function(timepoint) {
      if (timepoint < 1 || timepoint > length(self$raw_data)) {
        stop(sprintf("Timepoint %d not available. Study has %d timepoints.", timepoint, length(self$raw_data)))
      }
      return(self$raw_data[[timepoint]])
    },

    #' Get specific domain data across all timepoints
    #' @param domain Domain mapping name (e.g., "AE", "LB")
    get_domain_timeline = function(domain) {
      raw_name <- paste0("Raw_", domain)

      timeline_data <- list()
      for (i in seq_along(self$raw_data)) {
        if (raw_name %in% names(self$raw_data[[i]])) {
          timeline_data[[paste0("timepoint_", i)]] <- self$raw_data[[i]][[raw_name]]
        }
      }

      return(timeline_data)
    }
  )
)

#' Validate study input parameters
#' @param participants Number of participants
#' @param sites Number of sites
#' @param timepoints Number of timepoints
#' @param domains Domain specifications
#' @export
validate_study_inputs <- function(participants, sites, timepoints, domains) {
  if (participants < 1) stop("participants must be at least 1")
  if (sites < 1) stop("sites must be at least 1")
  if (timepoints < 1) stop("timepoints must be at least 1")
}

#' Ensure core mappings are included with domains
#' @param domains Vector of domain mapping names
#' @return Vector of mapping names including required core mappings
#' @export
ensure_core_mappings <- function(domains) {
  # Always include core mappings
  core_mappings <- c("STUDY", "SITE", "SUBJ", "ENROLL")
  unique(c(core_mappings, domains))
}

#' Generate study data snapshots
#' @param study_id Study identifier
#' @param participants Number of participants
#' @param sites Number of sites
#' @param timepoints Number of timepoints
#' @param interval Time interval
#' @param mappings Technical mapping names
#' @return List of raw data snapshots
#' @export
generate_study_snapshots <- function(study_id, participants, sites, timepoints, interval, mappings) {
  # Convert interval to snapshot width format expected by existing function
  snapshot_width <- parse_interval_to_snapshot_width(interval)

  generate_rawdata_for_single_study(
    SnapshotCount = timepoints,
    SnapshotWidth = snapshot_width,
    ParticipantCount = participants,
    SiteCount = sites,
    StudyID = study_id,
    workflow_path = "workflow/1_mappings",
    mappings = mappings,
    package = "gsm.mapping"
  )
}

#' Parse user-friendly interval to snapshot width format
#' @param interval User-friendly interval (e.g., "1 month", "2 weeks")
#' @return Snapshot width format for existing function
#' @export
parse_interval_to_snapshot_width <- function(interval) {
  # Simple parsing - can be enhanced later
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

#' Execute the analytics pipeline
#' @param raw_data List of raw data snapshots
#' @param config Study configuration
#' @return Pipeline results
#' @export
execute_analytics_pipeline <- function(raw_data, config) {
  # Get all unique mappings from the raw data
  all_mappings <- unique(unlist(lapply(raw_data, function(snapshot) {
    gsub("^Raw_", "", names(snapshot))
  })))

  # Setup workflow specifications
  mappings_wf <- gsm.core::MakeWorkflowList(
    strNames = all_mappings,
    strPath = "workflow/1_mappings",
    strPackage = "gsm.mapping"
  )
  mappings_spec <- gsm.mapping::CombineSpecs(mappings_wf)

  # Setup workflow specifications based on study type
  metrics_wf <- if (config$study_type == "standard") {
    gsm.core::MakeWorkflowList(strPath = "workflow/2_metrics", strPackage = "gsm.kri")
  } else if (config$study_type == "endpoints") {
    gsm.core::MakeWorkflowList(strPath = "workflow/2_metrics", strPackage = "gsm.endpoints")
  } else {
    # Default to gsm.kri for unrecognized study types
    gsm.core::MakeWorkflowList(strPath = "workflow/2_metrics", strPackage = "gsm.kri")
  }

  reporting_wf <- gsm.core::MakeWorkflowList(strPath = "workflow/3_reporting", strPackage = "gsm.reporting")

  # Process each timepoint through the pipeline
  timepoint_results <- list()

  for (i in seq_along(raw_data)) {
    # Ensure site status is set
    if ("Raw_SITE" %in% names(raw_data[[i]]) && !"site_status" %in% names(raw_data[[i]]$Raw_SITE)) {
      raw_data[[i]]$Raw_SITE$site_status <- "Active"
    }

    # Pipeline: raw -> mapped -> metrics -> reporting
    lRaw <- gsm.mapping::Ingest(raw_data[[i]], mappings_spec)
    mapped <- gsm.core::RunWorkflows(mappings_wf, lRaw)
    analyzed <- gsm.core::RunWorkflows(metrics_wf, c(mapped, list(lWorkflows = metrics_wf)))
    reporting <- gsm.core::RunWorkflows(reporting_wf, c(mapped, list(lAnalyzed = analyzed, lWorkflows = metrics_wf)))

    # Add timepoint information
    snapshot_date <- seq(as.Date("2025-02-01"), length.out = length(raw_data), by = "month")[i]
    reporting$Reporting_Results$SnapshotDate <- snapshot_date
    reporting$Reporting_Bounds$SnapshotDate <- snapshot_date

    timepoint_results[[i]] <- list(
      raw = raw_data[[i]],
      mapped = mapped,
      analyzed = analyzed,
      reporting = reporting
    )
  }

  return(timepoint_results)
}

#' Organize analytics results into intuitive structure
#' @param pipeline_results Results from pipeline execution
#' @return Organized analytics results
#' @export
organize_analytics_results <- function(pipeline_results) {
  # Combine results across timepoints
  all_results <- do.call(dplyr::bind_rows, lapply(pipeline_results, function(x) x$reporting$Reporting_Results))
  all_bounds <- do.call(dplyr::bind_rows, lapply(pipeline_results, function(x) x$reporting$Reporting_Bounds))

  # Use last timepoint for groups and metrics (they don't change across time)
  last_timepoint <- pipeline_results[[length(pipeline_results)]]
  all_groups <- last_timepoint$reporting$Reporting_Groups
  all_metrics <- last_timepoint$reporting$Reporting_Metrics

  # Organize by analysis level for intuitive access
  list(
    by_site = list(
      results = all_results %>% dplyr::filter(GroupLevel == "Site"),
      groups = all_groups %>% dplyr::filter(GroupLevel %in% c("Study", "Site")),
      bounds = all_bounds %>% dplyr::filter(stringr::str_detect(MetricID, "Analysis_kri")),
      metrics = all_metrics %>% dplyr::filter(GroupLevel == "Site")
    ),
    by_country = list(
      results = all_results %>% dplyr::filter(GroupLevel == "Country"),
      groups = all_groups %>% dplyr::filter(GroupLevel %in% c("Study", "Country")),
      bounds = all_bounds %>% dplyr::filter(stringr::str_detect(MetricID, "Analysis_cou")),
      metrics = all_metrics %>% dplyr::filter(GroupLevel == "Country")
    ),
    by_study = list(
      results = all_results %>% dplyr::filter(GroupLevel == "Study"),
      groups = all_groups %>% dplyr::filter(GroupLevel == "Study"),
      bounds = all_bounds %>% dplyr::filter(stringr::str_detect(MetricID, "Analysis_qtl")),
      metrics = all_metrics %>% dplyr::filter(GroupLevel == "Study")
    ),
    combined = list(
      all_results = all_results,
      all_groups = all_groups,
      all_bounds = all_bounds,
      all_metrics = all_metrics
    )
  )
}

#' Generate Complete Longitudinal Study with Full Pipeline Processing
#'
#' This function directly addresses the workflow shown in issue #95 by providing
#' a single function call to replace the complex manual orchestration of multiple
#' packages and workflow steps.
#'
#' @param participant_count Number of study participants (default: 1000)
#' @param site_count Number of study sites (default: 150)
#' @param study_id Study identifier (default: "AA-AA-000-0000")
#' @param snapshot_count Number of time points (default: 3)
#' @param snapshot_width Time between snapshots (default: "months")
#' @param core_mappings Vector of core mapping types to include
#' @param start_date Study start date
#' @return List containing all pipeline results equivalent to manual workflow
#' @export
#' @examples
#' \dontrun{
#' # Direct replacement for the manual workflow in issue #95
#' results <- generate_complete_longitudinal_study(
#'   participant_count = 1000,
#'   site_count = 150,
#'   study_id = "AA-AA-000-0000",
#'   snapshot_count = 3,
#'   snapshot_width = "months"
#' )
#'
#' # Access results similar to manual workflow
#' site_results <- results$reporting_site
#' country_results <- results$reporting_country
#' study_results <- results$reporting_study
#' }
generate_complete_longitudinal_study <- function(participant_count = 1000,
                                                site_count = 150,
                                                study_id = "AA-AA-000-0000",
                                                snapshot_count = 3,
                                                snapshot_width = "months",
                                                start_date = as.Date("2025-02-01"),
                                                core_mappings = c("AE", "COUNTRY", "DATACHG", "DATAENT",
                                                                "ENROLL", "LB", "VISIT", "PD", "PK", "QUERY",
                                                                "STUDY", "STUDCOMP", "SDRGCOMP", "SITE", "SUBJ",
                                                                "IE", "EXCLUSION")) {

  message("Generating longitudinal study data with integrated pipeline...")
  message(sprintf("Study: %s, Participants: %d, Sites: %d, Snapshots: %d",
                 study_id, participant_count, site_count, snapshot_count))

  # Generate the complete longitudinal study with full pipeline
  results <- generate_longitudinal_study(
    study_id = study_id,
    participant_count = participant_count,
    site_count = site_count,
    snapshot_count = snapshot_count,
    snapshot_width = snapshot_width,
    start_date = start_date,
    datasets = core_mappings,
    run_full_pipeline = TRUE
  )

  message("Pipeline complete. Results available for site, country, and study levels.")

  return(results)
}

#' Quick Start: Generate Complete Study with Full Analytics
#'
#' One-liner function to generate a complete longitudinal study with full analytics
#' pipeline. This directly replaces the complex manual workflow from issue #95.
#'
#' @param study_name Human-readable study name (automatically formats study ID)
#' @param participants Number of participants (default: 1000)
#' @param sites Number of sites (default: 150)
#' @param months_duration Study duration in months (default: 3)
#' @param study_type Type of study - "standard" for basic clinical domains or "endpoints" for comprehensive endpoint analysis
#' @param domains Clinical domains to include (overrides study_type if specified)
#' @return LongitudinalStudy object with complete analytics
#' @export
#' @examples
#' \dontrun{
#' # Standard clinical trial
#' study <- quick_longitudinal_study("Oncology Phase III", study_type = "standard")
#'
#' # Comprehensive endpoints study
#' study <- quick_longitudinal_study("Cardio Endpoints", study_type = "endpoints")
#'
#' # Custom domains (overrides study_type)
#' study <- quick_longitudinal_study("Custom Trial",
#'                                  domains = c("AE", "LB"))
#' }
quick_longitudinal_study <- function(study_name = "Clinical Trial",
                                    participants = 1000,
                                    sites = 150,
                                    months_duration = 3,
                                    study_type = "standard",
                                    domains = NULL) {

  # Define domain sets based on study type
  if (is.null(domains)) {
    domains <- switch(study_type,
      "standard" = c(
        "AE", "COUNTRY", "DATACHG", "DATAENT", "ENROLL", "LB",
        "VISIT", "PD", "PK", "QUERY", "STUDCOMP", "SDRGCOMP", "IE", "EXCLUSION"
      ),
      "endpoints" = c(
        "AntiCancer", "Baseline", "Death", "OverallResponse", "Randomization",
        "STUDCOMP", "VISIT", "Consents", "STUDY", "SUBJ", "AE", "PD", "LB"
      ),
      # Default to standard if unrecognized type
      c("AE", "COUNTRY", "DATACHG", "DATAENT", "ENROLL", "LB",
        "VISIT", "PD", "PK", "QUERY", "STUDCOMP", "SDRGCOMP", "IE", "EXCLUSION")
    )
  }

  # Auto-generate study ID from name
  study_id <- gsub("[^A-Za-z0-9]", "-", study_name)
  study_id <- gsub("-+", "-", study_id)
  study_id <- gsub("^-|-$", "", study_id)
  study_id <- toupper(paste0(study_id, "-", sprintf("%03d", sample(100:999, 1))))

  message(sprintf("Quick start: Generating '%s' [%s] (%s study)",
                 study_name, study_id, study_type))
  message(sprintf("Domains: %s", paste(domains, collapse = ", ")))

  create_longitudinal_study(
    study_id = study_id,
    participants = participants,
    sites = sites,
    timepoints = months_duration,
    interval = "1 month",
    domains = domains,
    include_pipeline = TRUE,
    study_type = study_type
  )
}
