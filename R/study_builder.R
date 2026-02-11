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
    #' @param domain_name Domain mapping name (e.g., "AE", "LB")
    get_domain_timeline = function(domain_name) {
      raw_name <- paste0("Raw_", domain_name)

      timeline_data <- list()
      for (i in seq_along(self$raw_data)) {
        if (raw_name %in% names(self$raw_data[[i]])) {
          timeline_data[[paste0("timepoint_", i)]] <- self$raw_data[[i]][[raw_name]]
        }
      }

      return(timeline_data)
    },

    #' Get available domain names
    #' @description Return list of available domain names across all timepoints
    get_domain_names = function() {
      if (length(self$raw_data) > 0) {
        return(unique(unlist(lapply(self$raw_data, names))))
      }
      return(character(0))
    }
  )
)
