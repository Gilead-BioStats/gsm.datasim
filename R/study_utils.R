#' Generate raw data for endpoints study (multi-package)
#'
#' @param config Study configuration object with enabled datasets
#' @param domain_package_df Data frame mapping domains to packages
#' @return List of raw data for enabled datasets
#' @export
generate_raw_data_for_endpoints <- function(config, domain_package_df) {
  generate_snapshots_from_config(
    config = config,
    domain_package_df = domain_package_df,
    default_package = "gsm.mapping",
    workflow_path = "workflow/1_mappings",
    verbose = FALSE
  )
}

# WP1 core helper: resolve enabled mapping names from study config
get_enabled_mapping_names <- function(config) {
  enabled_datasets <- names(config$dataset_configs)[
    sapply(config$dataset_configs, function(x) x$enabled)
  ]
  gsub("^Raw_", "", enabled_datasets)
}

# WP1 core helper: resolve domain->package data frame for enabled mappings
resolve_domain_package_df <- function(config, domain_package_df = NULL, default_package = "gsm.mapping") {
  mapping_names <- get_enabled_mapping_names(config)

  if (is.null(domain_package_df)) {
    return(data.frame(
      domain = mapping_names,
      package = rep(default_package, length(mapping_names)),
      stringsAsFactors = FALSE
    ))
  }

  resolved <- domain_package_df[match(mapping_names, domain_package_df$domain), , drop = FALSE]
  if (nrow(resolved) == 0) {
    resolved <- data.frame(domain = character(0), package = character(0), stringsAsFactors = FALSE)
  }

  missing_idx <- is.na(resolved$package)
  if (any(missing_idx)) {
    resolved$domain[missing_idx] <- mapping_names[missing_idx]
    resolved$package[missing_idx] <- default_package
  }

  resolved <- resolved[!duplicated(resolved$domain), , drop = FALSE]
  resolved
}

# WP1 core helper: generate snapshots from config (single- and multi-package)
generate_snapshots_from_config <- function(config,
                                           domain_package_df = NULL,
                                           default_package = "gsm.mapping",
                                           workflow_path = "workflow/1_mappings",
                                           verbose = FALSE) {
  domain_pkgs <- resolve_domain_package_df(config, domain_package_df, default_package)
  if (nrow(domain_pkgs) == 0) {
    return(list())
  }

  pkgs <- unique(domain_pkgs$package)
  raw_by_package <- list()

  for (pkg in pkgs) {
    domains_in_pkg <- domain_pkgs$domain[domain_pkgs$package == pkg]
    if (length(domains_in_pkg) == 0) next

    if (isTRUE(verbose)) {
      cat("Generating raw data for package", pkg, "with", length(domains_in_pkg), "domains...\n")
    }

    combined_specs <- load_specs(
      workflow_path = workflow_path,
      mappings = domains_in_pkg,
      package = pkg
    )
    prepared_specs <- prepare_combined_specs_for_generation(combined_specs)

    raw_by_package[[pkg]] <- generate_snapshots_from_combined_specs(
      SnapshotCount = config$temporal_config$snapshot_count,
      SnapshotWidth = config$temporal_config$snapshot_width,
      ParticipantCount = config$study_params$participant_count,
      SiteCount = config$study_params$site_count,
      StudyID = config$study_params$study_id,
      combined_specs = prepared_specs,
      mappings = domains_in_pkg,
      strStartDate = as.character(config$temporal_config$start_date)
    )
  }

  if (length(raw_by_package) == 0) {
    return(list())
  }

  if (length(raw_by_package) == 1) {
    return(raw_by_package[[1]])
  }

  first_pkg <- names(raw_by_package)[1]
  n_snapshots <- length(raw_by_package[[first_pkg]])
  all_raw_data <- vector("list", n_snapshots)

  for (i in seq_len(n_snapshots)) {
    merged_snapshot <- list()
    for (pkg in names(raw_by_package)) {
      snapshot_i <- raw_by_package[[pkg]][[i]]
      merged_snapshot <- c(merged_snapshot, snapshot_i)
    }
    all_raw_data[[i]] <- merged_snapshot
  }

  names(all_raw_data) <- names(raw_by_package[[first_pkg]])
  all_raw_data
}
#' Study Utility Functions
#'
#' This file contains utility functions for study validation, data generation,
#' and analytics pipeline execution.

#' Validate study input parameters
#'
#' @param participants Number of participants
#' @param sites Number of sites
#' @param snapshots Number of snapshots
#' @param domains Domains to include
#' @export
validate_study_inputs <- function(participants, sites, snapshots, domains) {
  if (participants <= 0) stop("Participants must be positive")
  if (sites <= 0) stop("Sites must be positive")
  if (snapshots <= 0) stop("Snapshots must be positive")
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

#' Generate study data across multiple snapshots
#'
#' @param study_id Study identifier
#' @param participants Number of participants
#' @param sites Number of sites
#' @param snapshots Number of snapshots
#' @param interval Time interval between snapshots
#' @param mappings Vector of mapping names to use
#' @param verbose Whether to print progress/output messages
#' @return List of raw data for each snapshot
#' @export
generate_study_snapshots <- function(study_id, participants, sites, snapshots, interval, mappings, verbose = FALSE) {
  snapshot_width <- parse_interval_to_snapshot_width(interval)

  # Calculate start dates for each snapshot
  base_date <- as.Date("2012-01-31")
  if (snapshot_width == "months") {
    base_month_start <- as.Date(format(base_date, "%Y-%m-01"))
    month_starts <- seq(base_month_start, length.out = snapshots, by = "month")
    start_dates <- as.Date(format(month_starts + 31, "%Y-%m-01")) - 1
  } else {
    start_dates <- seq(base_date, length.out = snapshots, by = snapshot_width)
  }

  raw_data_list <- list()

  for (i in 1:snapshots) {
    if (isTRUE(verbose)) cat("Generating snapshot", i, "of", snapshots, "\n")

    config <- create_study_config(
      study_id = study_id,
      participant_count = participants,
      site_count = sites
    )

    # Set temporal configuration with the specific start date for this snapshot
    config <- set_temporal_config(
      config,
      start_date = start_dates[i],
      snapshot_count = 1,
      snapshot_width = snapshot_width
    )

    # Add enabled datasets based on mappings
    for (mapping in mappings) {
      config <- add_dataset_config(config, mapping, enabled = TRUE)
    }
    config$verbose <- verbose

    raw_data <- generate_study_data(config, verbose = verbose)
    # Unwrap single-snapshot lists to avoid nested date keys
    if (is.list(raw_data) && length(raw_data) == 1 && is.list(raw_data[[1]])) {
      raw_data <- raw_data[[1]]
    }
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
#' @param config Study configuration object
#' @return Analytics pipeline results
#' @export
execute_analytics_pipeline <- function(raw_data, config) {
  verbose <- if (!is.null(config$verbose)) isTRUE(config$verbose) else TRUE
  vcat <- function(...) {
    if (isTRUE(verbose)) cat(...)
  }

  tryCatch({
    # Check if gsm.core is available
    if (!requireNamespace("gsm.core", quietly = TRUE)) {
      if (isTRUE(verbose)) message("gsm.core package not available. Skipping analytics pipeline.")
      return(NULL)
    }

    vcat("Running analytics pipeline on", length(raw_data), "snapshots...\n")

    # Determine workflow configuration once
    if (!is.null(config$study_params$analytics_package) && !is.null(config$study_params$analytics_workflows)) {
      lWorkflow <- gsm.core::MakeWorkflowList(
        strPackage = config$study_params$analytics_package,
        strNames = config$study_params$analytics_workflows
      )
    } else if (!is.null(config$study_params$analytics_package)) {
      lWorkflow <- gsm.core::MakeWorkflowList(
        strPackage = config$study_params$analytics_package
      )
    } else {
      lWorkflow <- gsm.core::MakeWorkflowList(
        strPackage = "gsm.kri"
      )
    }

    snapshot_names <- names(raw_data)
    if (is.null(snapshot_names) || any(snapshot_names == "")) {
      snapshot_names <- paste0("snapshot_", seq_along(raw_data))
    }
    snapshot_results <- setNames(vector("list", length(raw_data)), snapshot_names)

    for (i in seq_along(raw_data)) {
      snapshot_name <- snapshot_names[[i]]
      snapshot_data <- raw_data[[i]]

      vcat("\nProcessing snapshot ", i, "/", length(raw_data), " (", snapshot_name, ")...\n", sep = "")
      vcat("Available datasets:", paste(names(snapshot_data), collapse = ", "), "\n")

      required_datasets <- c("Raw_SITE", "Raw_SUBJ")
      available_datasets <- intersect(required_datasets, names(snapshot_data))
      if (length(available_datasets) < length(required_datasets)) {
        warning("Skipping snapshot ", snapshot_name, ": missing required datasets (", paste(required_datasets, collapse = ", "), ")")
        snapshot_results[[snapshot_name]] <- NULL
        next
      }

      # Only use domains present in this snapshot for mapping workflows
      available_domains <- names(snapshot_data)
      available_domains_stripped <- stringr::str_replace(available_domains, "Raw_", "")
      config_domains_norm <- trimws(tolower(config$domains))
      available_domains_norm <- trimws(tolower(available_domains_stripped))
      if (length(config_domains_norm) == 0 || all(is.na(config_domains_norm)) || all(config_domains_norm == "")) {
        config_domains_norm <- available_domains_norm
      }
      required_domains_norm <- trimws(tolower(c("SUBJ", "SITE", "STUDY", "ENROLL")))
      domains_to_map <- intersect(unique(c(config_domains_norm, required_domains_norm)), available_domains_norm)
      domains_to_map <- available_domains_stripped[match(domains_to_map, available_domains_norm)]
      domains_to_map <- domains_to_map[domains_to_map != "AntiCancer"]

      if (length(domains_to_map) == 0) {
        vcat("No domains available for mapping in snapshot ", snapshot_name, ". Skipping.\n", sep = "")
        snapshot_results[[snapshot_name]] <- NULL
        next
      }

      mappings_wf <- gsm.core::MakeWorkflowList(
        strNames = domains_to_map,
        strPath = "workflow/1_mappings",
        strPackage = "gsm.mapping"
      )

      mappings_spec <- CombineSpecs(mappings_wf)
      lRaw <- Ingest(snapshot_data, mappings_spec)
      mapped_data <- gsm.core::RunWorkflows(
        lWorkflow = mappings_wf,
        lData = lRaw
      )

      lResults <- list()
      for (wf_name in names(lWorkflow)) {
        single_wf <- lWorkflow[wf_name]
        wf_result <- tryCatch(
          gsm.core::RunWorkflows(
            lWorkflow = single_wf,
            lData = mapped_data
          ),
          error = function(e) {
            warning("Skipping workflow ", wf_name, " for snapshot ", snapshot_name, ": ", e$message)
            return(NULL)
          }
        )
        if (!is.null(wf_result)) {
          lResults <- c(lResults, wf_result)
        }
      }

      analytics_summary <- list(
        snapshot = snapshot_name,
        total_participants = nrow(snapshot_data$Raw_SUBJ %||% data.frame()),
        total_sites = nrow(snapshot_data$Raw_SITE %||% data.frame()),
        domains_available = names(snapshot_data),
        snapshots_processed = 1,
        workflows_executed = names(lResults),
        kri_results = length(lResults)
      )

      snapshot_results[[snapshot_name]] <- list(
        summary = analytics_summary,
        results = lResults,
        data = snapshot_data
      )
    }

    processed_count <- sum(!vapply(snapshot_results, is.null, logical(1)))
    vcat("\nAnalytics pipeline completed for ", processed_count, " of ", length(snapshot_results), " snapshots.\n", sep = "")

    if (processed_count == 0) {
      return(NULL)
    }

    return(snapshot_results)

  }, error = function(e) {
    warning("GSM analytics pipeline failed: ", e$message)
    vcat("Analytics pipeline skipped due to error.\n")
    return(NULL)
  })
}

#' Organize analytics pipeline results
#'
#' @param pipeline_results Raw pipeline results from gsm.core
#' @param verbose Whether to print progress/output messages
#' @return Organized analytics results
#' @export
organize_analytics_results <- function(pipeline_results, verbose = FALSE) {
  if (is.null(pipeline_results)) {
    if (isTRUE(verbose)) cat("Analytics pipeline completed with results for 0 study-level metrics\n")
    return(NULL)
  }

  # If analytics are returned per snapshot, organize each snapshot and preserve date names
  is_per_snapshot <- is.list(pipeline_results) &&
    length(pipeline_results) > 0 &&
    !"results" %in% names(pipeline_results)

  if (isTRUE(is_per_snapshot)) {
    organized_by_snapshot <- lapply(pipeline_results, function(snapshot_result) {
      organize_analytics_results(snapshot_result, verbose = FALSE)
    })
    if (!is.null(names(pipeline_results))) {
      names(organized_by_snapshot) <- names(pipeline_results)
    }
    if (isTRUE(verbose)) {
      cat("Analytics organized for", length(organized_by_snapshot), "snapshots\n")
    }
    return(organized_by_snapshot)
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

    # Process Analysis_* results (new pattern)
    analysis_results <- results[grep("^Analysis_", names(results), ignore.case = TRUE)]
    if (length(analysis_results) > 0) {
      # Try to classify by group level if possible
      for (name in names(analysis_results)) {
        res <- analysis_results[[name]]
        metric_id <- if (!is.null(res$ID)) {
          as.character(res$ID)
        } else {
          stringr::str_replace(as.character(name), "^Analysis_", "")
        }
        result_df <- NULL
        if (!is.null(res$Analysis_Summary) && is.data.frame(res$Analysis_Summary)) {
          result_df <- res$Analysis_Summary
        } else if (!is.null(res$Analysis_Flagged) && is.data.frame(res$Analysis_Flagged)) {
          result_df <- res$Analysis_Flagged
        } else if (!is.null(res$Analysis_Analyzed) && is.data.frame(res$Analysis_Analyzed)) {
          result_df <- res$Analysis_Analyzed
        } else {
          next
        }
        if (!"Metric_ID" %in% names(result_df)) {
          result_df$Metric_ID <- metric_id
        }

        # Try to detect group level from Analysis_Summary or Analysis_Analyzed
        group_level <- NA
        if (!is.null(res$Analysis_Summary) && "GroupLevel" %in% names(res$Analysis_Summary)) {
          group_level <- unique(res$Analysis_Summary$GroupLevel)
        } else if (!is.null(res$Analysis_Analyzed) && "GroupLevel" %in% names(res$Analysis_Analyzed)) {
          group_level <- unique(res$Analysis_Analyzed$GroupLevel)
        }
        # Assign to by_site, by_country, or by_study based on group level
        if (any(grepl("site", tolower(group_level)))) {
          organized_results$by_site$results <- rbind(organized_results$by_site$results, result_df)
          organized_results$by_site$metadata <- c(organized_results$by_site$metadata, list(res))
        } else if (any(grepl("country", tolower(group_level)))) {
          organized_results$by_country$results <- rbind(organized_results$by_country$results, result_df)
          organized_results$by_country$metadata <- c(organized_results$by_country$metadata, list(res))
        } else if (any(grepl("study", tolower(group_level)))) {
          organized_results$by_study$results <- rbind(organized_results$by_study$results, result_df)
          organized_results$by_study$metadata <- c(organized_results$by_study$metadata, list(res))
        } else {
          # If group level is not clear, put in by_study as fallback
          organized_results$by_study$results <- rbind(organized_results$by_study$results, result_df)
          organized_results$by_study$metadata <- c(organized_results$by_study$metadata, list(res))
        }
        total_metrics <- total_metrics + 1
      }
    }

    # Also support legacy patterns for site/country/study
    site_results <- results[grep("^site", names(results), ignore.case = TRUE)]
    if (length(site_results) > 0) {
      site_df <- do.call(rbind, lapply(names(site_results), function(result_name) {
        result <- site_results[[result_name]]$lResults
        if (!"Metric_ID" %in% names(result)) {
          result$Metric_ID <- as.character(result_name)
        }
        result
      }))
      organized_results$by_site$results <- rbind(organized_results$by_site$results, site_df)
      organized_results$by_site$metadata <- c(organized_results$by_site$metadata, lapply(site_results, function(x) x$lData))
      total_metrics <- total_metrics + length(site_results)
    }
    country_results <- results[grep("^country", names(results), ignore.case = TRUE)]
    if (length(country_results) > 0) {
      country_df <- do.call(rbind, lapply(names(country_results), function(result_name) {
        result <- country_results[[result_name]]$lResults
        if (!"Metric_ID" %in% names(result)) {
          result$Metric_ID <- as.character(result_name)
        }
        result
      }))
      organized_results$by_country$results <- rbind(organized_results$by_country$results, country_df)
      organized_results$by_country$metadata <- c(organized_results$by_country$metadata, lapply(country_results, function(x) x$lData))
      total_metrics <- total_metrics + length(country_results)
    }
    study_results <- results[grep("^study", names(results), ignore.case = TRUE)]
    if (length(study_results) > 0) {
      study_df <- do.call(rbind, lapply(names(study_results), function(result_name) {
        result <- study_results[[result_name]]$lResults
        if (!"Metric_ID" %in% names(result)) {
          result$Metric_ID <- as.character(result_name)
        }
        result
      }))
      organized_results$by_study$results <- rbind(organized_results$by_study$results, study_df)
      organized_results$by_study$metadata <- c(organized_results$by_study$metadata, lapply(study_results, function(x) x$lData))
      total_metrics <- total_metrics + length(study_results)
    }
  }

  # Report the total count
  if (total_metrics > 0) {
    if (isTRUE(verbose)) cat("Analytics pipeline completed with results for", total_metrics, "study-level metrics\n")
  } else {
    if (isTRUE(verbose)) cat("Analytics pipeline completed with results for 0 study-level metrics\n")
  }

  return(organized_results)
}

#' Generate analytics layers from raw data
#'
#' Convenience wrapper that executes the analytics pipeline and organizes
#' outputs into site/country/study layers (per snapshot when applicable).
#'
#' @param raw_data Raw study data (single or multi-snapshot list)
#' @param config Study configuration object
#' @param verbose Whether to print progress/output messages
#' @return Organized analytics results
#' @export
generate_analytics_layers <- function(raw_data, config, verbose = FALSE) {
  config$verbose <- verbose
  pipeline_results <- execute_analytics_pipeline(raw_data, config)
  organize_analytics_results(pipeline_results, verbose = verbose)
}

#' Generate raw data for study configuration
#'
#' @param config Study configuration object with enabled datasets
#' @param verbose Whether to print progress/output messages
#' @return List of raw data for enabled datasets
#' @export
generate_raw_data_from_config <- function(config, verbose = FALSE) {
  generate_snapshots_from_config(
    config = config,
    domain_package_df = NULL,
    default_package = "gsm.mapping",
    workflow_path = "workflow/1_mappings",
    verbose = verbose
  )
}

#' Generate study data with configuration
#'
#' @param config Study configuration object
#' @param workflow_path Path to workflow mappings (not used in new approach)
#' @param mappings Optional mappings list (not used in new approach)
#' @param package Package containing workflows (not used in new approach)
#' @param verbose Whether to print progress/output messages
#' @return List of generated study data
#' @export
generate_study_data <- function(config, workflow_path = "workflow/1_mappings",
                                           mappings = NULL, package = "gsm.mapping",
                                           verbose = FALSE) {

  # Use the new helper function instead of the old generate_rawdata_for_single_study
  raw_data <- generate_raw_data_from_config(config, verbose = verbose)

  return(raw_data)
}
