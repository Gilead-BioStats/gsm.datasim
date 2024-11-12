count_gen <- function(max_n, SnapshotCount) {
  iteration <- max_n / SnapshotCount
  counts <- c()
  for (i in seq(SnapshotCount)) {
    if (i > 1) {
      start <- counts[i - 1]
    } else {
      start <- 1
    }
    end <- i * iteration

    if (i < SnapshotCount) {
      if ((start < end) & ((floor(end) - start) > 1)) {
        new_element <- sample(start:floor(end), size = 1)
      } else {
        new_element <- start
      }
    } else {
      new_element <- max_n

    }

    counts <- c(counts, new_element)
  }

  return(counts)
}

load_specs <- function(workflow_path, mappings, package) {
  wf_mapping <- gsm::MakeWorkflowList(strPath = workflow_path, strNames = mappings, strPackage = package)
  wf_req <-  gsm::MakeWorkflowList(strPath =  "workflow/1_mappings", strNames = c("SUBJ", "STUDY", "SITE"), strPackage = "gsm")
  wf_all <- modifyList(wf_mapping, wf_req)
  combined_specs <- gsm::CombineSpecs(wf_all)

  return(combined_specs)
}

rename_raw_data_vars_per_spec <- function(variable_data, spec) {
  for (var_name in names(spec)) {
    variabale <- spec[[var_name]]

    # Check if "source_col" exists in the sublist
    if ("source_col" %in% names(variabale)) {
      # Retrieve the new name from "source_col"
      new_name <- variabale[["source_col"]]
      # Rename the variable in the appropriate dataset in the snapshot
      variable_data <- variable_data %>% dplyr::rename(!!new_name := var_name)
    }
  }
  return(variable_data)
}

generate_unique_combinations_code <- function(data, vars, run_code=FALSE) {
  # Extract unique combinations
  unique_combinations <- unique(data[, vars, drop = FALSE])

  # Start constructing the code
  code <- "unique_combinations <- data.frame(\n"

  # Iterate over each variable to construct the code
  for (i in seq_along(vars)) {
    var_name <- vars[i]
    values <- unique_combinations[[var_name]]
    # Handle character and factor variables by quoting the values
    if (is.character(values) || is.factor(values)) {
      values_str <- paste0('"', values, '"', collapse = ", ")
    } else {
      values_str <- paste(values, collapse = ", ")
    }
    # Add the variable and its values to the code
    code <- paste0(code, "  ", var_name, " = c(", values_str, ")")
    # Add a comma if not the last variable
    if (i < length(vars)) {
      code <- paste0(code, ",\n")
    } else {
      code <- paste0(code, "\n")
    }
  }
  code <- paste0(code, ")")

  if (run_code) {
    result <- eval(parse(text = code_to_run))
  } else {
    result <- code
  }

  cat(result)

  return(result)
}


generate_consecutive_random_dates <- function(n, start_date, mean_days_between_dates = 30, ...) {
  start_date <- as.Date(start_date)
  dates <- vector("character", n)
  prev_date <- start_date

  for (i in seq_len(n)) {
    # Define the date range
    min_date <- prev_date
    max_date <- prev_date + mean_days_between_dates
    # Generate a random date within the range
    random_date <- as.Date(sample(seq(min_date, max_date, by = "day"), 1), origin = "1970-01-01")
    # Store the date as a string
    dates[i] <- format(random_date, "%Y-%m-%d")
    # Update prev_date for the next iteration
    prev_date <- random_date
  }
  return(dates)
}

repeat_rows <- function(n, data) {
  if (is.vector(data)) {
    return(rep(data, each = n))
  } else if (is.data.frame(data) || is.matrix(data)) {
    result <- data[rep(seq_len(nrow(data)), each = n), , drop = FALSE]
    rownames(result) <- NULL
    return(result)
  } else {
    stop("Unsupported data type. Input must be a vector, matrix, or data frame.")
  }
}

generate_form_df <- function(n) {
  num_forms <- ceiling(n / 4)
  forms <- rep(paste0("form", 1:num_forms), each = 4)[1:n]
  fields <- paste0("field", 1:n)
  data.frame(form = forms, field = fields)
}

enrollment_count_gen <- function(subject_count) {
  screened <- function(n, previous_screened) {
    lower_bound <- max(n %/% 3, previous_screened)
    if (lower_bound != n) {
      return(sample(lower_bound:n, size = 1))
    } else {
      return(n)
    }
  }

  previous_screened <- 0
  enrollment_count <- c()
  for (i in seq(subject_count)) {
    if (i == 1) {
      previous_screened <- 0
    } else {
      previous_screened <- subject_count[i - 1]
    }

    enrollment_count <- c(enrollment_count, screened(subject_count[i], previous_screened))
  }

  return(enrollment_count)
}


save_data_on_disk <- function(data, base_path = NULL) {
  logger::log_info(glue::glue("Saving datasets ..."))
  logger::log_info(glue::glue("Please wait, proces may take around 15 minutes due to the number of files and file sizes ..."))

  # Calculate the total number of dataframes to process
  total_dfs <- sum(
    sapply(data,
           function(study_data) {
             sum(
               sapply(study_data,
                      function(snapshot_data) {
                        length(snapshot_data)
                        })
             )
           })
  )

  # Initialize the progress bar
  pb <- txtProgressBar(min = 0, max = total_dfs, style = 3)
  counter <- 0  # Initialize a counter

  tictoc::tic()
  for (study_name in names(data)) {
    study_data <- data[[study_name]]

    for (snapshot_name in names(study_data)) {
      snapshot_data <- study_data[[snapshot_name]]

      # Create the folder structure using the names
      if (is.null(base_path)) {
        base_path <- "data-raw"
      }

      dir_path <- file.path(base_path, "simulated_data", study_name, snapshot_name)

      dir.create(dir_path, recursive = TRUE, showWarnings = FALSE)

      for (df_name in names(snapshot_data)) {
        df_value <- snapshot_data[[df_name]]
        # Define the file path for the Parquet file
        file_path <- file.path(dir_path, paste0(df_name, ".parquet"))

        # Save the dataframe to a Parquet file
        arrow::write_parquet(df_value, file_path)

        counter <- counter + 1
        setTxtProgressBar(pb, counter)

      }
    }
  }

  close(pb)

  logger::log_info(glue::glue("Saved all data successfully"))
  tictoc::toc()

}

