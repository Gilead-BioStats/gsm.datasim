combination_var_splitter <- function(variable_data, split_vars) {
  for (split_var_name in split_vars) {
    # Step 1: Find the index of the sublist in the main list
    sublist_index <- which(names(variable_data) == split_var_name)

    # Step 2: Extract the elements of the sublist
    sublist_elements <- variable_data[[sublist_index]]

    # Step 3: Remove the sublist from the main list
    variable_data[[sublist_index]] <- NULL

    # Step 4: Insert the sublist elements into the main list at the original position
    variable_data <- append(variable_data, sublist_elements, after = sublist_index - 1)
  }

  return(variable_data)
}


add_new_var_data <- function(dataset, vars, args, ...) {
  internal_args <- list(...)

  variable_data <- lapply(names(vars), function(var_name) {
    generator_func <- var_name
    if (!(var_name %in% names(args))) {
      curr_args <- args$default
    } else {
      curr_args <- args[[var_name]]
      if (!(var_name %in% names(dataset))) {
        curr_args[[var_name]] <- NULL
      } else {
        curr_args[[var_name]] <- dataset[[var_name]]
      }
    }

    # Generate data using the generator function
    do.call(generator_func, curr_args)
  })

  names(variable_data) <- names(vars)

  if ("split_vars" %in% names(internal_args)) {
    variable_data <- combination_var_splitter(variable_data, internal_args$split_vars)
  }

  variable_data <- as.data.frame(variable_data)

  if (!is.null(dataset)) {
    return(dplyr::bind_rows(dataset, variable_data))
  } else {
    return(variable_data)
  }
}

Raw_STUDY <- function(data, spec, ...) {
  inps <- list(...)

  if ("Raw_STUDY" %in% names(data)) {
    dataset <- data$Raw_STUDY
  } else {
    dataset <- NULL
  }

  args <- list(
               studyid = list(1, inps$StudyID),
               num_plan_site = list(inps$SiteCount),
               num_plan_subj = list(inps$ParticipantCount),
               default = list(1)
  )

  res <- add_new_var_data(dataset, spec$Raw_STUDY, args, ...)
  return(res)
}



Raw_SITE <- function(data, spec, ...) {

  inps <- list(...)

  curr_spec <- spec$Raw_SITE

  if ("Raw_SITE" %in% names(data)) {
    dataset <- data$Raw_SITE
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n_sites - previous_row_num

  if (all(c("Country", "State", "City") %in% names(curr_spec))) {
    curr_spec$Country_State_City <- list(required = TRUE)
    curr_spec$Country <- NULL
    curr_spec$State <- NULL
    curr_spec$City <- NULL

  }

  if (!("siteid" %in% names(curr_spec))) {
    curr_spec$siteid <- list(required = TRUE)
  }


  # Function body for Raw_SITE
  args <- list(
    studyid = list(n, data$Raw_STUDY$studyid[[1]]),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, ...)

  return(res)
}



Raw_SUBJ <- function(data, spec, startDate, endDate, ...) {

  inps <- list(...)
  #browser()

  curr_spec <- spec$Raw_SUBJ

  if ("Raw_SUBJ" %in% names(data)) {
    dataset <- data$Raw_SUBJ
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }
  #browser()
  n <- inps$n_subj - previous_row_num

  if (!("siteid" %in% names(curr_spec))) {
    curr_spec$siteid <- list(required = TRUE)
  }

  if (!("enrolldt" %in% names(curr_spec))) {
    curr_spec$enrolldt <- list(required = TRUE)
  }

  if (all(c("siteid", "invid", "country") %in% names(curr_spec))) {
    curr_spec$subject_site_synq <- list(required = TRUE)
    curr_spec$siteid <- NULL
    curr_spec$invid <- NULL
    curr_spec$country <- NULL

  }

  if (all(c("subjid", "subject_nsv") %in% names(curr_spec))) {
    curr_spec$subjid_subject_nsv <- list(required = TRUE)
    curr_spec$subjid <- NULL
    curr_spec$subject_nsv <- NULL
  }
  #browser()

  if (all(c("enrolldt", "timeonstudy") %in% names(curr_spec))) {
    curr_spec$enrolldt_timeonstudy <- list(required = TRUE)
    curr_spec$enrolldt <- NULL
    curr_spec$timeonstudy <- NULL
  }

  args <- list(
    studyid = list(n, data$Raw_STUDY$studyid[[1]]),
    subject_site_synq = list(n, data),
    enrolldt_timeonstudy = list(n, startDate, endDate),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, ...)

  # Recalculate for all data
  res$timeonstudy <- timeonstudy(n, res$enrolldt, endDate)

  return(res)
}

Raw_ENROLL <- function() {
  # Function body for Raw_ENROLL
}

Raw_AE <- function() {
  # Function body for Raw_AE
}


Raw_PD <- function() {
  # Function body for Raw_PD
}

Raw_LB <- function() {
  # Function body for Raw_LB
}

Raw_SDRGCOMP <- function() {
  # Function body for Raw_SDRGCOMP
}

Raw_STUDCOMP <- function() {
  # Function body for Raw_STUDCOMP
}

Raw_DATACHG <- function() {
  # Function body for Raw_DATACHG
}

Mapped_SUBJ <- function() {
  # Function body for Mapped_SUBJ
}

Raw_DATAENT <- function() {
  # Function body for Raw_DATAENT
}

Raw_QUERY <- function() {
  # Function body for Raw_QUERY
}
