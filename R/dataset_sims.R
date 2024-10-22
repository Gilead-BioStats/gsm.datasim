add_new_var_data <- function(dataset, vars, args, ...) {
  internal_args <- list(...)


  variable_data <- lapply(names(vars), function(var_name) {
    # browser()
    generator_func <- var_name

    if (!(var_name %in% names(args))) {
      curr_args <- args$default
    } else {
      curr_args <- args[[var_name]]
      if (!(var_name %in% names(dataset))) {
        curr_args <- list(curr_args, var_name = NULL)
      } else {
        curr_args <- list(curr_args, var_name = dataset[[var_name]])
      }
    }

    # Generate data using the generator function
    do.call(generator_func, curr_args)
  })

  names(variable_data) <- names(vars)

  if ("split_vars" %in% internal_args) {
    for (split_var_name in names(internal_args$split_vars)) {
      # Step 1: Find the index of the sublist in the main list
      sublist_index <- which(names(variable_data) == split_var_name)

      # Step 2: Extract the elements of the sublist
      sublist_elements <- variable_data[[sublist_index]]

      # Step 3: Remove the sublist from the main list
      variable_data[[sublist_index]] <- NULL

      # Step 4: Insert the sublist elements into the main list at the original position
      variable_data <- append(variable_data, sublist_elements, after = sublist_index - 1)
    }
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
               studyid = list(inps$StudyID),
               num_plan_site = list(inps$SiteCount),
               num_plan_subj = list(inps$ParticipantCount),
               default = list(1)
  )

  res <- add_new_var_data(dataset, spec$Raw_STUDY, args)
  return(res)
}

data$Raw_STUDY <- Raw_STUDY(data, spec,
                            StudyID = StudyID,
                            SiteCount = SiteCount,
                            ParticipantCount = ParticipantCount)

Raw_SITE <- function(data, spec, ...) {

  inps <- list(...)
  curr_spec <- spec$Raw_SITE

  if ("Raw_SITE" %in% names(data)) {
    dataset <- data$Raw_SITE
  } else {
    dataset <- NULL
  }

  if (all(c("Country", "State", "City") %in% names(curr_spec))) {
    curr_spec$Country_State_City <- list(required = TRUE)
    curr_spec$Country <- NULL
    curr_spec$State <- NULL
    curr_spec$City <- NULL

  }

  # Function body for Raw_SITE
  args <- list(
    studyid = list(data$Raw_STUDY$studyid[[1]]),
    default = list(inps$n_sites)
  )
  res <- add_new_var_data(dataset, curr_spec, args)

  return(res)
}

# data$Raw_SITE
aaa <- Raw_SITE(data, spec, n_sites = 10,
                split_vars = list("Country_State_City" = c("Country", "State", "City"))
                )


Raw_SUBJ <- function() {
  # Function body for Raw_SUBJ
}

Raw_ENROLL <- function() {
  # Function body for Raw_ENROLL
}

Raw_AE <- function() {
  # Function body for Raw_AE
}

Raw_LB <- function() {
  # Function body for Raw_LB
}

Raw_PD <- function() {
  # Function body for Raw_PD
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
