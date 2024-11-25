parse_yaml_spec <- function(file_path) {
  spec <- yaml::yaml.load_file(file_path)
  return(spec)
}


add_new_var_data <- function(dataset, vars, n, split_vars = NULL) {
  internal_args <- list(...)

  variable_data <- lapply(names(vars), function(var_name) {
    # Generate data using the generator function
    do.call(var_name, curr_args)
  })


  names(variable_data) <- names(vars)

  if (!is.null(split_vars)) {
    variable_data <- combination_var_splitter(variable_data, internal_args$split_vars)
  }

  variable_data <- variable_data %>%
    as.data.frame() %>%
    rename_raw_data_vars_per_spec(orig_curr_spec)


  if (!is.null(dataset)) {
    return(dplyr::bind_rows(dataset, variable_data))
  } else {
    return(variable_data)
  }
}

create_dataset <- function(name, n, data, previous_data, yaml_spec, spec, external = NULL) {

  dataset <- if (name %in% names(previous_data)) previous_data[[name]] else NULL
  previous_row_num <- if (!is.null(dataset)) nrow(dataset) else 0

  n <- n - previous_row_num
  if (n == 0) return(dataset)

  curr_spec <- spec[[name]]

  # Check for unsupported variables
  unsupported_vars <- setdiff(names(curr_spec), names(yaml_spec$required_vars))
  if (length(unsupported_vars) > 0) {
    logger::log_error(glue::glue("Variable(s) {unsupported_vars} is/are not currently supported,
                             please update yaml spec for {name} and add var .yamls if needed"))
    stop()
  }

  # Update source_col for variables that have it
  for (var_name in names(curr_spec)) {
    var <- curr_spec[[var_name]]
    if ('source_col' %in% names(var)) {
      yaml_spec$required_vars[[var_name]]$source_col <- var$source_col
    }
  }

  vars <- yaml_spec$required_vars

  # Delete all vars that are marked as grouped
  vars <- vars[!sapply(vars, function(x) "grouped" %in% names(x))]

  variable_data <- lapply(names(vars), function(var_name) {
    curr_var <- vars[[var_name]]
    curr_args <- list(n)
    if ("external" %in% names(curr_var)) {
      for (el in curr_var$external) {
        curr_args <- append(curr_args, external[[el]])
      }
    }
    if ("dataset" %in% names(curr_var)) {
      for (el in curr_var$dataset) {
        curr_args <- append(curr_args, dataset[[el]])
      }
    }

    if ("data" %in% names(curr_var)) {
      #browser()
      for (df_name in names(curr_var$data)) {
        for (col_name in names(curr_var$data[[df_name]])) {

          col <- curr_var$data[[df_name]][[col_name]]
          additional_arg <- if (is.null(col)) data[[df_name]][[col_name]] else data[[df_name]][[col_name]][col]
          curr_args <- append(curr_args, additional_arg)
        }
      }
    }

    if ("random" %in% names(curr_var)) {
        curr_args <- append(curr_args, list(curr_var$random))
    }

    # Generate data using the generator function
    do.call(var_name, curr_args)
  })

  names(variable_data) <- names(vars)

  group_vars <- yaml_spec$group_vars %||% list()

  group_vars_data <- lapply(names(group_vars), function(var_name) {
    curr_var <- dplyr::bind_rows(group_vars[[var_name]])
    curr_args <- list(n, curr_var)
    # Generate data using the generator function
    do.call(var_name, curr_args)
  })

  group_vars_data <- do.call(c, group_vars_data)

  all_variable_data <- c(variable_data, group_vars_data) %>%
    as.data.frame() %>%
    rename_raw_data_vars_per_spec(vars)


  if (!is.null(dataset)) {
    return(dplyr::bind_rows(dataset, all_variable_data))
  } else {
    return(all_variable_data)
  }

}

################################## STUDY ####################################
phase <- function(n, pool_of_choises, external_phase = NULL, replace = TRUE) {
  if (!is.null(external_phase)) {
    return(sample(external_phase, n, replace = replace))
  }
  # Function body for phase
  return(unlist(pool_of_choises))
}

studyid <- function(n, pool_of_choises) {
  # Function body for studyid
  if (n == 1) {
    unlist(pool_of_choises)
  } else {
    sample(pool_of_choises, n, replace = TRUE)
  }

}

nickname <- function(n, pool_of_choises) {
  # Function body for nickname
  sample(pool_of_choises, n, replace = TRUE)

}

protocol_title <- function(n, pool_of_choises) {
  # Function body for protocol_title
  sample(pool_of_choises, n, replace = TRUE)

}

status <- function(n, pool_of_choises) {
  # Function body for status
  sample(pool_of_choises,
         n,
         replace = TRUE)
}

num_plan_site <- function(n, pool_of_choises) {
  # Function body for num_plan_site
  unlist(pool_of_choises)
}

num_plan_subj <- function(n, pool_of_choises) {
  # Function body for num_plan_subj
  unlist(pool_of_choises)
}

act_fpfv <- function(n, date_min, date_lim, prev_data=NULL) {
  generate_random_fpfv(date_min, date_lim, FALSE, prev_data)
}

est_fpfv <- function(n, date_min, date_lim, prev_data=NULL) {
  generate_random_fpfv(date_min, date_lim, FALSE, prev_data)
}

############################# SITE ################################

siteid <- function(n, siteid = NULL, isGenerated = FALSE, ...) {
  # Function body for invid
  if (!is.null(siteid)) {
    already_generated <- siteid
  } else {
    already_generated <- c()
  }

  if (isGenerated) {
    return(sample(already_generated, n, replace = TRUE))
  }


  # Generate all possible 3-digit numbers as strings with leading zeros
  possible_numbers <- sprintf("%03d", 0:9999)

  # Create all possible strings starting with "0X" and ending with the 3-digit numbers
  possible_strings <- paste0("Site", possible_numbers)

  # Exclude the old strings to avoid duplication
  new_strings_available <- setdiff(possible_strings, already_generated)

  # Check if there are enough unique strings to generate
  if (length(new_strings_available) < n) {
    stop("Not enough unique strings available to generate ", n, " new strings.")
  }

  # Randomly sample 'n' unique strings from the available strings
  sample(new_strings_available, n)

}

invid <- function(n, invid = NULL, isGenerated=FALSE, ...) {
  # Function body for invid
  args <- list(...)
  if (!is.null(invid)) {
    already_generated <- args$invid
  } else {
    already_generated <- c()
  }

  if (isGenerated) {
    return(sample(already_generated, n, replace = TRUE))
  }

  # Generate all possible 3-digit numbers as strings with leading zeros
  possible_numbers <- sprintf("%03d", 0:999)

  # Create all possible strings starting with "0X" and ending with the 3-digit numbers
  possible_strings <- paste0("0X", possible_numbers)

  # Exclude the old strings to avoid duplication
  new_strings_available <- setdiff(possible_strings, already_generated)

  # Check if there are enough unique strings to generate
  if (length(new_strings_available) < n) {
    stop("Not enough unique strings available to generate ", n, " new strings.")
  }

  # Randomly sample 'n' unique strings from the available strings
  sample(new_strings_available, n)
}

InvestigatorFirstName <- function(n, data_pool) {
  # Function body for InvestigatorFirstName
  sample(data_pool,
         n,
         replace = TRUE)
}

InvestigatorLastName <- function(n, data_pool) {
  # Function body for InvestigatorLastName
  sample(data_pool,
         n,
         replace = TRUE)
}

site_status <- function(n, ...) {
  # Function body for site_status
  sample(c("Active", "", "Closed"),
         n,
         replace = TRUE)
}



City <- function(n, Country_State_City_data) {
  cities <- unique(Country_State_City_data$city)
  # Function body for City
  sample(cities,
         n,
         replace = TRUE)
}

State <- function(n, Country_State_City_data, cities = NULL) {
  if (!is.null(cities)) {
    indices <- match(cities, Country_State_City_data$city)
    states <- Country_State_City_data$state[indices]
  } else {
    states <- sample(Country_State_City_data$state, n, replace = TRUE)
  }
  return(states)
}

Country <- function(n, Country_State_City_data, cities = NULL) {
  if (!is.null(cities)) {
    indices <- match(cities, Country_State_City_data$city)
    countries <- Country_State_City_data$country[indices]
  } else {
    countries <- sample(Country_State_City_data$country, n, replace = TRUE)
  }
  return(countries)
}

Country_State_City <- function(n, Country_State_City_data) {
  cities <- City(n, Country_State_City_data)
  states <- State(n, Country_State_City_data, cities = cities)
  countries <- Country(n, Country_State_City_data, cities = cities)
  return(list(City = cities,
              State = states,
              Country = countries))
}

