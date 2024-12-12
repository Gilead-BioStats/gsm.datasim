
parse_yaml_const <- function(yaml_file) {
  const_raw_data <- yaml::yaml.load_file(yaml_file)
  result <- const_raw_data

  if ("dataframe" %in% names(const_raw_data)) {
    result <- dplyr::bind_rows(const_raw_data$dataframe)

  } else if ("variable" %in% names(const_raw_data)) {
    result <- const_raw_data$variable
  }
  return(result)
}

# A helper function that recursively searches for the `yaml` key and expands it
expand_yaml_references <- function(x, base_path = ".") {
  if (is.list(x) & !is.data.frame(x)) {
    # If the current list element has a `yaml` key, read in the referenced YAML
    if ("yaml" %in% names(x)) {
      # The value of `yaml` should be a file stem that we append `.yaml` to
      yaml_file <- file.path(base_path, paste0(x$yaml, ".yaml"))

      # Load the referenced YAML file
      parse_result <- parse_yaml_const(yaml_file)

      # Recursively expand the included YAML in case it has nested references
      return(expand_yaml_references(parse_result, base_path))

    } else {
      # If no `yaml` key at this level, check sub-elements
      return(lapply(x, expand_yaml_references, base_path = base_path))
    }
  } else {
    # If it's not a list, just return the value as is
    return(x)
  }
}

parse_yaml_spec <- function(spec_name, base_path = "./inst/variables", method_name=NULL) {
  yaml_file <- file.path(base_path, paste0(spec_name, ".yaml"))
  full_spec <- yaml::yaml.load_file(yaml_file)
  method_spec <- full_spec
  if ("methods" %in% names(full_spec)) {
    if (is.null(method_name)) {
      method_name <- names(full_spec$methods)[[1]]
    }

    # Store method_name in method_spec$method
    method_spec$method <- method_name

    # Copy elements from full_spec$methods[[method_name]] to method_spec
    for (el in names(full_spec$methods[[method_name]])) {
      method_spec[[el]] <- full_spec$methods[[method_name]][[el]]
    }

    # Remove the entire methods element
    method_spec$methods <- NULL
  }

  method_spec <- expand_yaml_references(method_spec, base_path = base_path)

  return(method_spec)
}

generate_padded_sequence <- function(range_str) {
  # Split the input string into start and end parts
  parts <- strsplit(range_str, ":", fixed = TRUE)[[1]]

  # Convert to integers
  start_val <- as.integer(parts[1])
  end_val <- as.integer(parts[2])

  # Determine the number of digits needed based on the 'end' part
  width <- nchar(parts[2])

  # Generate the sequence and format with leading zeros
  seq_values <- seq(from = start_val, to = end_val)
  formatted_values <- sprintf(paste0("%0", width, "d"), seq_values)

  return(formatted_values)
}

consecutive_generator <- function(n, prefix, variation, previous_data, retrieveGenerated = FALSE) {
  if (!is.null(previous_data)) {
    already_generated <- previous_data
  } else {
    already_generated <- c()
  }

  if (retrieveGenerated) {
    return(sample(already_generated, n, replace = TRUE))
  }

  # Generate all possible 3-digit numbers as strings with leading zeros
  possible_numbers <- generate_padded_sequence(variation)

  # Create all possible strings starting with "0X" and ending with the 3-digit numbers
  possible_strings <- paste0(prefix, variation)

  # Exclude the old strings to avoid duplication
  new_strings_available <- setdiff(possible_strings, already_generated)

  # Check if there are enough unique strings to generate
  if (length(new_strings_available) < n) {
    stop("Not enough unique strings available to generate ", n, " new strings.")
  }

  # Randomly sample 'n' unique strings from the available strings
  sample(new_strings_available, n)

}

create_dataset_new <- function(name, n, current_data, previous_data, yaml_spec, spec, external = NULL) {
  browser()
  previous_dataset <- if (name %in% names(previous_data)) previous_data[[name]] else NULL
  previous_row_num <- if (!is.null(previous_dataset)) nrow(previous_dataset) else 0

  n <- n - previous_row_num
  if (n == 0) return(previous_dataset)

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

  variable_data <- list()
  for (var_name in names(vars)) {
    browser()

    curr_var <- vars[[var_name]]
    curr_args <- list("n" = n)
    if ("parameters"  %in% names(curr_var)) {
      params <- curr_var$parameters
      for (param_name in names(params)) {
        param <- params[[param_name]]
        if (!is.null(param$external)) {
          curr_args[[param_name]] <- external[[param$external]]

        } else if (!is.null(param$previous_dataset)) {
          curr_args[[param_name]] <- previous_dataset[[param$previous_dataset]]

        } else if (!is.null(param$current_data)) {
          for (ds_name in names(param$current_data)) {
            ds <- param$current_data[[ds_name]]
            for (col_name in names(ds)) {
                col <- ds[[col_name]]
                number_of_rows_to_add <- if (is.null(col)) 1 else ds[[col_name]]
                arg_to_add <- current_data[[ds_name]][[col_name]]

                if (is.vector(arg_to_add) | is.list(arg_to_add)) {
                  arg_to_add <- arg_to_add[[1:number_of_rows_to_add]]
                }

                curr_args[[param_name]] <- arg_to_add
            }
          }

        } else if (!is.null(param$random)) {
            curr_args[[param_name]] <- param$random

        }  else if (!is.null(param$derived)) {
          curr_args[[param_name]] <- variable_data[[param$derived]]
        }

      }

    }

    func_yaml <- parse_yaml_spec(var_name, method_name = curr_var$method)

    # Generate data using the generator function
    variable_data[[var_name]] <- simulate_variable(func_yaml, curr_args)
  }

  group_vars <- yaml_spec$group_vars %||% list()

  # group_vars_data <- lapply(names(group_vars), function(var_name) {
  #   browser()
  #   curr_var <- group_vars[[var_name]]
  #   # if (var_name == "act_fpfv") browser()
  #   curr_args <- list("n" = n)
  #
  #   curr_var <- dplyr::bind_rows(group_vars[[var_name]])
  #   curr_args <- list(n, curr_var)
  #   func_yaml <- parse_yaml_spec(glue::glue("~/gsm.datasim/inst/variables/{var_name}.yaml"), curr_var$method)
  #
  #   # Generate data using the generator function
  #   simulate_variable(func_yaml, curr_args)
  # })

  group_vars_data <- do.call(c, group_vars_data)

  all_variable_data <- c(variable_data, group_vars_data) %>%
    as.data.frame() %>%
    rename_raw_data_vars_per_spec(vars)


  if (!is.null(previous_dataset)) {
    return(dplyr::bind_rows(previous_dataset, all_variable_data))
  } else {
    return(all_variable_data)
  }

}


first_non_null <- function(...) {
  args <- list(...)
  for (arg in args) {
    if (!is.null(arg)) return(arg)
  }
  return(NULL)  # if all are NULL
}


get_arg <- function(arg_name, list_to_search, defaults) {
  default_name <- paste0("default_", arg_name)
  first_non_null(list_to_search[[arg_name]], defaults[[default_name]])
}



# Universal simulation function
simulate_variable <- function(var, var_inputs) {
    method <- var$method
    name <- var$name
    print(name)

    if (method == 'sample') {
      # Get pool_of_choices and replace from inputs or use defaults
      pool <- get_arg("pool_of_choices", var_inputs, var)
      replace <- get_arg("replace", var_inputs, var)
      result <- sample(pool, var_inputs$n, replace = TRUE)

    } else if (method == 'return_value') {
      # Get value from inputs or use default
      value <- get_arg("value", var_inputs, var)
      result <- rep(value, var_inputs$n)

    } else if (method == 'generate_random_fpfv') {
      # Get date_min and date_lim from inputs or use defaults
      date_min <- get_arg("MinDate", var_inputs, var)
      date_max <- get_arg("MaxDate", var_inputs, var)
      result <- generate_random_fpfv(min_date = date_min, max_date = date_max)

    } else if (method == 'consecutive_generator') {
      # Get date_min and date_lim from inputs or use defaults
      prefix <- get_arg("prefix", var_inputs, var)
      variation <- get_arg("variation", var_inputs, var)
      previous_data <- get_arg("previous_data", var_inputs, var)
      retrieveGenerated <- get_arg("retrieveGenerated", var_inputs, var)

      result <- consecutive_generator(var_inputs$n, prefix, variation, previous_data, retrieveGenerated)

    } else if (method == 'from_another_var') {
      # Get date_min and date_lim from inputs or use defaults
      full_df_pool <- get_arg("full_df_pool", var_inputs, var)
      result <- from_another_var(var_inputs, full_df_pool = full_df_pool)

    } else {
      stop(paste("Unknown method", method, "for variable", name))
    }

  return(result)
}

from_another_var <- function(var_inputs, full_df_pool) {
  browser()
  print('aha')
  return(0)
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

