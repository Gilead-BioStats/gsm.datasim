subjid <- function(n, external_subjid = NULL, replace = TRUE, previous_subjid=NULL, ...) {
  args <- list(...)

  if (!is.null(external_subjid)) {
    return(sample(external_subjid, n, replace = replace))
  }

  # Generate all possible 3-digit numbers as strings with leading zeros
  possible_numbers <- sprintf("%03d", 0:99999)

  # Create all possible strings starting with "0X" and ending with the 3-digit numbers
  possible_strings <- paste0("S", possible_numbers)

  # Exclude the old strings to avoid duplication
  new_strings_available <- setdiff(possible_strings, previous_subjid)

  # Check if there are enough unique strings to generate
  if (length(new_strings_available) < n) {
    stop("Not enough unique strings available to generate ", n, " new strings.")
  }
  res <- sample(new_strings_available, n, replace = FALSE)

  # Randomly sample 'n' unique strings from the available strings
  return(res)
}


subjid_repeated <- function(n, subjs, ...) {
  return(list(
    subjid = repeat_rows(n, subjs)
  ))

}

subj_visit_repeated <- function(n, data, ...) {
  res <- repeat_rows(n, data)
  return(list(
    subjid = res$subjid,
    visnam = res$instancename
  ))
}

act_fpfv <- function(date_min, date_lim, prev_data, ...) {
  generate_random_fpfv(date_min, date_lim, FALSE, prev_data)
}

est_fpfv <- function(date_min, date_lim, prev_data, ...) {
  generate_random_fpfv(date_min, date_lim, FALSE, prev_data)
}

est_lplv <- function(date_min, date_lim, prev_data, ...) {
  generate_random_fpfv(date_min, date_lim, FALSE, prev_data)
}

est_lfpv <- function(date_min, date_lim, prev_data, ...) {
  generate_random_fpfv(date_min, date_lim, FALSE, prev_data)
}


subject_nsv_visit_repeated <- function(n, data, ...) {
  res <- repeat_rows(n, data)
  return(list(
    subject_nsv = res$subject_nsv,
    visnam = res$instancename
  ))
}

form <- function(n, subject_nsv_visits, forms, ...) {
  rep(forms$form, nrow(subject_nsv_visits))

}

field <- function(n, subject_nsv_visits, forms, ...) {
  rep(forms$field, nrow(subject_nsv_visits))

}

battrnam <- function(n, subj_visits, tests, ...) {
  rep(tests$battrnam, nrow(subj_visits))

}


lbtstnam <- function(n, subj_visits, tests, ...) {
  rep(tests$lbtstnam, nrow(subj_visits))

}


visit_dt <- function(n, subjs, start_date, possible_visits, ...) {
  rep(generate_consecutive_random_dates(nrow(possible_visits), start_date, 30), length(subjs))
}

foldername <- function(n, subjs, possible_visits, ...) {
  rep(possible_visits$foldername, length(subjs))
}

instancename <- function(n, subjs, possible_visits, ...) {
  rep(possible_visits$instancename, length(subjs))
}

subjid_subject_nsv <- function(n, dataset,...) {
  subjid_dat <- subjid(n, previous_subjid = dataset, ...)
  subject_nsv_dat <- subject_nsv(n, subjid_dat, ...)
  return(list(
    subjid = subjid_dat,
    subject_nsv = subject_nsv_dat
  ))

}

aeser <- function(n, ...) {
  # Function body for aeser
  sample(c("Y", "N"), n, replace = TRUE)
}

studyid <- function(n, stid, ...) {
  # Function body for studyid
  if (n == 1) {
    unlist(stid)
      } else {
    sample(stid, n, replace = TRUE)
  }

}

siteid <- function(n, isGenerated = FALSE, ...) {
  # Function body for invid
  args <- list(...)
  if ("siteid" %in% names(args)) {
    already_generated <- args$siteid
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

subject_site_synq <- function(n, Raw_SITE_data, ...) {
  Raw_SITE_data[sample(nrow(Raw_SITE_data), n, replace = TRUE),
                c("siteid", "pi_number", "country")] %>%
    dplyr::rename("invid" =  "pi_number")

}

invid <- function(n, isGenerated=FALSE, ...) {
  # Function body for invid
  args <- list(...)
  if ("invid" %in% names(args)) {
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

country <- function(n, ...) {
  # Function body for country
  sample(c("US", "China", "Japan"), n, replace = TRUE)
}

subjectid <- function(n, ...) {
  # Function body for subjectid
  paste0("S", 1:n)
}

enrollyn <- function(n, ...) {
  # Function body for enrollyn
  # if (isSubjDataset) {
  #   return("Y")
  # } else {
  #   return("N")
  # }
  sample(c("Y", "N"),
         prob = c(0.75, 0.25),
         n,
         replace = TRUE)

}

toxgrg_nsv <- function(n, ...) {
  # Function body for toxgrg_nsv
  sample(c("", "0", "1", "2", "3", "4"),
         n,
         prob = c(0.49,0.4875,0.01, 0.005, 0.005, 0.0025),
         replace = TRUE)
}

deemedimportant <- function(n, ...) {
  # Function body for deemedimportant
  sample(c("Yes", "No"), n, replace = TRUE)
}

sdrgyn <- function(n, ...) {
  # Function body for sdrgyn
  sample(c("Y", "N"),
         prob = c(0.75, 0.25),
         n,
         replace = TRUE)
}

phase <- function(n, external_phase = NULL, replace = TRUE, ...) {
  if (!is.null(external_phase)) {
    return(sample(external_phase, n, replace = replace))
  }
  # Function body for phase
  return("Blinded Study Drug Completion")
}

sdrgyn <- function(n, ...) {
  # Function body for sdrgyn
  sample(c("Y", "N"),
         n,
         replace = TRUE)
}

compyn <- function(n, ...) {
  # Function body for compyn
  sample(c("", "N"),
         prob = c(0.1, 0.9),
         n,
         replace = TRUE)
}

screened <- function(n, ...) {
  lower_bound <- n %/% 3 + 1
  sample(lower_bound:n, size = 1)
}

subject_nsv <- function(n, subjid, subject_nsv=NULL, replace = TRUE, ...) {
  # Function body for subject_nsv
  if (!is.null(subject_nsv)) {
    return(sample(subject_nsv, n, replace = replace))
  }
  return(paste0(subjid, "-XXXX"))
}

enrolldt <- function(n, startDate, endDate, enrollyn_dat, ...) {
  full_sample <- sample(seq(as.Date(startDate), as.Date(endDate), by = "day"), n, replace = TRUE)
  full_sample[enrollyn_dat == "N"] <- NA
  return(full_sample)
}

timeonstudy <- function(n, enrolldt, endDate, ...) {
  # Function body for timeonstudy
  as.numeric(as.Date(endDate) - as.Date(enrolldt))
}

enrollyn_enrolldt_timeonstudy <- function(n, startDate, endDate, ...) {
  enrollyn_dat <- enrollyn(n, ...)
  enrolldt_dat <- enrolldt(n, startDate, endDate, enrollyn_dat, ...)
  timeonstudy_dat <- timeonstudy(n, enrolldt_dat, endDate, ...)
  return(list(
    enrollyn = enrollyn_dat,
    enrolldt = enrolldt_dat,
    timeonstudy = timeonstudy_dat
  ))
}



n_changes <- function(n, ...) {

  # Function body for n_changes
  sample(0:6,
         prob = c(0.74, 0.22, 0.03, 0.005, 0.003, 0.0019, 0.0001),
         n,
         replace = TRUE)
}

data_entry_lag <- function(n, ...) {
  # Function body for data_entry_lag
  sample(0:20,
         prob = c(0.25, 0.18, 0.14, 0.10, 0.07, 0.05, 0.05, 0.04, 0.03, 0.02, 0.02,
                  rep(0.005, 10)),
         n,
         replace = TRUE)
}

querystatus <- function(n, ...) {
  # Function body for querystatus
  sample(c("Answered", "Closed", "Open"),
         prob = c(0.02, 0.96, 0.02),
         n,
         replace = TRUE)
}

queryage <- function(n, ...) {
  # Function body for queryage
  sample(clindata::edc_queries$queryage,
         n,
         replace = T)
}

InvestigatorFirstName <- function(n, ...) {
  # Function body for InvestigatorFirstName
  sample(c("John", "Joanne", "Fred"),
         n,
         replace = TRUE)
}

InvestigatorLastName <- function(n, ...) {
  # Function body for InvestigatorLastName
  sample(c("Doe", "Deer", "Smith"),
         n,
         replace = TRUE)
}

site_status <- function(n, ...) {
  # Function body for site_status
  sample(c("Active", "", "Closed"),
         n,
         replace = TRUE)
}

Country_State_City_data <- data.frame(
    country = c("UK","UK", "US", "US", "Japan", "Japan"),
    state = c("Greater London", "Buckinghamshire", "CA", "PA", "Kanto", "Kumamoto Prefecture"),
    city = c("London", "Milton Keynes", "Foster City", "Newtown Square", "Tokyo", "Kumamoto")
)

City <- function(n, ...) {
  cities <- unique(Country_State_City_data$city)
  # Function body for City
  sample(cities,
         n,
         replace = TRUE)
}

State <- function(n, ...) {
  args <- list(...)
  if ("cities" %in% names(args)) {
    indices <- match(args$cities, Country_State_City_data$city)
    states <- Country_State_City_data$state[indices]
  } else {
    states <- Country_State_City_data$state %>%
      sample(n,
             replace = TRUE)
  }
  return(states)
}

Country <- function(n, ...) {
  args <- list(...)
  if ("cities" %in% names(args)) {
    indices <- match(args$cities, Country_State_City_data$city)
    countries <- Country_State_City_data$country[indices]
  } else {
    countries <- Country_State_City_data$country %>%
      sample(n, replace = TRUE)
  }
  return(countries)
}

Country_State_City <- function(n, ...) {
  cities <- City(n)
  states <- State(n, cities = cities)
  countries <- Country(n, cities = cities)
  return(list(City = cities,
              State = states,
              Country = countries))
}


nickname <- function(n, ...) {
  # Function body for nickname
  sample(c("OAK", "TREE", "GROOVE"), n, replace = TRUE)

}

protocol_title <- function(n,
                           prot_title = c("New Drug Phase 1", "New Drug Phase 2", "New Drug Phase 3"), ...) {
  # Function body for protocol_title
  sample(prot_title, n, replace = TRUE)

}

status <- function(n, stat = c("Active", "", "Finalized"), ...) {
  # Function body for status
  sample(stat,
         n,
         replace = TRUE)
}

therapeutic_area <- function(n, stat = c("Oncology", "Virology", "Inflammation"), ...) {
  # Function body for TA
  sample(stat,
         n,
         replace = TRUE)
}

protocol_indication <- function(n, stat = c("Cardiovascular Health", "Lung Function", "Hematology"), ...) {
  # Function body for protocol_indication
  sample(stat,
         n,
         replace = TRUE)
}

product <- function(n, ...) {
  # Function body for product
  num <- sample(1:50,
         n,
         replace = TRUE)
  paste("Product Name", num)
}

num_plan_site <- function(num_pl_site, ...) {
  # Function body for num_plan_site
  unlist(num_pl_site)
}

num_plan_subj <- function(num_pl_subj, ...) {
  # Function body for num_plan_subj
  unlist(num_pl_subj)
}

subject_to_enrollment <- function(n, data, previous_data, ...) {
  if (length(previous_data) != 0) {
    data_pool <- data$Raw_SUBJ[!(data$Raw_SUBJ$subjid %in% previous_data), ]
  } else {
    data_pool <- data$Raw_SUBJ
  }

  sample_subset <- sample(min(nrow(data_pool), n), n, replace = FALSE)
  res <- data_pool[sample_subset,
                c("subjid", "invid", "country", "enrollyn")] %>%
    dplyr::mutate(subjectid = paste0("XX-", subjid))

  return(res)
}

