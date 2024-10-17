subjid <- function(n, ..., subjects = NULL) {
  # Function body for subjid
  if (is.null(subjects)) {
    result <- paste0("S", 1:n)
  } else {
    result <- sample(subjects$subjid, n, replace = TRUE)
  }
  return(result)
}


aeser <- function(n) {
  # Function body for aeser
  sample(c("Y", "N"), n, replace = TRUE)
}

studyid <- function(stid) {
  # Function body for studyid
  stid
}

invid <- function(n) {
  # Function body for invid
  sample(
    c("0X132", "0X107", "0X041", "0X152", "0X113", "0X103", "0X116", "0X139"),
    n,
    replace = TRUE
    )
}

country <- function(n) {
  # Function body for country
  sample(c("US", "China", "Japan"), n, replace = TRUE)
}

subjectid <- function(n) {
  # Function body for subjectid
  paste0("S", 1:n)
}

enrollyn <- function(n, isSiteDataset = TRUE) {
  # Function body for enrollyn
  if (isSiteDataset) {
    return("Y")
  } else {
    return("N")
  }

}

toxgrg_nsv <- function(n) {
  # Function body for toxgrg_nsv
  sample(c("", "0", "1", "2", "3", "4"),
         n,
         prob = c(0.49,0.4875,0.01, 0.005, 0.005, 0.0025),
         replace = TRUE)
}

deemedimportant <- function(n) {
  # Function body for deemedimportant
  sample(c("Yes", "No"), n, replace = TRUE)
}

sdrgyn <- function(n) {
  # Function body for sdrgyn
  sample(c("Y", "N"),
         prob = c(0.75, 0.25),
         n,
         replace = TRUE)
}

phase <- function(n) {
  # Function body for phase
  "Blinded Study Drug Completion"
}

compyn <- function(n) {
  # Function body for compyn
  sample(c("", "N"),
         prob = c(0.1, 0.9),
         n,
         replace = TRUE)
}

screened <- function(n) {
  lower_bound <- n %/% 3
  sample(lower_bound:n, size = 1)
}

subject_nsv <- function(n) {
  # Function body for subject_nsv
  paste0("S", 1:n, "-XXXX")
}

enrolldt <- function(n, startDate, endDate) {
  sample(seq(
    as.Date(startDate), as.Date(endDate), by = "day"
  ), n, replace = TRUE)
}

timeonstudy <- function(n, startDate, endDate) {
  # Function body for timeonstudy
  enrolldt <- sample(seq(
    as.Date(startDate), as.Date(endDate), by = "day"
  ), n, replace = TRUE)
  as.numeric(as.Date(endDate) - as.Date(enrolldt))
}

n_changes <- function(n) {
  # Function body for n_changes
  sample(0:6,
         prob = c(0.74, 0.22, 0.03, 0.005, 0.003, 0.0019, 0.0001),
         n,
         replace = TRUE)
}

data_entry_lag <- function(n) {
  # Function body for data_entry_lag
  sample(0:20,
         prob = c(0.25, 0.18, 0.14, 0.10, 0.07, 0.05, 0.05, 0.04, 0.03, 0.02, 0.02,
                  rep(0.005, 10)),
         n,
         replace = TRUE)
}

querystatus <- function(n) {
  # Function body for querystatus
  sample(c("Answered", "Closed", "Open"),
         prob = c(0.02, 0.96, 0.02),
         n,
         replace = TRUE)
}

queryage <- function(n) {
  # Function body for queryage
  sample(clindata::edc_queries$queryage,
         n,
         replace = T)
}

InvestigatorFirstName <- function(n) {
  # Function body for InvestigatorFirstName
  sample(c("John", "Joanne", "Fred"),
         n,
         replace = TRUE)
}

InvestigatorLastName <- function(n) {
  # Function body for InvestigatorLastName
  sample(c("Doe", "Deer", "Smith"),
         n,
         replace = TRUE)
}

site_status <- function(n) {
  # Function body for site_status
  sample(c("Active", "", "Closed"),
         n,
         replace = TRUE)
}

City <- function(n) {
  # Function body for City
  sample(c("London", "New York", "Tokyo"),
         n,
         replace = TRUE)
}

State <- function(n) {
  # Function body for State
  sample(c("", "State 1", "State 2"),
         n,
         replace = TRUE)
}

Country <- function(n) {
  # Function body for Country
  sample(c("US", "UK", "Japan"), n, replace = TRUE)
}

nickname <- function(n) {
  # Function body for nickname
  sample(c("OAK", "TREE", "GROOVE"), n, replace = TRUE)

}

protocol_title <- function(n,
                           prot_title = c("New Drug Phase 1", "New Drug Phase 2", "New Drug Phase 3")) {
  # Function body for protocol_title
  sample(prot_title, n, replace = TRUE)

}

status <- function(n, stat = c("Active", "", "Finalized")) {
  # Function body for status
  sample(stat,
         n,
         replace = TRUE)
}

num_plan_site <- function(num_pl_site) {
  # Function body for num_plan_site
  num_pl_site
}

num_plan_subj <- function(num_pl_subj) {
  # Function body for num_plan_subj
  num_pl_subj
}
