#' Generate Raw PK Data
#'
#' Generate Raw PK Data based on `PK.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @returns a data.frame pertaining to the raw dataset plugged into `PK.yaml`
#' @family internal
#' @keywords internal
#' @noRd
Raw_PK <- function(data, previous_data, spec,...) {
  inps <- list(...)
  if ("Raw_PK" %in% names(previous_data)) {
    dataset <- previous_data$Raw_PK
    previous_row_num <- length(unique(dataset$subjid))
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) return(dataset)

  possible_visits <- data.frame(
    pktpt = c(
      "Cycle 1 Day 1",
      "Cycle 1 Day 1",
      "Cycle 1 Day 1",
      "Cycle 1 Day 15",
      "Cycle 1 Day 2",
      "Cycle 1 Day 4",
      "Cycle 1 Day 8"
    )
  )

  curr_spec <- spec$Raw_PK


  if (!("pktpt" %in% names(curr_spec))) {
    curr_spec$pktpt <- list(required = TRUE)
  }

  if (!("pkperf" %in% names(curr_spec))) {
    curr_spec$pkperf <- list(required = TRUE)
  }


  if (all(c("subjid", "visit", "pkdat") %in% names(curr_spec))) {
    curr_spec$subjid_visit_pkdat <- list(required = TRUE)
    curr_spec$subjid <- NULL
    curr_spec$visit <- NULL
    curr_spec$pkdat <- NULL
  }


  subjs <- subjid(n, external_subjid = data$Raw_SUBJ$subjid, replace = FALSE)
  args <- list(
    subjid_visit_pkdat = list(nrow(possible_visits), subjs, data$Raw_VISIT),
    default = list(n, subjs, possible_visits)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_PK, ...)

  return(res)
}

pktpt <- function(n, subjs, possible_visits, ...) {
  rep(possible_visits$pktpt, length(subjs))
}

pkperf <- function(n, subjs, possible_visits, ...) {
  sample(c("Yes", "No"), length(subjs), replace = TRUE, prob = c(0.95, 0.05))
}

subjid_visit_pkdat <- function(n, subjs, visit_data, ...) {
  df <- visit_data[which(visit_data$subjid %in% subjs),] %>%
    dplyr::select(subjid, foldername, visit_dt)
  return(list(
    subjid = df$subjid,
    visit = df$foldername,
    pkdat = df$visit_dt
  ))
}
