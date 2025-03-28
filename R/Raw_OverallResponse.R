#' Generate Raw Overall Response Data
#'
#' Generate Raw Overall Response Data based on `OverallResponse.yaml` from `gsm.mapping`.
#' These values align with RECIST 1.1 guidelines.
#'
#' @inheritParams Raw_STUDY
#'
#' @returns a data.frame pertaining to the raw dataset plugged into `OverallResponse.yaml`
#' @family internal
#' @keywords internal
#' @noRd
Raw_OverallResponse <- function(data, previous_data, spec, ...) {
  # Function body for Raw_OverallResponse
  inps <- list(...)

  curr_spec <- spec$Raw_OverallResponse


  if ("Raw_OverallResponse" %in% names(previous_data)) {
    dataset <- previous_data$Raw_OverallResponse
    previous_row_num <- length(unique(dataset$subjid))
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }
  n <- inps$n - previous_row_num
  if (n == 0) return(dataset)

  if (all(c("response_folder") %in% names(curr_spec))) {
    curr_spec$response_folder <- list(required = TRUE)
  }

  if (all(c("ovrlresp") %in% names(curr_spec))) {
    curr_spec$ovrlresp <- list(required = TRUE)
  }

  if (all(c("subjid", "rs_dt") %in% names(curr_spec))) {
    curr_spec$subjid_rs_dt <- list(required = TRUE)
    curr_spec$subjid <- NULL
    curr_spec$rs_dt <- NULL
  }

  args <- list(
    subjid_rs_dt = list(n,
                        subjids = data$Raw_SUBJ$subjid[sample(nrow(data$Raw_SUBJ), n/3, replace = TRUE)],
                        Raw_VISIT_data = data$Raw_VISIT),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_OverallResponse, ...)

  return(res)
}
response_folder <- function(n, ...) {
  rep("final response", n, replace = TRUE)
}
ovrlresp <- function(n, ...) {
  sample(c("NE", "PD", "SD", "PR", "CR"), n, replace = TRUE, prob = c(0.05, 0.65, 0.2, 0.05, 0.05))
}

subjid_rs_dt <- function(n, subjids, Raw_VISIT_data, ...) {
  filtered_visit_data <- Raw_VISIT_data %>% filter(subjid %in% subjids)
  res <- filtered_visit_data[sample(nrow(filtered_visit_data), n, replace = TRUE),
                       c("subjid", "visit_dt")] %>%
    rename("rs_dt" = "visit_dt")
  return(list(subjid = res$subjid,
              rs_dt = res$rs_dt))
}
