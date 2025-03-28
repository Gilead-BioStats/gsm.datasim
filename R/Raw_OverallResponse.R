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

  if (all(c("response_folder", "ovrlresp", "rs_dt") %in% names(curr_spec))) {
    curr_spec$response_folder <- list(required = TRUE)
    curr_spec$ovrlresp <- list(required = TRUE)
    curr_spec$rs_dt <- list(required = TRUE)
  }

  args <- list(
    subjid = list(n, external_subjid = data$Raw_SUBJ$subjid, replace = FALSE),
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
rs_dt <- function(n, ovrlresp, ...) {
  rep(Sys.Date(), n)
}
