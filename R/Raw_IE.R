#' Generate Raw IE Data
#'
#' Generate Raw IE based on `IE.yaml` from `gsm.mapping`.
#'
#' @inheritParams Raw_STUDY
#' @returns a data.frame pertaining to the raw dataset plugged into `IE.yaml`
#' @family internal
#' @keywords internal
#' @noRd

Raw_IE <- function(data, previous_data, spec, ...) {
  inps <- list(...)

  curr_spec <- spec$Raw_IE

  if ("Raw_IE" %in% names(previous_data)) {
    dataset <- previous_data$Raw_SUBJ %>% filter(enrollyn == "N")
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) return(dataset)

  if (all(c("TIVER_STD", "IETESTCD_STD", "IETEST_STD", "IEORRES_STD", "IECAT_STD") %in% names(curr_spec))) {
    curr_spec$TIVER_STD_IETESTCD_STD_IETEST_STD_IEORRES_STD_IECAT_STD <- list(required = TRUE)
    curr_spec$TIVER_STD <- NULL
    curr_spec$IETESTCD_STD <- NULL
    curr_spec$IETEST_STD <- NULL
    curr_spec$IEORRES_STD <- NULL
    curr_spec$IECAT_STD <- NULL
  }

  args <- list(
    subjid = list(n, external_subjid = data$Raw_SUBJ$subjid),
    TIVER_STD_IETESTCD_STD_IETEST_STD_IEORRES_STD_IECAT_STD = list(n, ...),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_IE, ...)

  return(res)
}

TIVER_STD_IETESTCD_STD_IETEST_STD_IEORRES_STD_IECAT_STD <- function(n, iecode, ...) {
  TIVER_STD_dat <- TIVER_STD(n, ...)
  IETESTCD_STD_dat <- IETESTCD_STD(n, ...)
  IETEST_STD_dat <- IETEST_STD(n, iecode = IETESTCD_STD_dat, ...)
  IEORRES_STD_dat <- IEORRES_STD(n, iecode = IETESTCD_STD_dat, ...)
  IECAT_STD_dat <-  IECAT_STD(n, iecode = IETESTCD_STD_dat, ...)

  return(list(
    TIVER_STD = TIVER_STD_dat,
    IETESTCD_STD = IETESTCD_STD_dat,
    IETEST_STD = IETEST_STD_dat,
    IEORRES_STD = IEORRES_STD_dat,
    IECAT_STD_dat = IECAT_STD_dat
  ))
}

TIVER_STD <- function(n, ...) {
  return(rep("A1", n))
}
IETESTCD_STD <- function(n, ...) {
  return(sample(c(paste0("INCL", 1:10), paste0("EXCL", 1:10)), n, replace = TRUE))
}
IETEST_STD <- function(n, iecode,...) {
  stringr::str_replace_all(iecode, c("^INCL" = "Inclusion ", "^EXCL" = "Exclusion "))
}
IEORRES_STD <- function(n, iecode,...) {
  ifelse(stringr::str_detect(iecode, "INCL"), "Y", "N")
}
IECAT_STD <- function(n, iecode,...) {
  ifelse(stringr::str_detect(iecode, "INCL"), "Inclusion", "Exlcusion")
}
