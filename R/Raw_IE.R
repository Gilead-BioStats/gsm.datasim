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
    dataset <- previous_data$Raw_SUBJ
    previous_row_num <- nrow(dataset)
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }

  n <- inps$n - previous_row_num
  if (n == 0) return(dataset)

  if (all(c("tiver", "ietestcd", "ietest", "ieorres", "iecat") %in% names(curr_spec))) {
    curr_spec$tiver_ietestcd_ietest_ieorres_iecat <- list(required = TRUE)
    curr_spec$tiver <- NULL
    curr_spec$ietestcd <- NULL
    curr_spec$ietest <- NULL
    curr_spec$ieorres <- NULL
    curr_spec$iecat <- NULL
  }

  args <- list(
    subjid = list(n, external_subjid = data$Raw_SUBJ$subjid, replace = FALSE),
    tiver_ietestcd_ietest_ieorres_iecat = list(n, ...),
    default = list(n)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_IE, ...)

  return(res)
}

tiver_ietestcd_ietest_ieorres_iecat <- function(n, iecode, ...) {
  TIVER_dat <- TIVER(n, ...)
  IETESTCD_dat <- IETESTCD(n, ...)
  IETEST_dat <- IETEST(n, iecode = IETESTCD_dat, ...)
  IEORRES_dat <- IEORRES(n, iecode = IETESTCD_dat, ...)
  IECAT_dat <-  IECAT(n, iecode = IETESTCD_dat, ...)

  return(list(
    TIVER = TIVER_dat,
    IETESTCD = IETESTCD_dat,
    IETEST = IETEST_dat,
    IEORRES = IEORRES_dat,
    IECAT = IECAT_dat
  ))
}

TIVER <- function(n, ...) {
  return(rep("A1", n))
}
IETESTCD <- function(n, ...) {
  return(sample(c(paste0("INCL", 1:10), paste0("EXCL", 1:10)), n, replace = TRUE))
}
IETEST <- function(n, iecode,...) {
  stringr::str_replace_all(iecode, c("^INCL" = "Inclusion ", "^EXCL" = "Exclusion "))
}
IEORRES <- function(n, iecode,...) {
  ifelse(stringr::str_detect(iecode, "INCL"), "Y", "N")
}
IECAT <- function(n, iecode,...) {
  ifelse(stringr::str_detect(iecode, "INCL"), "Inclusion", "Exlcusion")
}
