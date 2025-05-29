generic_timestamp <- function(n, startDate,...) {
  rep(as.POSIXlt(startDate), n)
}

generic_date <- function(n, startDate,...) {
  rep(as.Date(startDate), n)
}

enroll_dt <- generic_date
visit_date <-generic_date
lb_dt <- generic_date
deviationdate <- generic_date
dvdtm <- generic_timestamp
created <- generic_date
site_active_dt <- generic_date

mincreated_dts <- generic_timestamp
