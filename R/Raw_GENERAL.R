generic_timestamp <- function(n, ...) {
  rep(as.POSIXlt(Sys.time()), n)
}

generic_date <- function(n, ...) {
  rep(as.Date(Sys.time()), n)
}

enroll_dt <- generic_date
visit_date <-generic_date
lb_dt <- generic_date
deviationdate <- generic_date
created <- generic_date
site_active_dt <- generic_date

mincreated_dts <- generic_timestamp
