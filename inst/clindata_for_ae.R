clindata_ae <- clindata::rawplus_ae %>%
  filter(mdrsoc_nsv %in% unique(clindata::rawplus_ae$mdrsoc_nsv)[1:10]) %>%
  select(mdrsoc_nsv, mdrpt_nsv) %>% unique()
usethis::use_data(clindata_ae, internal = TRUE, overwrite = TRUE)
