# Helper to get all endpoint mapping domains plus extra gsm.endpoints domains
get_endpoints_domains <- function() {
  # Get all mapping names from gsm.endpoints workflows
  lEndpointMappings <- gsm.core::MakeWorkflowList(
    strPath = fs::path_package('gsm.endpoints', 'workflow', '1_mappings'),
    strPackage = NULL
  )
  endpoint_domains <- lEndpointMappings %>%
    gsm.mapping::CombineSpecs() %>%
    names() %>%
    stringr::str_replace('Mapped_', '')

  # Domains from gsm.endpoints
  df <- data.frame(
    domain = endpoint_domains,
    package = 'gsm.endpoints',
    stringsAsFactors = FALSE
  )

  # Add required reporting domains from gsm.mapping
  extra_domains <- data.frame(
    domain = c('STUDY', 'SITE'),
    package = 'gsm.mapping',
    stringsAsFactors = FALSE
  )

  df <- rbind(df, extra_domains)
  return(df)
}
