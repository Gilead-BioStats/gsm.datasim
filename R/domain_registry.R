#' Domain Registry (draft)
#'
#' Returns a named list that defines per-domain generation behavior for the
#' registry-based migration path.
#'
#' @return Named list of domain registry entries.
#' @export
get_domain_registry <- function() {
  list(
    Raw_AE = list(
      dataset = "Raw_AE",
      package = "gsm.mapping",
      workflow_path = "workflow/1_mappings",
      required_inputs = c("data", "previous_data", "combined_specs", "n", "start_date", "end_date"),
      count_fn = function(counts, snapshot_idx) counts$ae_count[snapshot_idx],
      args_builder = function(context) {
        list(
          context$data,
          context$previous_data,
          context$combined_specs,
          n = context$n,
          startDate = context$start_date,
          endDate = context$end_date,
          split_vars = list("aest_dt_aeen_dt")
        )
      },
      generate_fn = function(args) {
        do.call(Raw_AE, args)
      }
    ),
    Raw_LB = list(
      dataset = "Raw_LB",
      package = "gsm.mapping",
      workflow_path = "workflow/1_mappings",
      required_inputs = c("data", "previous_data", "combined_specs", "n", "start_date"),
      count_fn = function(counts, snapshot_idx) counts$subject_count[snapshot_idx],
      args_builder = function(context) {
        list(
          context$data,
          context$previous_data,
          context$combined_specs,
          n = context$n,
          startDate = context$start_date,
          split_vars = list("subj_visit_repeated")
        )
      },
      generate_fn = function(args) {
        do.call(Raw_LB, args)
      }
    )
  )
}

validate_domain_registry_entry <- function(entry) {
  required_fields <- c("dataset", "required_inputs", "count_fn", "args_builder", "generate_fn")
  missing_fields <- setdiff(required_fields, names(entry))
  if (length(missing_fields) > 0) {
    stop("Invalid domain registry entry. Missing fields: ", paste(missing_fields, collapse = ", "))
  }

  if (!is.character(entry$dataset) || length(entry$dataset) != 1) {
    stop("Invalid domain registry entry: 'dataset' must be a single character value")
  }
  if (!is.character(entry$required_inputs) || length(entry$required_inputs) == 0) {
    stop("Invalid domain registry entry: 'required_inputs' must be a non-empty character vector")
  }
  if (!is.function(entry$count_fn) || !is.function(entry$args_builder) || !is.function(entry$generate_fn)) {
    stop("Invalid domain registry entry: 'count_fn', 'args_builder', and 'generate_fn' must be functions")
  }

  invisible(TRUE)
}

generate_domain_from_registry <- function(data_type, context, registry = get_domain_registry()) {
  if (!data_type %in% names(registry)) {
    return(NULL)
  }

  entry <- registry[[data_type]]
  validate_domain_registry_entry(entry)

  missing_inputs <- setdiff(entry$required_inputs, names(context))
  if (length(missing_inputs) > 0) {
    stop("Missing required context fields for ", data_type, ": ", paste(missing_inputs, collapse = ", "))
  }

  args <- entry$args_builder(context)
  as.data.frame(entry$generate_fn(args))
}
