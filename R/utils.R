count_gen <- function(max_n, SnapshotCount) {
  iteration <- max_n %/% SnapshotCount

  counts <- c()
  for (i in seq(SnapshotCount)) {
    if (i > 1) {
      start <- counts[i - 1]
    } else {
      start <- 1
    }

    end <- i * iteration


    if (i < SnapshotCount) {
      counts <- c(counts, sample(start:end, size = 1))
    } else {
      counts <- c(counts, max_n)

    }
  }

  return(counts)
}

load_specs <- function(workflow_path, kris, package) {
  wf_mapping <- gsm::MakeWorkflowList(strPath = workflow_path, strNames = kris, strPackage = package)
  wf_req <-  gsm::MakeWorkflowList(strPath =  "workflow/1_mappings", strNames = c("SUBJ", "STUDY", "SITE"), strPackage = "gsm")
  wf_all <- modifyList(wf_mapping, wf_req)
  combined_specs <- gsm::CombineSpecs(wf_all)

  return(combined_specs)
}

rename_raw_data_vars_per_spec <- function(raw_data_list, combined_specs) {
  lapply(raw_data_list, function(study) {
    lapply(study, function(snapshot) {
      for (spec_name in names(snapshot)) {
        spec <- combined_specs[[spec_name]]
        for (i in seq_along(spec)) {
          variabale <- spec[[i]]

          # Check if "source_col" exists in the sublist
          if ("source_col" %in% names(variabale)) {
            # Retrieve the new name from "source_col"
            new_name <- variabale[["source_col"]]
            # Rename the variable in the appropriate dataset in the snapshot
            names(snapshot[[spec_name]])[i] <- new_name
          }
        }
      }
      snapshot
    })
  })
}
