Raw_LB <- function(data, previous_data, spec, ...) {
  # Function body for Raw_LB
  inps <- list(...)

  curr_spec <- spec$Raw_LB


  if ("Raw_LB" %in% names(previous_data)) {
    dataset <- previous_data$Raw_LB
    previous_row_num <- length(unique(dataset$subjid))
  } else {
    dataset <- NULL
    previous_row_num <- 0
  }


  n <- inps$n - previous_row_num
  if (n == 0) return(dataset)

  tests <- data.frame(
    battrnam = c("CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL",
                 "HEMATOLOGY&DIFFERENTIAL PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL",
                 "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL",
                 "CHEMISTRY PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL",
                 "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL",
                 "HEMATOLOGY&DIFFERENTIAL PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL", "CHEMISTRY PANEL",
                 "CHEMISTRY PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL",
                 "HEMATOLOGY&DIFFERENTIAL PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL",
                 "HEMATOLOGY&DIFFERENTIAL PANEL", "CHEMISTRY PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL",
                 "HEMATOLOGY&DIFFERENTIAL PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL",
                 "HEMATOLOGY&DIFFERENTIAL PANEL", "CHEMISTRY PANEL", "HEMATOLOGY&DIFFERENTIAL PANEL",
                 "HEMATOLOGY&DIFFERENTIAL PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL",
                 "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL",
                 "CHEMISTRY PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL",
                 "HEMATOLOGY&DIFFERENTIAL PANEL", "CHEMISTRY PANEL", "CHEMISTRY PANEL"),
    lbtstnam = c("ALT (SGPT)", "AST (SGOT)", "Albumin-QT", "Alkaline Phosphatase", "Basophils",
                 "Basophils (%)", "Calcium (EDTA)", "Calcium Corrected for Albumin",
                 "Cholesterol (High Performance)", "Creatine Kinase", "Direct Bilirubin",
                 "Eosinophils", "Eosinophils (%)", "GGT", "Globulin-QT", "Glucose (2dp SI)",
                 "Hematocrit", "Hemoglobin", "Indirect Bili", "LDH", "Lymphocytes",
                 "Lymphocytes (%)", "MCH", "MCHC", "MCV", "Magnesium-PS", "Monocytes",
                 "Monocytes (%)", "Neutrophils", "Neutrophils (%)", "Phosphorus", "Platelets",
                 "RBC", "Serum Bicarbonate", "Serum Chloride", "Serum Potassium", "Serum Sodium",
                 "Serum Uric Acid", "Total Bilirubin", "Total Protein", "Triglycerides (GPO)",
                 "Urea Nitrogen", "WBC", "Creatinine(Rate Blanked)-2dp", "CHM.CCA.00.00")
  )


  if (!("battrnam" %in% names(curr_spec))) {
    curr_spec$battrnam <- list(required = TRUE)
  }

  if (!("lbtstnam" %in% names(curr_spec))) {
    curr_spec$lbtstnam <- list(required = TRUE)
  }

  if (!("visnam" %in% names(curr_spec))) {
    curr_spec$visnam <- list(required = TRUE)
  }

  if (all(c("subjid", "visnam") %in% names(curr_spec))) {
    curr_spec$subj_visit_repeated <- list(required = TRUE)
    curr_spec$subjid <- NULL
    curr_spec$visnam <- NULL
  }

  subjs <- subjid(n, external_subjid = data$Raw_SUBJ$subjid, replace = FALSE)
  subj_visits <- data$Raw_SV %>%
    dplyr::filter(subjid %in% subjs) %>%
    dplyr::select(subjid, instancename)

  all_n <- nrow(subj_visits) * nrow(tests)

  args <- list(
    subj_visit_repeated = list(nrow(tests), subj_visits),
    default = list(all_n, subj_visits, tests)
  )

  res <- add_new_var_data(dataset, curr_spec, args, spec$Raw_LB, ...)

  return(res)
}
