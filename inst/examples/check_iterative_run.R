devtools::load_all()
 a<- raw_data_generator(ParticipantCount = 100,
                        SiteCount = 10,
                        StudyID = "Study01",
                        SnapshotCount = 5)

data <- list()
StudyID <- 'S0001'
workflow_path = "workflow/1_mappings"
wf_mapping <- gsm::MakeWorkflowList(strPath = workflow_path)
combined_specs <- gsm::CombineSpecs(wf_mapping)

spec <- combined_specs
SiteCount <- 30
ParticipantCount <- 1000
n_sites <- 20
n_subj <- 100
SnapshotCount <- 5

start_dates <- seq(as.Date("2012-01-01"), length.out = SnapshotCount, by = "months")
end_dates <- seq(as.Date("2012-02-01"), length.out = SnapshotCount, by = "months") - 1

startDate <- start_dates[1]
endDate <- end_dates[1]

data$Raw_STUDY <- Raw_STUDY(data, spec,
              StudyID = StudyID,
              SiteCount = SiteCount,
              ParticipantCount = ParticipantCount)

data$Raw_SITE  <- Raw_SITE(data, spec, n_sites = n_sites, split_vars = list("Country_State_City"))

data$Raw_SUBJ  <- Raw_SUBJ(data, spec, n_subj = n_subj, startDate = startDate,
                           endDate = endDate, split_vars = list("subject_site_synq",
                                                                "subjid_subject_nsv",
                                                                "enrolldt_timeonstudy"))




