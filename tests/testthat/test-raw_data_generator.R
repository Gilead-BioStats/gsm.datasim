test_that("generate_rawdata_for_single_study works", {
  result <- raw_data_generator(
    template_path = system.file("small_template.csv", package = "gsm.datasim"),
    mappings = c("AE")
  )

  expect_equal(length(result), 3)
  expect_equal(names(result), c("S0001", "S0004", "S0006"))
})

test_that("generate_rawdata_for_single_study works", {
  result <- raw_data_generator(
    SnapshotCount = 2,
    SnapshotWidth = "months",
    ParticipantCount = 50,
    SiteCount = 5,
    StudyID = "ABC",
    workflow_path = "workflow/1_mappings",
    mappings = "AE",
    package = "gsm.mapping"
  )

  expect_equal(length(result), 1) # 1 study
  expect_equal(names(result), c("ABC")) # 1 study with StudyID ABC
  expect_equal(length(result$ABC), 2) # 2 snapshots
})
