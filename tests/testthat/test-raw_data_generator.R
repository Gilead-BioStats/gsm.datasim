test_that("generate_rawdata_for_single_study works", {
  result <- raw_data_generator(
    template_path = system.file("small_template.csv", package = "gsm.datasim"),
    mappings = c("AE")
  )

  expect_equal(length(result), 3)
  expect_equal(names(result), c("S0001", "S0004", "S0006"))
})
