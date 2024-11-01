out_3st_cont <- readRDS(test_path("fixtures", "mhmm_cont.rds"))
test_that("Obtain plots", {
  expect_s3_class(plot_emiss(out_3st_cont,
                             type = "bar"), "ggplot")
  expect_s3_class(plot_emiss(out_3st_cont,
                             type = "bar",
                             individual = TRUE,
                             alpha = 0.5), "ggplot")
  expect_s3_class(plot_emiss(out_3st_cont,
                             type = "boxplot"), "ggplot")
})
test_that("Other Error messages", {
  expect_error(plot_emiss(c(1,2)))
})
