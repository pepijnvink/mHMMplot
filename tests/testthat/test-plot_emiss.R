out_3st_cont <- readRDS(test_path("fixtures", "mhmm_cont.rds"))
test_that("Obtain plots", {
  expect_s3_class(plot_emiss(out_3st_cont,
                             type = "bar"), "ggplot")
  expect_s3_class(plot_emiss(out_3st_cont,
                             type = "bar",
                             state_labels = c("a", "b", "c"),
                             individual = TRUE), "ggplot")
  expect_s3_class(plot_emiss(out_3st_cont,
                             type = "bar",
                             state_labels = c("a", "b"),
                             individual = TRUE), "ggplot")
  expect_s3_class(plot_emiss(out_3st_cont,
                             type = "boxplot",
                             state_labels = c("a", "b")), "ggplot")
})
test_that("Error messages", {
  expect_error(plot_emiss(c(1,2)))
  expect_warning(plot_emiss(out_3st_cont,
                            state_labels = c("a", "b")))
})
