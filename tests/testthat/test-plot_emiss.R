out_3st_cont <- readRDS(test_path("fixtures", "mhmm_cont.rds"))
test_that("Obtain plots (continuous)", {
  expect_s3_class(plot_emiss(
    out_3st_cont,
    type = "bar",
    subject_effects = FALSE
  ), "ggplot")
  expect_s3_class(plot_emiss(
    out_3st_cont,
    type = "bar",
    subject_effects = TRUE,
    alpha = 0.5
  ), "ggplot")
  expect_s3_class(plot_emiss(
    out_3st_cont,
    type = "boxplot"
  ), "ggplot")
  expect_s3_class(plot_emiss(
    out_3st_cont,
    type = "point"
  ), "ggplot")
  expect_s3_class(plot_emiss(
    out_3st_cont,
    type = "point",
    subject_effects = TRUE,
    errorbar = "eti"
  ), "ggplot")
  expect_s3_class(plot_emiss(
    out_3st_cont,
    type = "point",
    subject_effects = TRUE,
    errorbar = "sd"
  ), "ggplot")
})
test_that("Obtain plots (categorical)", {
  expect_s3_class(plot_emiss(
    out_3st_cat,
    type = "bar",
    errorbar = "hpd"
  ))
  expect_s3_class(plot_emiss(
    out_3st_cat,
    type = "bar",
    subject_effects = TRUE,
    errorbar = "eti",
    vrb = "observation 1"
  ), "ggplot")
  expect_s3_class(plot_emiss(
    out_3st_cat,
    type = "point",
    subject_effects = TRUE,
    errorbar = "eti",
    vrb = "observation 1"
  ), "ggplot")
  expect_s3_class(plot_emiss(
    out_3st_cat,
    type = "bar",
    subject_effects = TRUE,
    errorbar = "hpd",
    vrb = "observation 1"
  ), "ggplot")

})

test_that("Other Error and warning messages", {
  expect_error(plot_emiss(c(1, 2)))
  expect_warning(plot_emiss(
    out_3st_cat,
    type = "bar",
    errorbar = "sd",
    vrb = "observation 1"
  ))
})
