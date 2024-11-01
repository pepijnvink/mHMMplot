out_3st_cont <- readRDS(test_path("fixtures", "mhmm_cont.rds"))
out_2st_cat <- readRDS(test_path("fixtures", "gamma_group.rds"))
gamma_group <- readRDS(test_path("fixtures", "gamma_group.rds"))
gamma_subj <- readRDS(test_path("fixtures", "gamma_subj.rds"))

test_that("mHMM Object", {
  expect_s3_class(plot_gamma(out_3st_cont,
                             level = "group"), "ggplot")
  expect_s3_class(plot_gamma(out_3st_cont,
                             level = "subject",
                             ID = 1:5), "ggplot")
  expect_s3_class(plot_gamma(out_3st_cont,
                             level = "subject"), "ggplot")
  expect_s3_class(plot_gamma(out_3st_cont,
                             level = "group",
                             digits = 3), "ggplot")
  expect_s3_class(plot_gamma(out_3st_cont,
                             level = "group",
                             facet = FALSE), "ggplot")
  expect_s3_class(plot_gamma(out_3st_cont,
                             level = "subject",
                             facet = FALSE), "ggplot")
})

test_that("mHMM_gamma object", {
  expect_s3_class(plot_gamma(gamma_group,
                             level = "group",
                             facet = FALSE), "ggplot")
  expect_s3_class(plot_gamma(gamma_group,
                             level = "subject",
                             facet = FALSE), "ggplot") %>%
    expect_warning()
  expect_s3_class(plot_gamma(gamma_group,
                             level = "group",
                             facet = TRUE), "ggplot")
  expect_s3_class(plot_gamma(gamma_group,
                             level = "subject",
                             facet = TRUE), "ggplot") %>%
    expect_warning()
  expect_s3_class(plot_gamma(gamma_subj,
                             level = "group",
                             facet = FALSE), "ggplot") %>%
    expect_warning()
  expect_s3_class(plot_gamma(gamma_subj,
                             level = "subject",
                             facet = FALSE), "ggplot")
  expect_s3_class(plot_gamma(gamma_subj,
                             level = "group",
                             facet = TRUE), "ggplot") %>%
    expect_warning()
})

test_that("Other Error messages", {
  expect_error(plot_gamma(c(1,2)))
})
