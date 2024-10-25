out_3st_cont <- readRDS(test_path("fixtures", "mhmm_cont.rds"))
out_2st_cat <- readRDS(test_path("fixtures", "gamma_group.rds"))
gamma_group <- readRDS(test_path("fixtures", "gamma_group.rds"))
gamma_subj <- readRDS(test_path("fixtures", "gamma_subj.rds"))

test_that("Obtain plots", {
  expect_s3_class(plot_gamma(out_3st_cont,
                             level = "group"), "ggplot")
  expect_s3_class(plot_gamma(out_3st_cont,
                             level = "subject",
                             ID = 1:5), "ggplot")
  expect_s3_class(plot_gamma(out_3st_cont,
                             level = "subject"), "ggplot")
  expect_s3_class(plot_gamma(out_3st_cont,
                             level = "group",
                             state_labels = c("a", "b", "c"),
                             digits = 3), "ggplot")
  expect_s3_class(plot_gamma(out_3st_cont,
                             level = "group",
                             state_labels = c("a", "b", "c"),
                             facet = FALSE), "ggplot")
  expect_s3_class(plot_gamma(out_3st_cont,
                             level = "subject",
                             state_labels = c("a", "b", "c"),
                             facet = FALSE), "ggplot")
  expect_s3_class(plot_gamma(gamma_group,
                             level = "group",
                             state_labels = c("a", "b", "c"),
                             facet = FALSE), "ggplot")
  expect_s3_class(plot_gamma(gamma_group,
                             level = "subject",
                             state_labels = c("a", "b", "c"),
                             facet = FALSE), "ggplot")
  expect_s3_class(plot_gamma(gamma_group,
                             level = "group",
                             state_labels = c("a", "b", "c"),
                             facet = TRUE), "ggplot")
  expect_s3_class(plot_gamma(gamma_group,
                             level = "subject",
                             state_labels = c("a", "b", "c"),
                             facet = TRUE), "ggplot")
  expect_s3_class(plot_gamma(gamma_subj,
                             level = "group",
                             state_labels = c("a", "b", "c"),
                             facet = FALSE), "ggplot")
  expect_s3_class(plot_gamma(gamma_subj,
                             level = "subject",
                             state_labels = c("a", "b", "c"),
                             facet = FALSE), "ggplot")
  expect_s3_class(plot_gamma(gamma_subj,
                             level = "group",
                             state_labels = c("a", "b", "c"),
                             facet = TRUE), "ggplot")
  expect_s3_class(plot_gamma(gamma_subj,
                             level = "subject",
                             state_labels = c("a", "b", "c"),
                             facet = TRUE), "ggplot")
})
test_that("Error messages", {
  expect_error(plot_gamma(c(1,2)))
  expect_warning(plot_gamma(gamma_group,
                             level = "subject",
                             state_labels = c("a", "b", "c"),
                             facet = TRUE))
  expect_warning(plot_gamma(gamma_subj,
                             level = "group",
                             state_labels = c("a", "b", "c"),
                             facet = FALSE))
})
