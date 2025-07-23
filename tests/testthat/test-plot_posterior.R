out_3st_cont <- readRDS(test_path("fixtures", "mhmm_cont.rds"))
out_2st_cat <- readRDS(test_path("fixtures", "mhmm_cat.rds"))
test_that("Gamma", {
  expect_s3_class(plot_posterior(out_2st_cat), "ggplot")
  expect_s3_class(plot_posterior(
    out_2st_cat,
    state_labels = c("a", "b")
  ), "ggplot")
  expect_s3_class(plot_posterior(
    out_2st_cat,
    state_labels = c("a", "b"),
    vrb = "p_looking"
  ), "ggplot") %>%
    expect_warning()
  expect_s3_class(plot_posterior(
    out_2st_cat,
    state_labels = c("a", "b"),
    cat_labels = c("a", "b")
  ), "ggplot") %>%
    expect_warning()
  expect_s3_class(plot_posterior(
    out_2st_cat,
    state_labels = c("a", "b", "c")
  ), "ggplot") %>%
    expect_warning()
  expect_s3_class(plot_posterior(
    out_3st_cont,
    state_labels = c("a", "b")
  ), "ggplot") %>%
    expect_warning()
  expect_s3_class(plot_posterior(
    out_3st_cont,
    state_labels = c("a", "b", "c")
  ), "ggplot")
})

test_that("Emission", {
  expect_s3_class(plot_posterior(
    out_2st_cat,
    component = "emiss",
    vrb = "p_looking",
    cat_labels = c("a", "b")
  ), "ggplot")
  expect_s3_class(plot_posterior(
    out_2st_cat,
    component = "emiss",
    vrb = "p_looking",
    cat_labels = c("a", "b", "c")
  ), "ggplot") %>%
    expect_warning()

  expect_s3_class(plot_posterior(
    out_3st_cont,
    component = "emiss",
    vrb = "observation 1",
    burnin = 1
  ), "ggplot")
  expect_s3_class(
    plot_posterior(
      out_3st_cont,
      state_labels = c("a", "b", "c", "d"),
      component = "emiss",
      vrb = "observation 1"
    ),
    "ggplot"
  ) %>%
    expect_warning()
})
test_that("Other Error messages", {
  expect_error(plot_posterior(c(1, 2)))
})
