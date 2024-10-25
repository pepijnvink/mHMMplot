out_3st_cont <- readRDS(test_path("fixtures", "mhmm_cont.rds"))
out_2st_cat <- readRDS(test_path("fixtures", "mhmm_cat.rds"))
test_that("Obtain plot", {
  expect_s3_class(plot_posterior(out_2st_cat), "ggplot")
  expect_s3_class(plot_posterior(out_2st_cat,
                                 state_labels = c("a", "b")), "ggplot")
  expect_s3_class(plot_posterior(out_2st_cat,
                                 state_labels = c("a", "b", "c")), "ggplot")
  expect_s3_class(plot_posterior(out_2st_cat,
                                 component = "emiss",
                                 vrb = "p_looking",
                                 cat_labels = c("a", "b")), "ggplot")
  expect_s3_class(plot_posterior(out_2st_cat,
                                 component = "emiss",
                                 vrb = "p_looking",
                                 cat_labels = c("a", "b", "c")), "ggplot")
  expect_s3_class(plot_posterior(out_3st_cont,
                                 state_labels = c("a", "b")), "ggplot")
  expect_s3_class(plot_posterior(out_3st_cont,
                                 state_labels = c("a", "b", "c")), "ggplot")
  expect_s3_class(plot_posterior(out_3st_cont,
                                 component = "emiss",
                                 vrb = "observation 1",
                                 burnin = 1), "ggplot")

})
test_that("Error messages", {
  expect_error(plot_posterior(c(1,2)))
  expect_warning(plot_posterior(out_2st_cat,
                                component = "emiss",
                                vrb = "p_looking",
                                cat_labels = c("a", "b", "c")))
  expect_warning(plot_posterior(out_2st_cat,
                                component = "gamma",
                                cat_labels = c("a", "b", "c")))
  expect_warning(plot_posterior(out_2st_cat,
                                component = "gamma",
                                vrb = "a",
                                cat_labels = c("a", "b", "c")))
  expect_warning(plot_posterior(out_3st_cont,
                                state_labels = c("a", "b", "c")))
})
