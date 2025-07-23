viterbi <- readRDS(test_path("fixtures", "viterbi.rds"))
out_2st_cat <- readRDS(test_path("fixtures", "mhmm_cat.rds"))
data(nonverbal, package = "mHMMbayes")
test_that("Plotting works", {
  expect_s3_class(plot_viterbi(viterbi), "ggplot")
  expect_s3_class(plot_viterbi(
    viterbi,
    subject = 1:5
  ), "ggplot")
  expect_s3_class(plot_viterbi(
    viterbi,
    subject = 1
  ), "ggplot")
  ## provide mHMM object
  expect_s3_class(plot_viterbi(
    states = out_2st_cat,
    s_data = nonverbal,
    subject = 1:5
  ), "ggplot")
})
test_that("Warning and errors", {
  expect_error(plot_viterbi(out_2st_cat))
})
